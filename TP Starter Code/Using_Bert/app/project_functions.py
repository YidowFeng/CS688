import os
import numpy as np
import pickle
import tensorflow as tf
from tensorflow import keras
import tensorflow.keras.layers as layers
import matplotlib.pyplot as plt


# load Pickle file
def load_ds(fname, verbose=True):
    with open(fname, 'rb') as stream:
        ds, dicts = pickle.load(stream)
    if verbose:
        print('Done  loading: ', fname)
        print('      samples: {:4d}'.format(len(ds['query'])))
        print('   vocab_size: {:4d}'.format(len(dicts['token_ids'])))
        print('   slot count: {:4d}'.format(len(dicts['slot_ids'])))
        print(' intent count: {:4d}'.format(len(dicts['intent_ids'])))
    return ds, dicts


# convert Pickle file to arrays
def load_atis(filename, add_start_end_token=False, verbose=True):
    train_ds, dicts = load_ds(filename, verbose)
    t2i, s2i, in2i = map(dicts.get, ['token_ids', 'slot_ids', 'intent_ids'])
    i2t, i2s, i2in = map(lambda d: {d[k]: k for k in d.keys()}, [t2i, s2i, in2i])
    query, slots, intent = map(train_ds.get, ['query', 'slot_labels', 'intent_labels'])

    if add_start_end_token:
        i2s[178] = 'BOS'
        i2s[179] = 'EOS'
        s2i['BOS'] = 178
        s2i['EOS'] = 179

    input_tensor = []
    target_tensor = []
    query_data = []
    intent_data = []
    slot_data = []
    to_show = np.random.randint(0, len(query) - 1, 5)
    for i in range(len(query)):
        input_tensor.append(query[i])
        slot_text = []
        slot_vector = []
        for j in range(len(query[i])):
            slot_text.append(i2s[slots[i][j]])
            slot_vector.append(slots[i][j])
        if add_start_end_token:
            slot_text[0] = 'BOS'
            slot_vector[0] = 178
            slot_text[-1] = 'EOS'
            slot_vector[-1] = 179
        target_tensor.append(slot_vector)
        q = ' '.join(map(i2t.get, query[i]))
        query_data.append(q.replace('BOS', '').replace('EOS', ''))
        intent_data.append(i2in[intent[i][0]])
        slot = ' '.join(slot_text)
        slot_data.append(slot[1:-1])
        if i in to_show and verbose:
            print('Query text:', q)
            print('Query vector: ', query[i])
            print('Intent label: ', i2in[intent[i][0]])
            print('Slot text: ', slot)
            print('Slot vector: ', slot_vector)
            print('*' * 74)
    query_data = np.array(query_data)
    intent_data = np.array(intent_data)
    slot_data = np.array(slot_data)
    intent_data_label = np.array(intent).flatten()
    return t2i, s2i, in2i, i2t, i2s, i2in, input_tensor, target_tensor, query_data, intent_data, intent_data_label, slot_data


def max_length(tensor):
    return max(len(t) for t in tensor)


# Helper function to pad the query tensor and slot (target) tensor to the same length.
# Also creates a tensor for teacher forcing.
def create_tensors(input_tensor, target_tensor, nb_sample=9999999, max_len=0):
    len_input, len_target = max_length(input_tensor), max_length(target_tensor)
    len_input = max(len_input, max_len)
    len_target = max(len_target, max_len)

    # Padding the input and output tensor to the maximum length
    input_data = tf.keras.preprocessing.sequence.pad_sequences(input_tensor,
                                                               maxlen=len_input,
                                                               padding='post')

    teacher_data = tf.keras.preprocessing.sequence.pad_sequences(target_tensor,
                                                                 maxlen=len_target,
                                                                 padding='post')

    target_data = [[teacher_data[n][i + 1] for i in range(len(teacher_data[n]) - 1)] for n in range(len(teacher_data))]
    target_data = tf.keras.preprocessing.sequence.pad_sequences(target_data, maxlen=len_target, padding="post")
    target_data = target_data.reshape((target_data.shape[0], target_data.shape[1], 1))

    nb = len(input_data)
    p = np.random.permutation(nb)
    input_data = input_data[p]
    teacher_data = teacher_data[p]
    target_data = target_data[p]

    return input_data[:min(nb_sample, nb)], teacher_data[:min(nb_sample, nb)], target_data[:min(nb_sample, nb)], len_input, len_target


def get_vocab_size(t2i_train, t2i_test, s2i_train, s2i_test):
    vocab_in_size = len({**t2i_train, **t2i_test})
    vocab_out_size = len({**s2i_train, **s2i_test})
    return vocab_in_size, vocab_out_size


def build_seq2seq(input_data_train, len_input_train, vocab_in_size, vocab_out_size):
    BUFFER_SIZE = len(input_data_train)
    BATCH_SIZE = 64
    N_BATCH = BUFFER_SIZE // BATCH_SIZE
    embedding_dim = 256
    units = 1024

    # Create the Encoder layers first.
    encoder_inputs = keras.Input(shape=(len_input_train,))
    encoder_emb = layers.Embedding(input_dim=vocab_in_size, output_dim=embedding_dim)
    encoder_lstm = layers.LSTM(units=units, return_sequences=True, return_state=True)
    encoder_outputs, state_h, state_c = encoder_lstm(encoder_emb(encoder_inputs))
    encoder_states = [state_h, state_c]

    # Now create the Decoder layers.
    decoder_inputs = keras.Input(shape=(None,))
    decoder_emb = layers.Embedding(input_dim=vocab_out_size, output_dim=embedding_dim)
    decoder_lstm = layers.LSTM(units=units, return_sequences=True, return_state=True)
    decoder_lstm_out, _, _ = decoder_lstm(decoder_emb(decoder_inputs), initial_state=encoder_states)
    # Two dense layers to improve inference capabilities.
    decoder_d1 = layers.Dense(units, activation="relu")
    decoder_d2 = layers.Dense(vocab_out_size, activation="softmax")
    # Drop-out is added in the dense layers to help mitigate overfitting in this part of the model.
    decoder_out = decoder_d2(layers.Dropout(rate=.4)(decoder_d1(layers.Dropout(rate=.4)(decoder_lstm_out))))

    # Finally, create a training model which combines the encoder and the decoder.
    # Note that this model has three inputs:
    #  encoder_inputs=[batch,encoded_words] from input (query)
    #  decoder_inputs=[batch,encoded_words] from output (slots). This is the "teacher tensor".
    #  decoder_out=[batch,encoded_words] from output (slots). This is the "target tensor".
    model = keras.Model([encoder_inputs, decoder_inputs], decoder_out)
    # Use sparse_categorical_crossentropy so we don't have to expand decoder_out into a massive one-hot array.
    model.compile(optimizer=tf.optimizers.Adam(), loss="sparse_categorical_crossentropy", metrics=['sparse_categorical_accuracy'])

    model.summary()
    return model


def plot_training_accuracy(history):
    acc = history.history['sparse_categorical_accuracy']
    val_acc = history.history['val_sparse_categorical_accuracy']

    epochs = range(1, len(acc) + 1)

    plt.plot(epochs, acc, 'bo', label='Training accuracy')
    plt.plot(epochs, val_acc, 'r', label='Validation accuracy')
    plt.title('Training and validation accuracy')
    plt.xlabel('Epochs')
    plt.ylabel('Accuracy')
    plt.legend()

    plt.show()