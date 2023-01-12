# https://towardsdatascience.com/how-to-use-bert-from-the-hugging-face-transformer-library-d373a22b0209

import numpy as np
from transformers import AutoTokenizer, AutoModel
from numpy import dot
from numpy.linalg import norm

# Define Model & tokenizer 
model_name = "distilroberta-base"  # distilroberta
model_name = "bert-base-uncased"  # Plain Bert Model
model_name = "GanjinZero/UMLSBert_ENG"  # CODER UMLS Medical Bert Model

model = AutoModel.from_pretrained(model_name)
tokenizer = AutoTokenizer.from_pretrained(model_name)
from sklearn.metrics.pairwise import cosine_similarity

sentence_pairs = {'similar': ['the MRI of the abdomen is normal and without evidence of malignancy',
                              'no significant abnormalities involving the abdomen is observed'],
                  'dissimilar': ['mild scattered paranasal sinus mucosal thickening is observed',
                                 'deformity of the ventral thecal sac is observed']}


def get_bert_based_similarity(sentence_pairs, model, tokenizer):
    """
    computes the embeddings of each sentence and its similarity with its corresponding pair
    Args:
        sentence_pairs(dict): dictionary of lists with the similarity type as key and a list of two sentences as value
        model: the language model
        tokenizer: the tokenizer to consider for the computation

    Returns:
        similarities(dict): dictionary with similarity type as key and the similarity measure as value
    """
    similarities = dict()
    for sim_type, sent_pair in sentence_pairs.items():
        inputs_1 = tokenizer(sent_pair[0], return_tensors='pt')   # Get tokens as tensors
        inputs_2 = tokenizer(sent_pair[1], return_tensors='pt')
        # tokenized_text = tokenizer.tokenize(sent_pair[0])  # Get tokens as words
        """ Better to use average of the last_hidden_state than pooler_output """
        sent_1_embed = np.mean(model(**inputs_1).last_hidden_state[0].detach().numpy(), axis=0)
        sent_2_embed = np.mean(model(**inputs_2).last_hidden_state[0].detach().numpy(), axis=0)
        similarities[sim_type] = dot(sent_1_embed, sent_2_embed) / (norm(sent_1_embed) * norm(sent_2_embed))  # Calculate cosine similarity

        """ 1) Cosine Similarity using sklearn """
        sent_11_embed = np.mean(model(**inputs_1).last_hidden_state[0].detach().numpy(), axis=0, keepdims=True)
        sent_22_embed = np.mean(model(**inputs_2).last_hidden_state[0].detach().numpy(), axis=0, keepdims=True)
        similarities1 = cosine_similarity(sent_11_embed, sent_22_embed)  # Find Cosine Symilarity between the 2 texts
        print(sim_type, ": ", similarities[sim_type], "---", similarities1[0][0])

    return similarities


similarities = get_bert_based_similarity(sentence_pairs, model, tokenizer)


print("End")





