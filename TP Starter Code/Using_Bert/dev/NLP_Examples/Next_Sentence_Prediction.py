"""
 3) Next Sentence Prediction
 Next Sentence Prediction is the task of predicting whether one sentence follows another sentence.
“The child came home from school.” is the given sentence and we are trying to predict whether
“He played soccer after school.” is a good choice as a next sentence.

Bert returns two values in a tensor:
    The First value represents whether the second sentence is a continuation of the first,
    The Second value represents whether the second sentence is not a good continuation of the first.
"""


from transformers import BertTokenizer, BertForNextSentencePrediction
from torch.nn import functional as F
tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
model = BertForNextSentencePrediction.from_pretrained('bert-base-uncased')

# first_sentence = "The child came home from school."
# next_sentence = "He played soccer after school."
# next_sentence = "The great Wall of China can be seen from the Moon."

first_sentence = "I just arrived from Paris."
next_sentence1 = "There I saw the Eiffel Tower."
next_sentence2 = "Dogs make great pets."

print("~~~~~~~~~~~~~~~~~~~~~~")
print(first_sentence)
print("~~~~~~~~~~~~~~~~~~~~~~")
for next_sentence in [next_sentence1, next_sentence2]:
    encoding = tokenizer.encode_plus(first_sentence, next_sentence, return_tensors='pt')
    outputs = model(**encoding)[0]
    softmax = F.softmax(outputs, dim=1)

    if softmax[0][0] > softmax[0][1]:
        print(next_sentence)
        print("This sentence is a good continuation of the first")
        print(softmax)
        print("==== ==== ==== ")
    else:
        print(next_sentence)
        print("This sentence is not a good continuation of the first")
        print(softmax)
        print("==== ==== ==== ")
print("End")

