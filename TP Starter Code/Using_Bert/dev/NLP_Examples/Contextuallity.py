# https://mccormickml.com/2019/05/14/BERT-word-embeddings-tutorial/
"""
 5) Illustration that all of the vector representations of a same word preserve text contextually
 i.e. different instances of the word “bank” are represented differently in the following sentence:
   “After stealing money from the bank vault, the bank robber was seen fishing on the Charles river bank.”

  Contextually of the word "bank" are preserved
  i.e "bank vault" is closer to "bank robber" than to "river bank"
"""

import numpy as np
from transformers import AutoTokenizer, AutoModel
from sklearn.metrics.pairwise import cosine_similarity
import torch
# Define Model & Tokenizer
model_name = "bert-base-uncased"  # Plain Bert Model

model = AutoModel.from_pretrained(model_name)
tokenizer = AutoTokenizer.from_pretrained(model_name)

sentence = "After stealing money from the bank vault, the bank robber was seen fishing on the Charles river bank."

# Tokenize Sentence
tokenized_text = tokenizer.tokenize("[CLS] " + sentence + " [SEP]")   # Get tokens as strings
tokenized_tensor = tokenizer(sentence, return_tensors='pt')   # Get tokens as tensors

#  Get the BERT Output
output = model(**tokenized_tensor)
token_embeddings = output.last_hidden_state[0]  # Token Vectors
sentence_embeddings = np.mean(model(**tokenized_tensor).last_hidden_state[0].detach().numpy(), axis=0, keepdims=True)  # Sentence Vector

# Find the index of the three instances of the word “bank” in the example sentence
for i, token_str in enumerate(tokenized_text):
    print(i, token_str)  # Indices of the word "bank" are: 5, 9, 18


# Print the 3 vector representations for the word "bank"
print("bank vault   ", str(token_embeddings[6][0:5]))
print("bank robber  ", str(token_embeddings[10][0:5]))
print("river bank   ", str(token_embeddings[19][0:5]))

#  Calculate Cosine Symilarity between these 3 vector representations for the word "bank"
t1 = torch.reshape(token_embeddings[6], (1, token_embeddings[6].shape[0]))  # Reshape Torch Tensor to calculate Cosine Symilarity
t2 = torch.reshape(token_embeddings[10], (1, token_embeddings[10].shape[0]))  # Reshape Torch Tensor to calculate Cosine Symilarity
t3 = torch.reshape(token_embeddings[19], (1, token_embeddings[19].shape[0]))  # Reshape Torch Tensor to calculate Cosine Symilarity

similarities_1 = cosine_similarity(t1.detach().numpy(), t2.detach().numpy())  # 0.9514484 - Cosine Symilarity between "bank vault" and "bank robber"
similarities_2 = cosine_similarity(t1.detach().numpy(), t3.detach().numpy())  # 0.69878554 - Cosine Symilarity between "bank vault" and "river bank"
similarities_3 = cosine_similarity(t2.detach().numpy(), t3.detach().numpy())  # 0.6960857 - Cosine Symilarity between "bank robber" and "river bank"

# Print the Cosine Symilarity between the 3 representations of the word "bank"
print("bank vault vs bank robber  ", str(similarities_1[0][0]))  # 0.9514484
print("bank vault vs river bank   ", str(similarities_2[0][0]))  # 0.69878554
print("bank robber vs river bank  ", str(similarities_3[0][0]))  # 0.6960857


print("End")

