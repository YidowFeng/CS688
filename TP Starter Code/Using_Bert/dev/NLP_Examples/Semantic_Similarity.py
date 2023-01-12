# Semantic Similarity on Scientific Text
# https://towardsdatascience.com/how-to-use-bert-from-the-hugging-face-transformer-library-d373a22b0209
# https://mccormickml.com/2020/06/22/domain-specific-bert-tutorial/#11-why-not-do-my-own-pre-training

import numpy as np
from transformers import AutoTokenizer, AutoModel
from sklearn.metrics.pairwise import cosine_similarity

# Define Model & Tokenizer
# model_name = "distilroberta-base"  # Distilroberta Model
model_name = "bert-base-uncased"  # Plain Bert Model
# model_name = "GanjinZero/UMLSBert_ENG"  # CODER UMLS Medical Bert Model

model = AutoModel.from_pretrained(model_name)
tokenizer = AutoTokenizer.from_pretrained(model_name)

query = "Mona Lisa is a painting by Leonardo DaVinci."
sent_1 = "The Last Supper is another painting by DaVinci."
sent_2 = "Acupuncture is a part of the Traditional Chinese medicine."

# query = "the MRI of the abdomen is normal and without evidence of malignancy."
# sent_1 = "no significant abnormalities involving the abdomen is observed."
# sent_2 = "deformity of the ventral thecal sac is observed."

# Tokenize Sentences
inputs_0 = tokenizer(query, return_tensors='pt')   # Get tokens as tensors
inputs_1 = tokenizer(sent_1, return_tensors='pt')   # Get tokens as tensors
inputs_2 = tokenizer(sent_2, return_tensors='pt')   # Get tokens as tensors

#  Get the Embeddings
sent_0_embed = np.mean(model(**inputs_0).last_hidden_state[0].detach().numpy(), axis=0, keepdims=True)
sent_1_embed = np.mean(model(**inputs_1).last_hidden_state[0].detach().numpy(), axis=0, keepdims=True)
sent_2_embed = np.mean(model(**inputs_2).last_hidden_state[0].detach().numpy(), axis=0, keepdims=True)

# Calculate Cosine Symilarity between the 2 texts
similarities_q1 = cosine_similarity(sent_0_embed, sent_1_embed)  # Find Cosine Symilarity between the 2 texts
similarities_q2 = cosine_similarity(sent_0_embed, sent_2_embed)  # Find Cosine Symilarity between the 2 texts
print("Query Symilarity with Sentence 1 ---", similarities_q1[0][0])
print("Query Symilarity with Sentence 2 ---", similarities_q2[0][0])
print("==============")


print("End")




