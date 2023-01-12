"""
 1) Masked Language Modeling (MLM)
 MLM is the task of predicting (decoding) a masked token in a sentence.

 Produces the top 5 replacement words for the mask token
"""

from transformers import AutoModelForMaskedLM, AutoTokenizer
from torch.nn import functional as F
import torch

# Define Model & tokenizer
model_name = "distilroberta-base"  # Bert on HF
model = AutoModelForMaskedLM.from_pretrained(model_name)
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)

# Masked Text
text = "The capital of France, " + tokenizer.mask_token + ", contains the Eiffel Tower."
text = "The president of United States lives in the" + tokenizer.mask_token + " House."

# Return the top 5 replacement words for the mask token
input = tokenizer.encode_plus(text, return_tensors = "pt")
mask_index = torch.where(input["input_ids"][0] == tokenizer.mask_token_id)
output = model(**input)
logits = output.logits
softmax = F.softmax(logits, dim=-1)
mask_word = softmax[0, mask_index, :]
top_5 = torch.topk(mask_word, 5, dim=1)[1][0]
for token in top_5:
   word = tokenizer.decode([token])
   new_sentence = text.replace(tokenizer.mask_token, word)
   print(new_sentence)


print("End")




