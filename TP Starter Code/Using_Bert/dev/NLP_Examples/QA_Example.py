"""
 4) Extractive Question Answering based on
    https://mccormickml.com/2020/03/10/question-answering-with-a-fine-tuned-BERT/
"""
import torch
from transformers import AutoTokenizer, AutoModelForQuestionAnswering

tokenizer = AutoTokenizer.from_pretrained("deepset/bert-base-cased-squad2")
model = AutoModelForQuestionAnswering.from_pretrained("deepset/bert-base-cased-squad2")

question = "Who ruled Macedonia?"

text = """Macedonia was an ancient kingdom on the periphery of Archaic and Classical Greece, 
and later the dominant state of Hellenistic Greece. The kingdom was founded and initially ruled 
by the Argead dynasty, followed by the Antipatrid and Antigonid dynasties. Home to the ancient 
Macedonians, it originated on the northeastern part of the Greek peninsula. Before the 4th 
century BC, it was a small kingdom outside of the area dominated by the city-states of Athens, 
Sparta and Thebes, and briefly subordinate to Achaemenid Persia."""


'''  1. Tokenize Query and Text '''
tokenizer = AutoTokenizer.from_pretrained("deepset/bert-base-cased-squad2")
model = AutoModelForQuestionAnswering.from_pretrained("deepset/bert-base-cased-squad2")
inputs = tokenizer.encode_plus(question, text, return_tensors="pt")

''' 2. Calculate Model Score '''
# The AutoModelForQuestionAnswering class includes a span predictor on top of the model.
# The model returns 'answer_start' and 'end_scores' for each word in the text
answer_start_scores, answer_end_scores = model(**inputs, return_dict=False)
answer_start = torch.argmax(answer_start_scores)  # get the most likely beginning of answer with the argmax of the score
answer_end = torch.argmax(answer_end_scores) + 1  # get the most likely end of answer with the argmax of the score

''' 3. Get the Answer Text '''
# Once we have the most likely start and end tokens, grab all the tokens between them and convert tokens back to words!
the_answer = tokenizer.convert_tokens_to_string(tokenizer.convert_ids_to_tokens(inputs["input_ids"][0][answer_start:answer_end]))
print('==================')
print('Question: ' + question)
print('Answer: ' + the_answer)
print('==================')


print('END')


