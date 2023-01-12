"""
Predict the Answer from a Text for a given Question using Bert-question-answering
"""
import torch
from transformers import AutoTokenizer, AutoModelForQuestionAnswering

''' Questions '''
question = "What does CAD refer to?"
# question = "What is the number one cause of death in the United States?"
# question = "What are the common CAD risk factors?"
# question = "What are the complications from CAD?"
# question = "What are the lifestyle modifications for the patients with CAD?"

''' Read Text from File '''
sentence_list = list()
with open('../../data/T116156.txt', 'r') as text_file:
    text = text_file.read()

chunk = 2100
text = text[0:chunk]

'''  1. Tokenize Query and Text '''
tokenizer = AutoTokenizer.from_pretrained("deepset/bert-base-cased-squad2")
model = AutoModelForQuestionAnswering.from_pretrained("deepset/bert-base-cased-squad2")
inputs = tokenizer.encode_plus(question, text, return_tensors="pt")  # Take the question & text as text and returns tokens (model-ingestible format).

''' 2. Calculate Model Score using AutoModelForQuestionAnswering class span predictor on top of the model.'''
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

print('End')









