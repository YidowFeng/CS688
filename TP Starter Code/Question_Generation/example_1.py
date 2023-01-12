#  https://towardsdatascience.com/questgen-an-open-source-nlp-library-for-question-generation-algorithms-1e18067fcdc6
#  Question generation using state-of-the-art Natural Language Processing techniques


from pprint import pprint
from Questgen import main
qe = main.BoolQGen()
payload = {
            "input_text": "Sachin Ramesh Tendulkar is a former international cricketer from India and a former captain of the Indian national team. "
                          "He is widely regarded as one of the greatest batsmen in the history of cricket. "
                          "He is the highest run scorer of all time in International cricket."
        }

# 1. Generate boolean (Yes/No) Questions
output = qe.predict_boolq(payload)
pprint(output)

# 2. Generate MCQ Questions
# qg = main.QGen()
# output = qg.predict_mcq(payload)
# pprint(output)

# 3. Generate FAQ Questions
qg = main.QGen()
output = qg.predict_shortq(payload)
pprint(output)


print("End")







