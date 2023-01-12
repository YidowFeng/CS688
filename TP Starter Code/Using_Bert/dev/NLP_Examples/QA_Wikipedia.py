from app import qa_functions as qaf
import wikipedia as wiki

questions = [
    'When was Barack Obama born?',
    'Why is the sky blue?',
    'How many sides does a pentagon have?'
]

reader = qaf.DocumentReader("deepset/bert-base-cased-squad2")


for question in questions:
    print(f"Question: {question}")
    results = wiki.search(question)

    page = wiki.page(results[0], auto_suggest=False)
    print(f"Top wiki result: {page}")
    print(f"\nThe {results[0]} Wikipedia article contains {len(page.content)} characters.")

    text = page.content

    reader.tokenize(question, text)
    print(f"Answer: {reader.get_answer()}")
    print()


print('End')

