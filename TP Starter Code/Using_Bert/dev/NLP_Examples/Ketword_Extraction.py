# https://pypi.org/project/keybert/
# sentence-transformers 1.0.4 has requirement torch>=1.6.0.
# flair 0.8.0.post1 has requirement torch<=1.7.1,>=1.5.0.


from keybert import KeyBERT
import app.text_samples as ts

# doc = ts.doc_ml
doc = ts.doc_cad1

model = KeyBERT('distilbert-base-nli-mean-tokens')
keywords = model.extract_keywords(doc, top_n=5)

# # You can set keyphrase_ngram_range to set the length of the resulting keywords/keyphrases:
# model.extract_keywords(doc, keyphrase_ngram_range=(1, 1), stop_words=None)
#
# # To extract keyphrases, simply set keyphrase_ngram_range to (1, 2) or higher depending on the number of words you would like in the  keyphrases:
keywords0 = model.extract_keywords(doc, keyphrase_ngram_range=(1, 2), stop_words=None, top_n=5)

# NOTE: For a full overview of all possible transformer models see sentence-transformer. I would advise either 'distilbert-base-nli-mean-tokens'
# or 'xlm-r-distilroberta-base-paraphrase-v1' as they have shown great performance in semantic similarity and paraphrase identification respectively.

# 2.3. Max Sum Similarity
# To diversity the results, we take the 2 x top_n most similar words/phrases to the document. Then, we take all top_n combinations from the 2 x top_n
# words and extract the combination that are the least similar to each other by cosine similarity.
keywords1 = model.extract_keywords(doc, keyphrase_ngram_range=(3, 3), stop_words='english', use_maxsum=True, nr_candidates=20, top_n=5)

# 2.4. Maximal Marginal Relevance
# To diversify the results, we can use Maximal Margin Relevance (MMR) to create keywords / keyphrases which is also based on cosine similarity.
# The results with high diversity:

keywords2 = model.extract_keywords(doc, keyphrase_ngram_range=(3, 3), stop_words='english', use_mmr=True, diversity=0.7, top_n=5)

# 2.5. Embedding Models
# The parameter model takes in a string pointing to a sentence-transformers model, a SentenceTransformer, or a Flair DocumentEmbedding model.
#
# Sentence-Transformers
# You can select any model from sentence-transformers here and pass it through KeyBERT with model:
model = KeyBERT(model='distilbert-base-nli-mean-tokens')
keywords3 = model.extract_keywords(doc, keyphrase_ngram_range=(3, 3), stop_words='english', use_maxsum=True, nr_candidates=20, top_n=5)

# Or select a SentenceTransformer model with your own parameters:
from sentence_transformers import SentenceTransformer

sentence_model = SentenceTransformer("distilbert-base-nli-mean-tokens", device="cpu")
model = KeyBERT(model=sentence_model)
keywords3 =  model.extract_keywords(doc, keyphrase_ngram_range=(3, 3), stop_words='english', use_maxsum=True, nr_candidates=20, top_n=5)

# Flair allows you to choose almost any embedding model that is publicly available. Flair can be used as follows:
from flair.embeddings import TransformerDocumentEmbeddings

roberta = TransformerDocumentEmbeddings('roberta-base')
model = KeyBERT(model=roberta)
keywords4 = model.extract_keywords(doc, keyphrase_ngram_range=(3, 3), stop_words='english', use_maxsum=True, nr_candidates=20, top_n=5)


# # To use Spacy's non-transformer models in KeyBERT:
# import spacy
#
# nlp = spacy.load("en_core_web_md", exclude=['tagger', 'parser', 'ner', 'attribute_ruler', 'lemmatizer'])
# model = KeyBERT(model=document_glove_embeddings_nlp)


# Using spacy-transformer models:
import spacy

spacy.prefer_gpu()
nlp = spacy.load("en_core_web_sm", exclude=['tagger', 'parser', 'ner', 'attribute_ruler', 'lemmatizer'])
model = KeyBERT(model=nlp)
keywords5 = model.extract_keywords(doc, keyphrase_ngram_range=(3, 3), stop_words='english', use_maxsum=True, nr_candidates=20, top_n=5)

print("End")

