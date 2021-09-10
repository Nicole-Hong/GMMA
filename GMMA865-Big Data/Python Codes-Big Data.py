####   Individual Assigbment 865   ####

### Step 1. Import libraries
import datetime
print(datetime.datetime.now())

import numpy as np
import pandas as pd
import math
import random

import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns 

import os                          # for str.join in vectorization step
import re                          # Removing special characters
import string
import unicodedata
import unidecode                   # Removing special characters

from autocorrect import Speller    # for Spelling Check

import nltk
nltk.download('punkt')      # for 3-2 Tokenization
nltk.download('stopwords')  # for Removing Stop Words
from nltk.corpus import stopwords
from nltk import word_tokenize
from nltk.stem import LancasterStemmer, WordNetLemmatizer  # for Lemmatization

# for splitting training dataset into train / test datasets
from sklearn.model_selection import train_test_split  

import bs4
from bs4 import BeautifulSoup

from yellowbrick.text import FreqDistVisualizer

from os import path
from PIL import Image
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator

from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.preprocessing import FunctionTransformer
from sklearn.model_selection import GridSearchCV
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer   # for vectorization
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import NMF
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB, BernoulliNB, MultinomialNB
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.svm import LinearSVC, SVC
from sklearn.neural_network import MLPClassifier
from sklearn.experimental import enable_hist_gradient_boosting
from sklearn.ensemble import (RandomForestClassifier,
                              AdaBoostClassifier,
                              GradientBoostingClassifier,
                              HistGradientBoostingClassifier)
from xgboost import XGBClassifier
from lightgbm import LGBMClassifier
from catboost import CatBoostClassifier
import lightgbm as lgb
from sklearn.svm import SVC
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report, f1_score

SEED = 47


### Step 2. EDA/Sentiment Analysis

## Downloading the datasets
df_train = pd.read_csv('sentiment_train.csv') 

df = df_train
df.info()     

# Running the above info codes produced the following output:
# RangeIndex: 2400 entries, 0 to 2399
# Data columns (total 2 columns):
# #   Column    Non-Null Count  Dtype
# ---  ------    --------------  -----
#  0   Sentence  2400 non-null   object
#  1   Polarity  2400 non-null   int64
# dtypes: int64(1), object(1)
# memory usage: 37.6+ KB


df['Polarity'].isna().sum()

np.bincount(df['Polarity'])
# Running the above codes generated the outpu: array([1213, 1187], dtype=int64)
# which shows that the data is balanced, so no need to weighting the dataset


# Data Preprocessing for EDA Analysis
X = df['Sentence']
y = df['Polarity']

## Setting up the preprocessing function, 
# where:
# - input: single string (document)
# - output: single string (document)
stop_word = stopwords.words('english')
lemmer = WordNetLemmatizer()
def preprocess_text(doc):
    
    # Lowercase
    doc = doc.lower()
    
    # Remove Numbers
    doc = re.sub(r'\d+', '', doc)

    # Remove unidecode
    doc = unidecode.unidecode(doc)

    # Remove special characters
    doc = re.sub(r'[^\w\s]', '', doc)

    # Spelling Check
    spell = Speller(lang='en')
    doc = spell(doc)

    # Remove stopwords and lemmatize
    doc = [lemmer.lemmatize(w) for w in doc.split() if w not in stop_word]

    # Remove rare words generated from lemmatization
    doc1 = []
    common_and_rare_words = ['wa', 'ha', 'ive', 'im', 'youd', 'names']
    for wd in doc:
        if wd not in common_and_rare_words:
            doc1.append(wd)

    return ' '.join(doc) 


# Applying the preprocessing step to text data (i.e. 'Sentence')
X_preprocessed = X.apply(preprocess_text)
type(X_preprocessed)
df.insert(2, "Preprocessed", X_preprocessed, True) 
df.info()

# Tokenize and add the tokenized data to the dataframe column
X_token_lst = [word_tokenize(word) for word in X_preprocessed]
type(X_token_lst)
df.insert(3, "Tokenized", X_token_lst, True) 
df.info()
df.head()

# remove list format in the dataframe column, 'Tokenized'
vector_lst = []
for vector in X_token_lst:
    vector_lst.append(', '.join(vector))

len(vector_lst)

# replace the list generated above with data values in the dataframe column
df.insert(4, "Tokenized_Cleaned", vector_lst, True) 
df.info()
df.head()


## Creating BOW - TFIDF

tf_vectorizer = CountVectorizer(max_features=1500, min_df=0.02, max_df=0.90, ngram_range=[1,3])

dtm_tf = tf_vectorizer.fit_transform(df['Tokenized_Cleaned'])
dtm_tf_array = tf_vectorizer.fit_transform(df['Tokenized_Cleaned']).toarray()
len(dtm_tf_array)
dtm_tf_array[0]


tfidf_vectorizer = TfidfVectorizer(min_df=0.02, max_df=0.90, ngram_range=[1,3])
dtm_tfidf = tfidf_vectorizer.fit_transform(df['Tokenized_Cleaned'])

bow_df_tfidf = pd.DataFrame(dtm_tfidf.toarray(), columns=tfidf_vectorizer.get_feature_names(), index=df.index)
bow_df_tfidf.shape

df_vectorized_tfidf = pd.concat([df, bow_df_tfidf], axis=1)
df_vectorized_tfidf.shape
df_vectorized_tfidf.head()


## Showing the top tokens/grams

# Calculate column sums from DTM
sum_words = dtm_tf.sum(axis=0) 
words_freq = [(word, sum_words[0, idx]) for word, idx in tf_vectorizer.vocabulary_.items()]

# Sort the calculated column sums
words_freq =sorted(words_freq, key = lambda x: x[1], reverse=True)

# Display top 50
words_freq[:2000]
word_lst = words_freq[:50]

# Convert the word frequency list above to the dataframe
text = pd.DataFrame(list(word_lst), columns = ["Word", "Frequency"])
print(text)

# Exporting the word frequency dataframe above to csv file
text.to_csv('text2.csv', index=False)

## Visualize the word frequency in the bar chart
plt.figure(figsize=(5,8))
visualizer = FreqDistVisualizer(features=tf_vectorizer.get_feature_names(), n=25)
visualizer.fit(dtm_tf)
visualizer.poof()


## Create the Word Cloud
# Start with one review - generating one string of words based on their frequency
# per the pandas dataframe, 'text', above. Each word in this string is separated by a space
text_cloud = " "
q = 0
for wd in text['Word']:
    text_unit = (wd+" ") * text['Frequency'][q]
    text_cloud = text_cloud + text_unit
    q += 1


# Checking if 'text_ckoud' string contains the correct number of the word frequency
def count(word, array):
    n=0
    for x in array:
        if x== word:
            n+=1
    return n


# checking if text_cloud is generated correctly by counting the number of words
ar = text_cloud.split()  
print(count('also', ar))

# Word Cloud Version 1: Create and generate a word cloud image with the dark background
wordcloud1 = WordCloud(collocations=False).generate(text_cloud)

# Display the generated image
plt.imshow(wordcloud1, interpolation='bilinear')
plt.axis("off")
plt.show()

# Word Cloud Version 2: lower max_font_size, change the maximum number of word and lighten the background
wordcloud2 = WordCloud(max_font_size=50, max_words=100, background_color="white", collocations=False).generate(text_cloud)
plt.figure()
plt.imshow(wordcloud2, interpolation="bilinear")
plt.axis("off")
plt.show()

# Save the image in the img folder:
wordcloud1.to_file("C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 865 Big Data Analytics/Individual Assignment/Visuals v2/Figure.3_Word_Cloud (dark) v2.png")
wordcloud2.to_file("C:/Users/User/Desktop/Nicole Labtop Folder 2019/GMMA - Queens/GMMA 865 Big Data Analytics/Individual Assignment/Visuals v2/Figure.3_Word_Cloud (light) v2.png")


### Step 3. Developing the Machine Learning Models

## Reading datasets & Data structure
df_train = pd.read_csv('sentiment_train.csv') 

df = df_train

## Split the Training Dataset into Train / Test data
from sklearn.model_selection import train_test_split

X = df['Sentence']
y = df['Polarity']

X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.1, random_state=SEED)

type(X_train)
X_train.shape
X_train.head()

type(y_train)
y_train.shape
y_train.head()


## Repeating the same step in the Step 2. EDA/Sentiment Analysis:
## Setting up the preprocessing function 
# where:
# - input: single string (document)
# - output: single string (document)

stop_word = stopwords.words('english')
lemmer = WordNetLemmatizer()

def preprocess_text(doc):
    
    # Lowercase
    doc = doc.lower()
    
    # Remove Numbers
    doc = re.sub(r'\d+', '', doc)

    # Remove unidecode
    doc = unidecode.unidecode(doc)

    # Remove special characters
    doc = re.sub(r'[^\w\s]', '', doc)

    # Spelling Check
    spell = Speller(lang='en')
    doc = spell(doc)

    # Remove stopwords and lemmatize
    doc = [lemmer.lemmatize(w) for w in doc.split() if w not in stop_word]

    # Remove rare words generated from lemmatization
    doc1 = []
    common_and_rare_words = ['wa', 'ha', 'ive', 'im', 'youd', 'names']
    for wd in doc:
        if wd not in common_and_rare_words:
            doc1.append(wd)
    
    return ' '.join(doc1) 


## Topic Modeling
nmf = NMF(n_components=25, random_state=1, init='nndsvda', solver='mu', alpha=.1, l1_ratio=.5)

## Feature Engineering
def num_exclamation_marks(corpus):
    return np.array([doc.count('!') for doc in corpus]).reshape(-1, 1)

def has_good_great(corpus):
    return np.array([bool(re.search("good", "great")) for doc in corpus]).reshape(-1, 1)

def has_frequent_words_testdata(corpus):
    return np.array([bool(re.search("film", "movie")) for doc in corpus]).reshape(-1, 1)


## Vectorization:
# The vectorizer used to create the BOW features.
vectorizer = TfidfVectorizer(preprocessor=preprocess_text,
                             max_features=1500,
                             ngram_range=[1,3],
                             max_df=0.5, min_df=0.001, use_idf=True)

# The vectorizer2 used for the text before topic modeling.
vectorizer2 = TfidfVectorizer(preprocessor=preprocess_text,
                             max_features=1500,
                             ngram_range=[1,2],
                             max_df=0.5, min_df=0.001, use_idf=True)


## Machine Learning Algorithms
knn = KNeighborsClassifier(3)
LR = LogisticRegression()
bnb = BernoulliNB()
mnb = MultinomialNB()

svm = SVC(gamma=2, C=1)
linearsvm = LinearSVC(C=0.025)

dt = DecisionTreeClassifier(max_depth=5, random_state=SEED)
rf = RandomForestClassifier(criterion='entropy', max_depth=5, n_estimators=500, max_features=1, random_state=SEED)  #random_state=220 or 150

base_estim = DecisionTreeClassifier(max_depth=1, max_features=0.5)
ab = AdaBoostClassifier(base_estimator=base_estim, n_estimators=500, learning_rate=0.5, random_state=SEED)

xgb = XGBClassifier(n_estimators=1500, tree_method='hist', subsample=0.67, colsample_level=0.06, verbose=2, n_jobs=3, random_state=SEED)
gbm = GradientBoostingClassifier(n_estimators=1500, subsample=0.67, max_features=0.06, validation_fraction=0.1, n_iter_no_change=10, verbose=2, random_state=SEED)
lgbm = LGBMClassifier(n_estimators=1500, feature_fraction=0.07, bagging_fraction=0.67, bagging_freq=1, verbose=2, n_jobs=3, random_state=SEED)

mlp = MLPClassifier(alpha=1, random_state=150, verbose=2, max_iter=200)

# The following M/L algorithms did not work:
# - gnb = GaussianNB()
# - hgbm = HistGradientBoostingClassifier(max_iter=2000, validation_fraction=0.1, n_iter_no_change=15, verbose=0, random_state=1234)
# - cb = CatBoostClassifier(n_estimators=2000, text_features=[0], colsample_bylevel=0.06, max_leaves=31, subsample=0.67, verbose=0, thread_count=6, random_state=1234)


## Building the pipeline
feature_processing =  FeatureUnion([ 
    ('bow', Pipeline([('cv', vectorizer2), ])),
    ('topics', Pipeline([('cv', vectorizer2), ('nmf', nmf),])),    
    ('has_good_great', FunctionTransformer(has_good_great, validate=False)),  
    ('has_frequent_words_testdata', FunctionTransformer(has_frequent_words_testdata, validate=False)),  
])

steps = [('features', feature_processing)]

# for running the different machine learning codes, replace 'mlp' with other machine learing variable
# 'bnb' and 'mlp' produced the best results with the feature engineering
pipe = Pipeline([('features', feature_processing), ('clf', knn)])

## Fitting the model
pipe.fit(X_train, y_train)


## Estimate Model Performance
pred_val = pipe.predict(X_val)

print("Confusion matrix:")
print(confusion_matrix(y_val, pred_val))

print("\nF1 Score = {:.5f}".format(f1_score(y_val, pred_val, average="micro")))

print("\nClassification Report:")
print(classification_report(y_val, pred_val))


### Download the test dataset; Apply the pipeline to Test Dataset and Observe Model Performance
df_test = pd.read_csv('sentiment_test.csv') 

np.bincount(df_test['Polarity'])
# Running the above code produced the output: array([287, 313], dtype=int64)
# which shows that the test data is balanced, so no need for weighting the data

X_test = df_test['Sentence']
y_test = df_test['Polarity']

pred_test = pipe.predict(X_test)

print("Confusion matrix:")
print(confusion_matrix(y_test, pred_test))

print("\nF1 Score = {:.5f}".format(f1_score(y_test, pred_test, average="micro")))

print("\nClassification Report:")
print(classification_report(y_test, pred_test))


### Exporting the output of the predictions to a csv file
my_result = pd.DataFrame({'text': df_test.Sentence, 'given': df_test.Polarity, 'predicted': pred_test})
my_result.head()
my_result.to_csv('my_result.csv', index=False)