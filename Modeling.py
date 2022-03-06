########################### ENVIRONEMENT PREPARATION ###########################

import csv,sys
from sklearn.svm import LinearSVC
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import HashingVectorizer
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.linear_model import SGDClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.feature_extraction.stop_words import ENGLISH_STOP_WORDS
from sklearn.pipeline import Pipeline
from sklearn.model_selection import cross_val_predict
from sklearn import feature_selection
import numpy as np
import pandas as pd
from nltk.stem.wordnet import WordNetLemmatizer
from nltk import wordpunct_tokenize
from nltk.corpus import wordnet as wn
from functools import lru_cache
from nltk.tag.perceptron import PerceptronTagger



########################### TRAINING ###########################

#Read the data already created with R
f = open('Trainingv5.csv.csv', 'r', encoding = 'utf8')
#Store the variables
reader = csv.reader(f, delimiter=',', quotechar='"')
#Initialize the variable , that will contain all the features
X=[]
#Y Initialize the variable that will contain the lables ( recievers mails)
y=[]
# Concatenate all the variables as one document per row
for row in reader:
  X.append(row[1]+" "+row[2]+" "+row[3]+" "+row[4]+" "+row[5]+" "+row[6]+" "+row[7]+" "+row[8]
  +" "+row[10])  #Content (senders, date, content of the email,community,topics,year,weekday)
  y.append(row[9]) #Receivers

#Delete the headers
y.pop(0)
X.pop(0)

#As I was only interested in nouns, verbs, adverbs and adjectives, 
#I created a lookup dict to quicken up the process.


# Initiate lemmatizer
wnl = WordNetLemmatizer()

# Load tagger pickle
tagger = PerceptronTagger()

# Lookup if tag is noun, verb, adverb or an adjective
tags = {'N': wn.NOUN, 'V': wn.VERB, 'R': wn.ADV, 'J': wn.ADJ}

# Memoization of POS tagging and Lemmatizer
lemmatize_mem = lru_cache(maxsize=200000)(wnl.lemmatize)
tagger_mem = lru_cache(maxsize=200000)(tagger.tag)



# Next, the tokenizer function was created. It breaks the text into words and iterate over them, 
# ignoring the stop-words and POS-tagging/Lemmatising the rest.
# This function will receive all documents from the dataset
# POS tag sentences and lemmatize each word
def tokenizer(text):
    for token in wordpunct_tokenize(text):
        if token not in ENGLISH_STOP_WORDS:
            tag = tagger_mem(frozenset({token}))
            yield lemmatize_mem(token, tags.get(tag[0][1],  wn.NOUN))
            
            
            
# Pipeline definition
clf = Pipeline([
    ('vectorizer', TfidfVectorizer(
        tokenizer=tokenizer,
        ngram_range=(1, 2),
        stop_words=ENGLISH_STOP_WORDS,
        sublinear_tf=True,
        min_df=0.00009
    )),
    ('classifier', SGDClassifier(
       loss='modified_huber',alpha=.0001, n_iter=400,penalty="elasticnet"
    )),
])
   
    
    
# fit my model
clf.fit(X, y) 

# temptation t visualize the features
clf.get_params()['vectorizer'].get_feature_names()

import matplotlib.pyplot as plt
def plot_coefficients(classifier, feature_names, top_features=20):
    coef = classifier.coef_.ravel()
    top_positive_coefficients = np.argsort(coef)[-top_features:]
    top_negative_coefficients = np.argsort(coef)[:top_features]
    top_coefficients = np.hstack([top_negative_coefficients, top_positive_coefficients])
    # create plot
    plt.figure(figsize=(15, 5))
    colors = ['red' if c < 0 else 'blue' for c in coef[top_coefficients]]
    plt.bar(np.arange(2 * top_features), coef[top_coefficients], color=colors)
    feature_names = np.array(feature_names)
    plt.xticks(np.arange(1, 1 + 2 * top_features), feature_names[top_coefficients], rotation=60, ha='right')
    plt.show()

plot_coefficients(clf.get_params()['classifier'],clf.get_params()['vectorizer'].get_feature_names())

###########################  PREDICTIONS  ###########################


# Load the test file , already created with R

f = open('Testv5.csv', 'r', encoding = 'utf8')
# Store the variables
reader = csv.reader(f, delimiter=',', quotechar='"')
#Initialize the variable , that will contain all the features
Xtest=[]

# Concatenate all the variables as one document per row
for row in reader:
    Xtest.append(append(row[1]+" "+row[2]+" "+row[3]+" "+row[4]+" "+row[8]+" "+row[11]
  +" "+row[9]+" "+row[10]+" "+row[14]) #Content
Xtest.pop(0) #Delete Haders


      
###########################  Attempt 1 : Submission file  ###########################
             
# lets make a prediction base one top 10 classes with highest proba (mail_adresses)   
                 
SGDpred=pd.DataFrame(clf.predict_proba(Xtest), columns=clf.classes_) 

def Class_top10predictions(pred):
    pred_ = []
    for i in range(len(pred)):
        tab = pred.iloc[i].sort_values(ascending = False)
        pred_.append(tab.iloc[0:10].index.tolist())
    return pred_
top10=Class_top10predictions(SGDpred)    


predictions=[] # Initalize the liste that will contains the predictions
for i in range(len(top10)):
    predictions.append(" ".join(str(x) for x in top10[i]))  
    
    
# set the prediction file by importing our test file and droping useles colomns
Test = pd.read_csv("Testv5.csv")

  
Test.drop(['Sender', 'Date' ,'Content', 'Community_grp' ,'weekday' ,'month' ,'year','week',
           'Sender_user','topic1','topic2','topic3','nb_msj_sent','wordcount',
           'weekend'], axis=1, inplace=True)
# store predictions in the file
Test["Recipients"]=predictions
Test.columns = ['Id', 'Recipients'] # set the headers name 
# write submission file
Test.to_csv("final_submission1.csv",index=False)           
                 
                 
                 
               
########################### 2nd attempt ###########################
# here we drop a reciever from our predictions if it the sender of the mail
           
                 
# Store the predicted propba
pred_proba=clf.predict_proba(Xtest) #Donne le score par rapport aux voisins

labels_receivers = np.unique(y) #set all the possible recievers
labels_receivers.sort() # sort the recivers by alphabetique orders

proba_predict = pd.DataFrame(pred_proba) #Put the obtained scores in a dataframe
proba_predict.columns = labels_receivers.tolist() #set lables names
predList=list() #Intialise the list that will contains the predicted mails adresses
for i in range(len(proba_predict)):# We take the score for the row i and put the results in decending order 
    sorted_proba = proba_predict.loc[i].T.sort_values(ascending=False) 
    credible_receiver = sorted_proba.loc[sorted_proba>0] # keep only the proba higher than one 
    credible_receiver = credible_receiver.index.tolist() #get the credible mail adress 
    predList.append(credible_receiver) # add to the predicted mail liste
    
#If more than than 11 ,set it to 11
for i in range(len(predList)):
    if len(predList[i])>11:
        predList[i] = predList[i][0:11]
		


###########################  Attempt 2 Submission file  ###########################


# Write the csv file 
f = open('Testv5.csv', 'r', encoding = 'utf8') 
reader = csv.reader(f, delimiter=',', quotechar='"')
liste=list()
cpt = 0
indice=[]
sender=[]

# set the id in the order
for row in reader:
    sender.append(row[1])
    indice.append(row[0]) 
indice.pop(0) # drop the header
sender.pop(0) #drop the header

# drop the predictions where the recievers= sender
for i in range(len(predList)):
    for email in predList[i]:
        if email == sender[i]:
            key = predList[i].index(email)
            del(predList[i][key])
            
#Création create an array that will be used later for the final creation
for i in range(len(indice)):
    row = indice[i]+","
    for elem in predList[i]:
        row = row +elem+" "
    liste.append(row)

# create final prediction file ,
f = open('final_submission2.csv', 'w', encoding = 'utf8')
f.write('Id,Recipients\n') # set the headers
for row in liste:
    f.write(str(row)+"\n") 
    cpt=cpt+1
    
    
# With this Aproach we have a dataset with incomplete row (1991)

# I complete it with the predictions of my first attemp