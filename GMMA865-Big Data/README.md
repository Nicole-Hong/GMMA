# Sentiment Analysis via Machine Learning (ML)-based Approach

This was the individual project for performing the sentiment analysis on text datasets, using the ML-based approach and Natural Languague Processing (NLP) techniques.

## Data Used
The following “Product Sentiment” datasets in csv files were provided for this analysis:
- sentiment_train.csv = to train the machine learning algorithms
- sentiment_test.csv = to predict the product sentiment based on the trained models

## Task Process
a.	Load, clean, and preprocess the data as you find necessary.
b.	Using the training data, extract features from the text (i.e., BOW and/or Bag of N-Grams and/or topics and/or lexical features and/or whatever you want). 
c.	Use my favorite ML algorithm to train a classification model (using hyperparameter tuning, cross validation, handling imbalanced data, etc.)
    Make reasonable decisions and try to create the best-performing classifier.
d.	Use the testing data to measure the accuracy and F1-score of models. 

## Result of the ML-based Analysis
The highest accuracy and F1 score on the test dataset was 0.73 without the feature engineering and 0.71 with the feature engineering by using the Naive Bayes classifier for multivariate Bernoulli model (i.e. BernoulliNB()) and MLP Neural Network.

Test dataset was balanced with the array of 287 of polarity = 0 and 313 of polarity = 1, so there was no need for weighting the overall dataset.

The following feature engineering was applied to the dataset:
-	Topic modeling with Non-negative Matrix Factorization (NMF)
-	TFIDF Bag of Words
-	Finding the most frequent words in both, training and test datasets (i.e. great, good, film and movie)
-	Calculating the number of exclamation marks
-	Various hyperparameter tuning on each model being used

With the feature engineering above, 71% to 73% of F1 scores mean that 3 reviews out of every 10 reviewers in reality are misinterpreted as being not helpful, when in fact, those are helpful reviews. I would consider this result as pretty satisfying, although much higher accuracy and scores are attainable. However, without the feature engineering, both, the F1 score and accuracy scores were slightly higher by 2% with 0.74.

## Five example instances in which model predictions were incorrect & Description of My Reasoning 
Five instances in which my model’s predictions were incorrect are summarized in the following table:
Instance	Machine Learning Model	F1 Score on Training Dataset	F1 Score on Test Dataset
1	SVC	0.78	0.69
2	Linear SVC	0.76	0.67
3	Gradient Boosting	0.73	0.65
4	AdaBoostClassifier	0.78	0.69
5	KNN	0.67	0.53

Overall, F1 scores on training dataset were might higher than F1 scores on test datasets for all five instances, which seems to indicate the overfitting issue.  The training data was randomly split between the train and hold out set in the proportion of 90% and 10%.  The grid search (i.e. SKlearn grid search method - GridSearchCV) was not done, so it was not possible to find out if there was any overlap between the data points used and the hold-out set. However, given only two variables with relatively small size of datasets for both, training and test datasets without any timestamps, it was assumed that there was no potential overlap.  

Based on the codes that count up the occurrences of each class in datasets and also the way that both the training and test datasets were provided for this project, I confirmed that the test data comes from the same source and the distribution is roughly the same. Also, as both the accuracy and F1 scores are about the same for all these instances, the prediction discrepancy could have been due to factors other than overfitting.  I looked into individual model in each instance as follows:

### Instance #1:
svm = SVC(gamma=2, C=1)

Support Vectors Classifier tries to find the best hyperplane to separate the different classes by maximizing the distance between sample points and the hyperplane, where gamma is a parameter for non-linear hyperplanes, and C is the penalty parameter of the error term. 

To avoid the overfitting and to control the trade-off between smooth decision boundary and classifying the training points correctly, I set gamma low at 2 and C at 1. 

After trying various hyperparameter tuning on this model, I realized that SVC produced the similar outcomes without much improvement and required higher training time for optimization. The size of datasets could have been the factor of the prediction discrepancy, but it is less efficient to run SVC with the increased data size. 

### Instance #2:
linearsvm = LinearSVC(C=0.025)

Linear Support Vectors Classifier had the same issue with SVC in the instance #1 above, and the higher training time for optimization was the issue with the same assumption regarding the size of the datasets to train the model better.

### Instance #3:
gbm = GradientBoostingClassifier(n_estimators=1500, subsample=0.67, max_features=0.06, validation_fraction=0.1, n_iter_no_change=10, verbose=2, random_state=SEED)

Gradient Boosting is a sequential technique which works on the principle of ensemble, which combines a set of weak learners and delivers improved prediction accuracy - the outcomes predicted correctly are given a lower weight and the ones miss-classified are weighted higher. 

The parameters of this model can be divided into 3 categories:
•	Tree-Specific Parameters, affecting each individual tree in the model.
•	Boosting Parameters, affecting the boosting operation in the model.
•	Other parameters for overall functioning of the model.

The prediction discrepancy was due to the overfitting based on the parameters set.  ‘n_estimators’, which is the number of sequential trees to be modeled, was set, ranging from 500 to 1,500, but for this dataset, those hyperparameter values were still too high. I also could not identify the most suitable CV for the optimized learning rate.

### Instance #4:
base_estim = DecisionTreeClassifier(max_depth=1, max_features=0.5)

ab = AdaBoostClassifier(base_estimator=base_estim, n_estimators=500, learning_rate=0.5, random_state=SEED)

where SEED = 47

AdaBoost is an ensemble learning method,  which was initially created to increase the efficiency of binary classifiers. AdaBoost uses an iterative approach to learn from the mistakes of weak classifiers, and turn them into strong ones. I tried ensembling with DecisionTree only (i.e. ‘base_estim’), and have not tried with other models.

Adaboost works on weighted samples on classified samples, decision stump for each of multiple variables, and reiteration of these steps.  

The reason for the prediction discrepancy by using this model was because the datasets were not entirely compatible with this model. Both, training and test datasets, are balanced, and thus data are not weighted, and there are no multiple variables for a decision stump to work properly.  

Also, parameter values, such as n_estimators, were set relatively higher, ranging from 500 to 2,000, which caused overfitting.  The setting the proper learning rate was also important, but with manual hyperparameter tuning, it was challenging to identify the optimized learning rate to set the suitable pace for the model.

### Instance #5:
knn = KNeighborsClassifier()

KNN is used for regression as well as for classification problems, and one of the most basic yet essential classification algorithms in Machine Learning. As a classifier, the output can be calculated as the class with the highest frequency from the K-most similar instances. Each instance in essence votes for their class and the class with the most votes is taken as the prediction.

One of the steps involved in KNN is determining the position of the unknown value K, which is the number of the nearest neighbors based on the distance of point X from all other points being calculated, and should be optimized, as 

K value was not assigned and thus the model was not at all optimized, when KNN model was run, which could have been the major cause of the prediction discrepancy for this model.

