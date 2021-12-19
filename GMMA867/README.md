# Kaggle Competition: House Prices - Advanced Regression Techniques
## Predict sales prices and practice feature engineering, RFs, and gradient boosting

This was the individual project on the Kaggle competition (Link: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview)

## Competition Description
Ask a home buyer to describe their dream house, and they probably won't begin with the height of the basement ceiling or the proximity to an east-west railroad. But this playground competition's dataset proves that much more influences price negotiations than the number of bedrooms or a white-picket fence.

With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa, this competition challenges you to predict the final price of each home.
Practice Skills

    Creative feature engineering 
    Advanced regression techniques like random forest and gradient boosting

### Acknowledgments
The Ames Housing dataset (http://jse.amstat.org/v19n3/decock.pdf) was compiled by Dean De Cock for use in data science education. It's an incredible alternative for data scientists looking for a modernized and expanded version of the often cited Boston Housing dataset. 

## Evaluation
The main goal is to predict the sales price for each house. For each Id in the test set, the value of the SalePrice variable must be predicted. 

### Metric
Submissions are evaluated on Root-Mean-Squared-Error (RMSE) between the logarithm of the predicted value and the logarithm of the observed sales price. 
(Taking logs means that errors in predicting expensive houses and cheap houses will affect the result equally.)

### Submission File Format
The file should contain a header and have the following format:

    Id,SalePrice
    1461,169000.1
    1462,187724.1233
    1463,175221
    etc.

## Data Description
### File descriptions

    train.csv - the training set
    test.csv - the test set
    data_description.txt - full description of each column, originally prepared by Dean De Cock but lightly edited to match the column names used here
    sample_submission.csv - a benchmark submission from a linear regression on year and month of sale, lot square footage, and number of bedrooms

### Data fields
Here's a brief version of the data description file:
https://github.com/Nicole-Hong/GMMA-Projects/blob/master/GMMA867/Data_Fields

## Final Report
My final report on this Kaggle Competition can be found in the following link:

https://github.com/Nicole-Hong/GMMA-Projects/blob/master/GMMA867/GMMA%20867%20Individual%20Assignment%201.pdf
