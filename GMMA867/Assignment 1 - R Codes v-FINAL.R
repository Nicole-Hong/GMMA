##### ## ## ## ## ### ## ### ### ### ### ## ## ## ####
### GMMA 867 - Individual Assigment #1 - R Codes   ###
##### ## ## ## ## ### ## ### ### ### ### ## ## ## ####

install.packages("eeptools")
library(eeptools)
install.packages("scales")
library(tidyverse)
library(dplyr)
library(readxl)
library(sqldf)
library(lubridate)
library(hms)
library(tidyr)
library(mice)
library(ggplot2)
library(glmnet)
library(dplyr)
library(Lahman)
library(stats)
library(Metrics)
library(e1071)

install.packages("randomForest")
library(party)
library(randomForest)

############ ########
### LOAD DATA     ###
############ ########
# load the data downloaded from Kaggle Competition:
#    train_test.csv, which combined both the train.csv and test.csv datasets
#    from Kaggle into the houseprice.data dataframe

# with the dependent variable, salesprice
houseprice.data<-read.csv(file.choose(), header=TRUE, sep=",")

# the same dataset as hourseprice but without the dependent variable, salesprice
no_salesprice.data <- houseprice.data
no_salesprice.data$SalePrice <- NULL

str(no_salesprice.data)

############ ###########
### LEARN about DATA
############ ##########

#### ## ## ## 
### NUMERICAL summaries
#### ## ## ##

# show the structure of the houseprice.data dataframe
str(houseprice.data) 
summary(houseprice.data)

head(houseprice.data, 4) # show the first 4 rows
tail(houseprice.data, 4) # show the last 4 rows

# show the structure of the no_salesprice.data dataframe
str(no_salesprice.data) 
summary(no_salesprice.data)

head(no_salesprice.data, 4) # show the first 4 rows
tail(no_salesprice.data, 4) # show the last 4 rows


### Missing Data - Info & Solving ###

## Step 1. Obtaining the info on missing data

# getting the sum of missing values in the whole data frame column wise
##  https://www.rdocumentation.org/packages/LaF/versions/0.8.4/topics/colsum
##  https://www.programmingr.com/tutorial/colsums-in-r/

colSums(is.na(houseprice.data))
colSums(is.na(no_salesprice.data))


## Step 2. Resolving the missing data issue

## for both datatframes, houseprice.data & no_salesprice.data,
##  'LotFrontage', the numerical variable has the most missing data
##   (486 'NA'). I am removing 'LotFrontage' from these dataframes.


houseprice.data2 <- houseprice.data         # Keep the original dataframe, and assign to a new dataframe
no_salesprice.data2 <- no_salesprice.data

colSums(is.na(houseprice.data2))      # checking
colSums(is.na(no_salesprice.data2))   # checking

lot <- houseprice.data2$LotFrontage
lot2 <- no_salesprice.data2$LotFrontage

#houseprice.data2[,c('lot')] <- NULL
#no_salesprice.data2[, c('lot2')] <- NULL
houseprice.data2$LotFrontage <- NULL
no_salesprice.data2$LotFrontage <- NULL

str(houseprice.data2)      # making sure that 'LotFrontage is removed
str(no_salesprice.data2)  # making sure that 'LotFrontage is removed


# used the multiple imputation in the linear regression
#    to resolve the missinf data issues


missing_no_salesprice.data <- no_salesprice.data2 
md.pattern(missing_no_salesprice.data) 


# Separate Id from the dataframe before filling in 'NA', missing data
Id <- missing_no_salesprice.data$Id
missing_no_salesprice.data[,c('Id')] <- NULL
str(missing_no_salesprice.data)

# Separate the dataset into factor variables and integer variables
fac.data <- missing_no_salesprice.data[,sapply(missing_no_salesprice.data,is.factor)]
int.data <- missing_no_salesprice.data[,sapply(missing_no_salesprice.data,is.integer)]

str(fac.data)  # check data structure and data type in fac.data
str(int.data)  # check data structure and data type in int.data

# Convert all the datatype in fac.data dateframe from factor to character
# https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
chr.data <- data.frame(lapply(fac.data, as.character), stringsAsFactors=FALSE)
str(chr.data)  # check the data types in chr.data to confirm all the datatypes are character

# confirm that the number of missing data, 'NA' in chr.data is the same as in houseprice.data
colSums(is.na(chr.data))

# Fill the missing data, 'NA' for character variables with "Not available" and turn them into factor
chr.data[is.na(chr.data)] <- "None"

str(chr.data)
colSums(is.na(chr.data))  # check that the number of 'NA' in all the variables in fac2.data is zero
                           # All the variables should have zero for 'NA'.

## Fill the missing data, 'NA', for Numeric Variables using the Random Forest

# first, combine two dataframes (fac2.data and int.data above) into one dataframe, alldata.df
alldata.df <- bind_cols(chr.data,int.data)
str(alldata.df)
colSums(is.na(alldata.df))

# used the Random Forest to fill 'NA' for numeric variables
# and also, assign the filled dataset to the dataframe, alldata.df
micemod <- alldata.df %>% mice(method='rf')
alldata.df <- complete(micemod)

str(alldata.df)                # check the data structure of alldata.df
colSums(is.na(alldata.df))     # check that there is no 'NA' in alldata.df

# to restore the variables (i.e. Id, and SalePrice) that were dropped,
#  fetch those variables and put them into the new dataframe, add.df.
str(houseprice.data)
add.df <- select(houseprice.data, Id, SalePrice)
str(add.df)   # check

# combine two dataframes (add.df and alldata.df), 
#   and assign the combined dataframes to the new dataframe, houseprice.df
houseprice.df <- cbind(add.df, alldata.df)
str(houseprice.df)            # check
colSums(is.na(houseprice.df)) # check no 'NA': all the missing data are filled


## Re-order the columns in the dataframe, houseprice.df

# Get column names
colnames(houseprice.df)

# Re-order the columns in the dataframe, houseprice.df
houseprice.df <- houseprice.df[, c(1,46,3,47,4,5,6,7,8,9,10,11,12,13,14,15,48,49,50,51,16,17,18,19,20,52,21,22,23,24,25,26,27,53,28,54,55,56,29,30,31,32,57,58,59,60,61,62,63,64,65,66,33,67,34,68,35,36,69,37,70,71,38,39,40,72,73,74,75,76,77,41,42,43,78,79,80,44,45,2)]

str(houseprice.df)         # check the structure of houseprice.df
colnames(houseprice.df)    # check the order of the columns in houseprice.df

md.pattern(houseprice.df)  # check for missing data again


### Now, missing values 'NA' in categorical variables in houseprice.df
###    are filled with "None". I did Excel Pivot on the original csv file
###    and summarized the most common values in each categorical values.
###    For the purpose of regression models, I am replacing "None" values
###    with the most common values in selective categorical variables,
###    which had missing data less than 100.

#  MSZoning: [RL]
#  Utilities: [AllPub]
#  Exterior1st: [VinylSd]
#  Exterior2nd: [VinylSd]
#  MasVnrType: [None]  <- No action taken
#  BsmtQual: [TA]
#  BsmtCond: [TA]
#  BsmtExposure: [No]
#  BsmtFinType1: [Unf]
#  KitchenQual: [TA]
#  Functional: [Typ]
#  SaleType: [WD]

houseprice.df$MSZoning[houseprice.df$MSZoning == "None"] <- "RL"
houseprice.df$Utilities[houseprice.df$Utilities == "None"] <- "AllPub"
houseprice.df$Exterior1st[houseprice.df$Exterior1st == "None"] <- "VinylSd"
houseprice.df$Exterior2nd[houseprice.df$Exterior2nd == "None"] <- "VinylSd"
houseprice.df$BsmtQual[houseprice.df$BsmtQual == "None"] <- "TA"
houseprice.df$BsmtCond[houseprice.df$BsmtCond == "None"] <- "TA"
houseprice.df$BsmtExposure[houseprice.df$BsmtExposure == "None"] <- "No"
houseprice.df$BsmtFinType1[houseprice.df$BsmtFinType1 == "None"] <- "Unf"
houseprice.df$KitchenQual[houseprice.df$KitchenQual == "None"] <- "TA"
houseprice.df$Functional[houseprice.df$Functional == "None"] <- "Typ"
houseprice.df$SaleType[houseprice.df$SaleType == "None"] <- "WD"


str(houseprice.df)
colSums(is.na(houseprice.df))

write.csv(houseprice.df, file = "train_test_full v3.2.csv", row.names=FALSE)


############ ###########
### Separate TRAIN/PREDICT dataset within the houseprice.df dataframe
############ ##########
houseprice.df.training <-subset(houseprice.df, Id<=1460) #separate ID 1...1460 into "training"
houseprice.df.prediction <-subset(houseprice.df, Id>=1461) #separate ID 1461...2919 into "prediction" 


#### ## ## ## 
### GRAPHS
#### ## ## ##

## ## ## SCATTER PLOT ### ###
## ## ##
par(mfrow=c(2,2))
plot(SalePrice ~ LotArea + OverallQual + OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + GarageYrBlt + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold, data=houseprice.df) #plot of SalePrice vs LotArea 


par(mfrow=c(1,1))
# the histrogram shows the positive skewness
hist(houseprice.df$SalePrice)

# the histrogram of Log(SalePrice) made the distribution of SalePrice look normal.
# Should do the Log Transformation (Log-Linear)
hist(log(houseprice.df$SalePrice))



############ ###########
###  MODEL #1: fit linear regression model
############ ##########
#run a multiple linear regression model (lm) on the training data, call it "fit" 

colnames(houseprice.df.training)[43] <- "X1stFlrSF"
colnames(houseprice.df.training)[44] <- "X2ndFlrSF"
colnames(houseprice.df.training)[69] <- "X3SsnPorch"

colnames(houseprice.df.prediction)[43] <- "X1stFlrSF"
colnames(houseprice.df.prediction)[44] <- "X2ndFlrSF"
colnames(houseprice.df.prediction)[69] <- "X3SsnPorch"

colnames(houseprice.df.training)
colnames(houseprice.df.prediction)


str(houseprice.df.training)
str(houseprice.df.prediction)


fit.linear.reg<-lm(SalePrice ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, data=houseprice.df.training) 

summary(fit.linear.reg) # summary of the "fit" regression model


#use the "fit,linear.reg" model to predict prices for the prediction data
houseprice.df.prediction$SalePrice <- NULL
str(houseprice.df.prediction)

predicted.houseprice <- predict(fit.linear.reg, houseprice.df.prediction)

str(predicted.houseprice)

write.csv(predicted.houseprice, file = "predicted.df.csv")
predicted_df<-read.csv(file.choose(), header=TRUE, sep=",")
colnames(predicted_df) <- c("Id", "SalePrice")
str(predicted_df)

#### #### #### ## #### ## ###
### SAVE predicted data on computer 
## ### ## ##### ## ### ### ##

write.csv(predicted_df, file = "Predicted_HousePrice.csv", row.names=FALSE)


############ ###########
###  MODEL #2: LOG-LINEAR transformation linear regression model
############ ##########

#added "log()" for SalePrice
fit.log<-lm(log(SalePrice) ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, data=houseprice.df.training) 

summary(fit.log)


#use the "fit.log" model to predict prices for the prediction data
str(houseprice.df.prediction)  # check before applying the prediction

predicted.houseprice.log <- exp(predict(fit.log, houseprice.df.prediction))

str(predicted.houseprice.log)

write.csv(predicted.houseprice.log, file = "predicted_log.df.csv")
predicted_log_df <- read.csv(file.choose(), header=TRUE, sep=",")
colnames(predicted_log_df) <- c("Id", "SalePrice")
str(predicted_log_df)

#### #### #### ## #### ## ###
### SAVE predicted data on computer 
## ### ## ##### ## ### ### ##

write.csv(predicted_log_df, file = "Predicted_HousePrice_log.csv", row.names=FALSE)


## ### ### #### ## ## ### ## ##
### ASSESSING regression model
## # # ## # ## #  ## ## ## ## ## ##
par(mfrow=c(1,1))

plot(density(resid(fit.log))) 

par(mfrow=c(1,4)) 
plot(fit.log) 


#### #### #### ### #### ##
## LASSO regression, automatic lambda choice
#### #### #### ### #### ##

lasso.df.training <- subset(houseprice.df.training, Id <= 1168)

y <-log(lasso.df.training$SalePrice) #create the y variable

# fit.log<-lm(log(SalePrice) ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, data=houseprice.df.training) 

X <- model.matrix(Id ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, houseprice.df)[,-1]  #create matrix (capital X) of x variables, it will make the code below easier to read + will ensure that all interactions exist. We use model.matrix(...)[, -1] to discard the intercept later

X <-cbind(houseprice.df$Id, X) #add houseprice.data Id vector to matrix X

#create train/test dataset
X.training<-subset(X, X[,1]<=1168)
X.testing<-subset(X, (X[,1]>=1169 & X[,1]<=1460))

X.prediction<-subset(X, X[,1]>=1461) #split X into testing, training/holdout and prediction as before

#### #### #### ### #### ##
## LASSO regression, automatic lambda choice
#### #### #### ### #### ##

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1) #alpha parameter tells glmnet() to perform a ridge (alpha = 0) or lasso (alpha = 1) model
par(mfrow=c(1,1)) # 1 graph plotting
plot(lasso.fit, xvar = "lambda")

#### #### #### ### #### ##
## Choosing best penalty option
#### #### #### ### #### ##
set.seed(2021)
crossv <- cv.glmnet(x = X.training, y = y, alpha = 1)  #create 10-fold cross-validation
crossv #displays 2 lowest lambda values with corresponding stats
plot(crossv) #plot MSE values for different values of (log)lambda 
plot(crossv, xlim=c(-8.5,-4), ylim=c(0.009,0.02)) #zoom-in the plot above to see better the optimal lambda

penalty.lasso <- crossv$lambda.min  #optimal penalty parameter, lambda
print(log(penalty.lasso)) #print optimal log(lambda) value

#### #### #### ### #### ##
## MODEL 4: re-run LASSO regression with optimal lambda
#### #### #### ### #### ##
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso)  #run the regression model with the optimal lambda, log(λ) = -8.324598

coef(lasso.opt.fit) #display model coefficients 

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing)) #running  Model #4  on the testing dataset. exp() converts variables back to the original units
str(lasso.testing)


#####################
#### #### #### ### #### ## #### #### #### ### #### ##
##  Practice exercise (Model #5) LASSO
#### #### #### ### #### ## #### #### #### ### #### ##
#####################


lasso.df.training2 <- subset(houseprice.df.training, Id <= 1460)

y <-log(lasso.df.training2$SalePrice) #create the y variable

# fit.log<-lm(log(SalePrice) ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, data=houseprice.df.training) 

X <- model.matrix(Id ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, houseprice.df)[,-1]  #create matrix (capital X) of x variables, it will make the code below easier to read + will ensure that all interactions exist. We use model.matrix(...)[, -1] to discard the intercept later

X <-cbind(houseprice.df$Id, X) #add houseprice.data Id vector to matrix X


ncol(X) #check the number of columns

#create train/test dataset
X.training <- subset(X, X[,1]<=1460)
X.testing <- subset(X, (X[,1]>=1461 & X[,1]<=2919))
X.prediction <- subset(X, X[,1]>=1461) #split X into testing, training/holdout and prediction as before

#### #### #### ### #### ##
## Choosing best penalty option
#### #### #### ### #### ##
set.seed(2021)
crossv <- cv.glmnet(x = X.training, y = y, alpha = 1)  #create 10-fold cross-validation
crossv #displays 2 lowest lambda values with corresponding stats and number of nonzero predictors
plot(crossv) #plot MSE values for different values of (log)lambda 
penalty.lasso <- crossv$lambda.min  #optimal penalty parameter, lambda
print(log(penalty.lasso)) #print optimal log(lambda) value

#### #### #### ### #### ##
## MODEL 5: LASSO regression with optimal lambda
#### #### #### ### #### ##
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso)  #run the regression model with the optimal lambda

coef(lasso.opt.fit) #display model coefficients 

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing)) #running  Model #4  on the testing dataset. exp() converts variables back to the original units

str(lasso.testing)

lasso.prediction <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction)) #running  Model #4  on the testing dataset. exp() converts variables back to the original units
str(lasso.prediction)



write.csv(lasso.prediction, file = "lasso_predicted.df.csv")
lasso_predicted_df<-read.csv(file.choose(), header=TRUE, sep=",")
colnames(lasso_predicted_df) <- c("Id", "SalePrice")
str(lasso_predicted_df)

#### #### #### ## #### ## ###
### SAVE predicted data on computer 
## ### ## ##### ## ### ### ##

write.csv(lasso_predicted_df, file = "Predicted_HousePrice_lasso.csv", row.names=FALSE)




############# ## ######### ############# ## #########
### ### ### #### ## ## ###### #  
### RIDGE REGRESSION
### ### ### #### ## ## ###### #  
 
# recreate X matrix

lasso.df.training3 <- subset(houseprice.df.training, Id <= 1460)

y2 <-log(lasso.df.training3$SalePrice) #create the y variable

# fit.log<-lm(log(SalePrice) ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, data=houseprice.df.training) 

X2 <- model.matrix(Id ~ MSSubClass + MSZoning + LotArea + Street + Alley + LotShape + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, houseprice.df)[,-1]  #create matrix (capital X) of x variables, it will make the code below easier to read + will ensure that all interactions exist. We use model.matrix(...)[, -1] to discard the intercept later

X2 <-cbind(houseprice.df$Id, X2) #add houseprice.data Id vector to matrix X


#create train/test dataset
X.training2 <- subset(X2, X2[,1]<=1460)
X.testing2 <- subset(X2, (X2[,1]>=1461 & X2[,1]<=2919))
X.prediction2 <- subset(X2, X2[,1]>=1461) #split X into testing, training/holdout and prediction as before

## ### ### #### ## ## ### ## ##
### # RIDGE (alpha=0)
## # # ## # ## #  ## ## ## ## ## ##

ridge.fit<-glmnet(x = X.training2, y = y2, alpha = 0)
plot(ridge.fit, xvar = "lambda")

## ### ### #### ## ## ### ## ##
####  create 10-fold cross validation data
## # # ## # ## #  ## ## ## ## ## ## 
crossval2 <-  cv.glmnet(x = X.training2, y = y, alpha = 0)
plot(crossval2)
crossval2

## ### ### #### ## ## ### ## ##
### display the best penalty lambda for RIDGE regression
## # # ## # ## #  ## ## ## ## ## ## 
penalty.ridge <- crossval2$lambda.min 
log(penalty.ridge) #log(lambda) value in the ridge regression
exp(log(penalty.ridge)) #lambda value in the ridge regression

## ### ### #### ## ## ### ## ##
### estimate the regression model with the optimal penalty
## # # ## # ## #  ## ## ## ## ## ## 
ridge.opt.fit <-glmnet(x = X.training2, y = y2, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

# predicting the performance on the testing set
ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing2))


#####################
#### #### #### ### #### ## #### #### #### ### #### ##
##  Practice exercise (Model #6) RIDGE
#### #### #### ### #### ## #### #### #### ### #### ##
#####################

ncol(X2) #check the number of columns

## ### ### #### ## ## ### ## ##
### # RIDGE (alpha=0)
## # # ## # ## #  ## ## ## ## ## ##
ridge.fit<-glmnet(x = X.training2, y = y2, alpha = 0)
plot(ridge.fit, xvar = "lambda")

## ### ### #### ## ## ### ## ##
####  create 10-fold cross validation data
## # # ## # ## #  ## ## ## ## ## ## 
crossval2 <-  cv.glmnet(x = X.training2, y = y2, alpha = 0)
plot(crossval2)
crossval2

## ### ### #### ## ## ### ## ##
### display the best penalty lambda for RIDGE regression
## # # ## # ## #  ## ## ## ## ## ## 
penalty.ridge <- crossval2$lambda.min 
log(penalty.ridge) #log(lambda) value in the ridge regression
exp(log(penalty.ridge)) #lambda value in the ridge regression

## ### ### #### ## ## ### ## ##
### estimate the regression model with the optimal penalty
## # # ## # ## #  ## ## ## ## ## ## 
ridge.opt.fit <-glmnet(x = X.training2, y = y2, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

# predicting the performance on the prediction set
ridge.prediction <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.prediction2))

str(ridge.prediction)


write.csv(ridge.prediction, file = "ridge_predicted.df.csv")
ridge_predicted_df<-read.csv(file.choose(), header=TRUE, sep=",")
colnames(ridge_predicted_df) <- c("Id", "SalePrice")
str(ridge_predicted_df)


#### #### #### ## #### ## ###
### SAVE predicted data on computer 
## ### ## ##### ## ### ### ##

write.csv(ridge_predicted_df, file = "Predicted_HousePrice_ridge.csv", row.names=FALSE)


#####################
#### #### #### ### #### ## #### #### #### ### #### ##
##  SVM Model
#### #### #### ### #### ## #### #### #### ### #### ##
#####################

str(houseprice.df)  # data.frame':	2919 obs. of  80 variables

# Separate the dataset into factor variables and integer variables
chr.data3 <- houseprice.df[,sapply(houseprice.df,is.character)]
int.data3 <- houseprice.df[,sapply(houseprice.df,is.integer)]
int.data3$SalePrice <- NULL
int.data3$Id <- NULL

str(chr.data3)  # check data structure and data type in fac.data
str(int.data3)  # check data structure and data type in int.data

fac.data3 <- chr.data3 %>% lapply(as.factor) %>% as.data.frame()

str(fac.data3)
combined.df <- cbind(fac.data3, int.data3)
str(combined.df)
str(add.df) 

# combine two dataframes (add.df and alldata.df), 
#   and assign the combined dataframes to the new dataframe, houseprice.df
houseprice.df3 <- cbind(add.df, combined.df)
str(houseprice.df3)            # check

# Re-order the columns in the dataframe, houseprice.df
houseprice.df3 <- houseprice.df3[, c(1,46,3,47,4,5,6,7,8,9,10,11,12,13,14,15,48,49,50,51,16,17,18,19,20,52,21,22,23,24,25,26,27,53,28,54,55,56,29,30,31,32,57,58,59,60,61,62,63,64,65,66,33,67,34,68,35,36,69,37,70,71,38,39,40,72,73,74,75,76,77,41,42,43,78,79,80,44,45,2)]

str(houseprice.df3)         # check the structure of houseprice.df
colnames(houseprice.df3)    # check the order of the columns in houseprice.df


houseprice.df.training3 <-subset(houseprice.df3, Id<=1460) #separate ID 1...1460 into "training"
houseprice.df.prediction3 <-subset(houseprice.df3, Id>=1461) #separate ID 1461...2919 into "prediction" 
houseprice.df.testing3 <-houseprice.df.prediction3


str(houseprice.df.training3)  # check
str(houseprice.df.testing3)  # check
str(houseprice.df.prediction3)  # check


train <- houseprice.df.training3
test <- houseprice.df.testing3
predict <- test
predict$SalePrice <- NULL
full <- bind_rows(train,test)

str(train)
str(test)
str(predict)
str(full)

SalePrice <- train$SalePrice

train2 <- full[1:length(SalePrice),]

test2 <- full[(length(SalePrice)+1):nrow(full),]


svm_model<-svm(SalePrice~., data=train2, cost = 3)

svm.prediction <- predict(svm_model, newdata = predict)

str(svm.prediction)


write.csv(svm.prediction, file = "svm_predicted.df.csv")
svm_predicted_df<-read.csv(file.choose(), header=TRUE, sep=",")
colnames(svm_predicted_df) <- c("Id", "SalePrice")
str(svm_predicted_df)


#### #### #### ## #### ## ###
### SAVE predicted data on computer 
## ### ## ##### ## ### ### ##

write.csv(svm_predicted_df, file = "Predicted_HousePrice_svm.csv", row.names=FALSE)


#### #### #### ## #### ## ###
### ENSEMBLE / HYBRID of Regressions above 
## ### ## ##### ## ### ### ##

# install.packages("nthroot")
# library(nthroot)

nthroot = function(x, n){x^(1/n)}


other1 <- predicted_log_df
other2 <- lasso_predicted_df

nrow(other1)
nrow(other2)
nrow(svm_predicted_df)


Id2 <- other2$Id

## Hybrid of log-linear, Lasso & SVM
mysolution <- data.frame(Id=Id2,SalePrice=nthroot(other1$SalePrice*other2$SalePrice*svm.prediction,3))

str(mysolution)

## Hybrid of Lasso & SVM
mysolution2 <- data.frame(Id=Id2,SalePrice=nthroot(other2$SalePrice*svm.prediction,2))

str(mysolution2)


#### #### #### ## #### ## ###
### SAVE predicted data on computer 
## ### ## ##### ## ### ### ##


write.csv(mysolution,"Ensemble_mysolution.csv",row.names = FALSE)

write.csv(mysolution2,"Ensemble_mysolution.v2.csv",row.names = FALSE)