
#1. Set Up----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1.1 Libraries............................................................................................................................................................................................................................................................
library(rpart)
library(rpart.plot)
library(forecast)
library(caret)
library(janitor)
library(tidyverse)
library(tidyr)
library(ROSE)
#1.2 Load in the Data.......................................................................................................................................................................................................................................................
setwd("C:/Users/chan/Documents/R/Buan 4310 Data mining") #Set working directory

credit <- read.csv("credit_4.csv", header = TRUE)

head(credit) # Gives us the first n rows of the data set
str(credit)#To learn more about each of the variables, use the str(structure)function. The structure function puts each of the variables as a row,
#with its name, followed by its data type, and then followed by the first several values.
names(credit)
nrow(credit)


#Create the new variable combining credit amount and income
credit['PERCENT_OF_INCOME'] = credit['AMT_CREDIT'] /credit['AMT_INCOME_TOTAL'] *100


#Create Dataframe 1---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Determine which variables are relevant in determining if our client had payment difficulties (high-riisk customers) for df1
#Identifier variable: SK_ID_CURR ID of loan in our sample
#Outcome variable: TARGET. Target variable (1- client with payment difficulties: he/she had late payment, 0- otherwise)
#Relevant variables for DF1......................................
# NAME_CONTRACT_TYPE: Identification if loan is cash or revolving (options: Cash loans, Revolving loans)
# FLAG_OWN_CAR: Does client own a car? (Options: Yes, or No)
# FLAG_OWN_REALITY: Does the client own a house or flat? (Options: Yes, or No)
# CNT_CHILDREN: Number of children the client has(Options: 0-7)
# AMT_INCOME_TOTAL: Income of the client (Between $ 27,000- 18,000,090)
# AMT_CREDIT: Credit amount of the loan (**I think I should make this into a new variable that does the amount of credit as a percentage of total income, and then possibly delete AMT_INCOME_TOTAL, and AMT_CREDIT from the model)
# NAME_EDUCATION_TYPE: Level of highest education the client achieved
# NAME_FAMILY_STATUS: Single, Married, Divorced, etc. Could give us an idea as to if the client has a cosigner
# NAME_HOUSING_TYPE: Whether someone rents or not could give us evidence to whether or not they will default
# DAYS_EMPLOYED: How many days before the application the person started current employment?
# OCCUPATION_TYPE: What kind of occupation the client has


df1 <- credit %>% select(TARGET,NAME_CONTRACT_TYPE, FLAG_OWN_CAR, FLAG_OWN_REALTY, CNT_CHILDREN, PERCENT_OF_INCOME, NAME_EDUCATION_TYPE, NAME_FAMILY_STATUS, NAME_HOUSING_TYPE, DAYS_EMPLOYED, OCCUPATION_TYPE)


library(tidyr)

df1 <- drop_na(df1) #Drop rows with missing records

nrow(df1)

#Split data into training and valuation sets (DF1).............................................................................................................................................................................................................................
set.seed(666)
train_index_df1 <- sample(1:nrow(df1), 0.6*nrow(df1))
valid_index_df1 <- setdiff(1:nrow(df1), train_index_df1)

train_df1 <- df1[train_index_df1,]
valid_df1 <- df1[valid_index_df1,]
nrow(train_df1) #Checks to see the number of rows after the split
nrow(valid_df1) #Checks to see the number of rows after the split
str(train_df1)
str(valid_df1)


#Factor categorical variables for d1

train_df1$NAME_CONTRACT_TYPE <- as.factor(train_df1$NAME_CONTRACT_TYPE)
train_df1$FLAG_OWN_CAR <- as.factor(train_df1$FLAG_OWN_CAR)
train_df1$FLAG_OWN_REALTY <- as.factor(train_df1$FLAG_OWN_REALTY)
train_df1$NAME_EDUCATION_TYPE <- as.factor(train_df1$NAME_EDUCATION_TYPE)
train_df1$NAME_FAMILY_STATUS <- as.factor(train_df1$NAME_FAMILY_STATUS)
train_df1$NAME_HOUSING_TYPE <- as.factor(train_df1$NAME_HOUSING_TYPE)
train_df1$OCCUPATION_TYPE <- as.factor(train_df1$OCCUPATION_TYPE)
str(train_df1)

#Deal with Unbalanced Data ......................................................................................................................................................................................................................................................
train_df1_balance <- ROSE(TARGET ~ NAME_CONTRACT_TYPE + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN + PERCENT_OF_INCOME + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + DAYS_EMPLOYED + OCCUPATION_TYPE , data = train_df1, seed = 666)$data
valid_df1_balance <- ROSE(TARGET ~ NAME_CONTRACT_TYPE + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN + PERCENT_OF_INCOME + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + DAYS_EMPLOYED + OCCUPATION_TYPE , data = train_df1, seed = 666)$data
#Build a Classification Tree (DF1) ..............................................................................................................................................................................................................................................
class_tr1df1 <- rpart(TARGET ~ NAME_CONTRACT_TYPE + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN + PERCENT_OF_INCOME + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + DAYS_EMPLOYED + OCCUPATION_TYPE,
                      data = train_df1_balance, method = "class", minbucket = 2, cp = .0006498616)
rpart.plot(class_tr1df1)

#printcp(class_tr1df1) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
#cptable <- as.data.frame(printcp(class_tr1df1))
#sort
#cptable[order(cptable$xerror),] #Lowest xerror .9038594. Use the corresponding cp value for the classification tree. .0006498616
str(train_df1_balance)
str(class_tr1df1)

# Then we use the model to predict the outcome for training set
class_tr1df1_predict <- predict(class_tr1df1, train_df1_balance, type = "class")
t(t(head(class_tr1df1_predict,10)))
confusionMatrix(class_tr1df1_predict, train_df1_balance$TARGET)

# Then we use the model to predict the outcome for validation set. This will tell us how good (or lousy) our predictions are.
class_tr1df1_valid_predict <- predict(class_tr1df1, valid_df1, type = "class")
t(t(head(class_tr_valid_predict,10)))
confusionMatrix(class_tr1df1_valid_predict, valid_df1$TARGET)

# Compute the probabilities.
class_tr1df1_valid_predict_prob <- predict(class_tr, valid_df, type = "prob")
head(class_tr1df1_valid_predict_prob)

# The ROC curve.
library(ROSE)
## Loaded ROSE 
ROSE::roc.curve(class_tr1df1$Target, class_tr1df1_predict)





