# Stark Inc - Financial Risk Assessment 
# The following models, evaluations and predictions come from 
# Stark has decided to enter into the financial industry. 

# Below we explore different model's to find which has the best 
# accuracy to predict Stark's new customers risk level (High or not)

# The data set used to train and validate the models were provided 
# by Stark random sample of the larger dataset (about 30,000 records and 
# 66  variables and 1 target variable to train and validate). 

# The key variables in our exploration are related to the clients income, 
# ownership of real property and personal property, and varied other 
# socioeconomic factors. 


#1. Set Up----------------------------------------------------------------------
#1.1 Libraries..................................................................
library(rpart)
library(rpart.plot)
library(forecast)
library(caret)
library(janitor)
library(tidyverse)
library(tidyr)
library(ROSE)
#1.2 Load in the Data............................................................
credit <- read.csv("credit_4.csv", header = TRUE)

head(credit) # Gives us the first n rows of the data set
str(credit)
#To learn more about each of the variables, use the str(structure)function.
# The structure function puts each of the variables as a row,
#with its name, followed by its data type, and then followed by the first several values.
names(credit)
nrow(credit)


#Create the new variable combining credit amount and income
credit['PERCENT_OF_INCOME'] = credit['AMT_CREDIT'] /credit['AMT_INCOME_TOTAL'] *100


#Create Dataframe 1---------------------------------------------------------------
# Determine which variables are relevant in determining if our client had payment 
  # difficulties (high-risk customers) for df1
# Identifier variable: SK_ID_CURR ID of loan in our sample
# Outcome variable: TARGET. Target variable (1- client with payment difficulties: 
  # he/she had late payment, 0- otherwise)
# Relevant variables for DF1......................................
# NAME_CONTRACT_TYPE: Identification if loan is cash or revolving 
  # (options: Cash loans, Revolving loans)
# FLAG_OWN_CAR: Does client own a car? (Options: Yes, or No)
# FLAG_OWN_REALITY: Does the client own a house or flat? (Options: Yes, or No)
# CNT_CHILDREN: Number of children the client has(Options: 0-7)
# AMT_INCOME_TOTAL: Income of the client (Between $ 27,000- 18,000,090)
# AMT_CREDIT: Credit amount of the loan 
  # (We made this into a new variable that does the amount of 
  # credit as a percentage of total income, and then possibly delete 
  # AMT_INCOME_TOTAL, and AMT_CREDIT from the model)
# NAME_EDUCATION_TYPE: Level of highest education the client achieved
# NAME_FAMILY_STATUS: Single, Married, Divorced, etc. 
  # This gives us an idea as to if the client has a cosigner
# NAME_HOUSING_TYPE: Whether someone rents or not may give evidence 
  # to whether or not they will default
# DAYS_EMPLOYED: How many days before the application the person started current employment?
# OCCUPATION_TYPE: What kind of occupation the client has

# Reasoning: 
# These variables were chosen by looking at the tree set up from a 
# Econometrics/Gaus-Markov assumptions and perspectives. 


df1 <- credit %>% select(TARGET,NAME_CONTRACT_TYPE, FLAG_OWN_CAR, 
                         FLAG_OWN_REALTY, CNT_CHILDREN, PERCENT_OF_INCOME, 
                         NAME_EDUCATION_TYPE, NAME_FAMILY_STATUS, 
                         NAME_HOUSING_TYPE, DAYS_EMPLOYED, OCCUPATION_TYPE)


library(tidyr)

#Drop rows with missing records to be sure missing values do not skew model. 
df1 <- drop_na(df1)

nrow(df1)

#Split data into training and valuation sets (DF1).............................
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

valid_df1$NAME_CONTRACT_TYPE <- as.factor(valid_df1$NAME_CONTRACT_TYPE)
valid_df1$FLAG_OWN_CAR <- as.factor(valid_df1$FLAG_OWN_CAR)
valid_df1$FLAG_OWN_REALTY <- as.factor(valid_df1$FLAG_OWN_REALTY)
valid_df1$NAME_EDUCATION_TYPE <- as.factor(valid_df1$NAME_EDUCATION_TYPE)
valid_df1$NAME_FAMILY_STATUS <- as.factor(valid_df1$NAME_FAMILY_STATUS)
valid_df1$NAME_HOUSING_TYPE <- as.factor(valid_df1$NAME_HOUSING_TYPE)
valid_df1$OCCUPATION_TYPE <- as.factor(valid_df1$OCCUPATION_TYPE)
str(valid_df1)

#Deal with Unbalanced Data ....................................................
train_df1_balance <- ROSE(TARGET ~ NAME_CONTRACT_TYPE + FLAG_OWN_CAR + 
                            FLAG_OWN_REALTY + CNT_CHILDREN + PERCENT_OF_INCOME + 
                            NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + 
                            NAME_HOUSING_TYPE + DAYS_EMPLOYED + OCCUPATION_TYPE, 
                          data = train_df1, seed = 666)$data

#Build a Classification Tree (DF1) .............................................
class_tr1df1 <- rpart(TARGET ~ NAME_CONTRACT_TYPE + FLAG_OWN_CAR + 
                        FLAG_OWN_REALTY + CNT_CHILDREN + PERCENT_OF_INCOME + 
                        NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + 
                        NAME_HOUSING_TYPE + DAYS_EMPLOYED + OCCUPATION_TYPE,
                      data = train_df1_balance, method = "class", 
                      minbucket = 2, maxdepth = 6, cp = .0006498616)
rpart.plot(class_tr1df1)

#Reasoning: 
# We chose to limit the depth of this tree to 6, as it produced a better ROC, higher accuracy, 
# and a closer result between training and validation. 

printcp(class_tr1df1) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
cptable <- as.data.frame(printcp(class_tr1df1))
#sort
cptable[order(cptable$xerror),] #Lowest xerror .9038594. 
# Use the corresponding cp value for the classification tree. .0006498616

# To choose our cp we sorted the table by lowest x-error. Then the corresponding
# cp from that row is what is used in the model. 

str(train_df1_balance)
str(class_tr1df1)

# Then we use the model to predict the outcome for training set
class_tr1df1_predict <- predict(class_tr1df1, train_df1_balance, type = "class")
t(t(head(class_tr1df1_predict,10)))
confusionMatrix(class_tr1df1_predict, as.factor(train_df1_balance$TARGET), positive = "1")

# Then we use the model to predict the outcome for validation set. 
# This will tell us how good (or lousy) our predictions are.
class_tr1df1_valid_predict <- predict(class_tr1df1, valid_df1, type = "class")
t(t(head(class_tr1df1_valid_predict,10)))
confusionMatrix(class_tr1df1_valid_predict, as.factor(valid_df1$TARGET), positive = "1")

# Compute the probabilities.
class_tr1df1_valid_predict_prob <- predict(class_tr1df1, valid_df1, type = "prob")
head(class_tr1df1_valid_predict_prob)

# The ROC curve.
library(ROSE)
## Loaded ROSE 0.0-3
ROSE::roc.curve(valid_df1$TARGET, class_tr1df1_valid_predict)

# Create Data Frame 2--------
#Determine which variables are relevant in determining if our client has payment difficulties or Not 
#SkU_ID_CURR we will use as our identifier variable
# TARGET is our outcome variable
# CODE_GENDER , AMT_ANNUITY, NAME_INCOME_TYPE,  DAYS_REGISTRATION, 
# FLAG_CONT_MOBILE, CNT_FAM_MEMBERS, ORGANIZATION_TYPE

# For this data frame we wanted to look at some random variables to see if there
# was correlation in the model and resulting ourput. 

df2 <- credit %>% select(TARGET,CODE_GENDER, AMT_ANNUITY, NAME_INCOME_TYPE,DAYS_REGISTRATION)

library(tidyr)

#Drop rows with missing records to be sure missing values do not skew model. 
df2 <- drop_na(df2) 
nrow(df2)

#Split data into training and valuation sets (DF2) 
set.seed(1000)
train_index_df2 <- sample(1:nrow(df2), 0.6*nrow(df2))
valid_index_df2 <- setdiff(1:nrow(df2), train_index_df2)

train_df2 <- df2[train_index_df2,]
valid_df2 <- df2[valid_index_df2,]
nrow(train_df2) #Checks to see the number of rows after the split
nrow(valid_df2) #Checks to see the number of rows after the split
str(train_df2)
str(valid_df2)

#Factor categorical variables
train_df2$CODE_GENDER <- as.factor(train_df2$CODE_GENDER)
train_df2$NAME_INCOME_TYPE <-as.factor(train_df2$NAME_INCOME_TYPE)
str(train_df2)

valid_df2$CODE_GENDER <- as.factor(valid_df2$CODE_GENDER)
valid_df2$NAME_INCOME_TYPE <-as.factor(valid_df2$NAME_INCOME_TYPE)
str(valid_df2)

#Deal with Unbalanced Data .......................................................................................
train_df2_balance <- ROSE(TARGET ~ CODE_GENDER + AMT_ANNUITY + NAME_INCOME_TYPE 
                          + DAYS_REGISTRATION,data = train_df2, seed = 1000)$data

#Build a Classification Tree (DF2) ...............................................................................
class_tr1df2 <- rpart(TARGET ~ CODE_GENDER + AMT_ANNUITY + NAME_INCOME_TYPE 
                      + DAYS_REGISTRATION, 
                      data = train_df2_balance,  method = "class", 
                      minbucket = 2,maxdepth = 7, cp = 0.0006498616)
rpart.plot(class_tr1df2)

printcp(class_tr1df2) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
cptable <- as.data.frame(printcp(class_tr1df2))
#sort
cptable[order(cptable$xerror),] #Lowest xerror .9038594. 
# Use the corresponding cp value for the classification tree. .0006498616

str(train_df2_balance)
str(class_tr1df2)

# Then we use the model to predict the outcome for training set
class_tr1df2_predict <- predict(class_tr1df2, train_df2_balance, type = "class")
t(t(head(class_tr1df2_predict,10)))
confusionMatrix(class_tr1df2_predict, as.factor(train_df2_balance$TARGET), positive = "1")

# Then we use the model to predict the outcome for validation set. 
# This will tell us how good (or lousy) our predictions are.
class_tr1df2_valid_predict <- predict(class_tr1df2, valid_df2, type = "class")
t(t(head(class_tr1df2_valid_predict,10)))
confusionMatrix(class_tr1df2_valid_predict, as.factor(valid_df2$TARGET), positive = "1")

# Compute the probabilities.
class_tr1df2_valid_predict_prob <- predict(class_tr1df2, valid_df2, type = "prob")
head(class_tr1df2_valid_predict_prob)

# The ROC curve.
library(ROSE)
## Loaded ROSE 0.0-3
ROSE::roc.curve(valid_df2$TARGET, class_tr1df2_valid_predict)

# We will not use this model, as the variables are not coorelated enough with 
# our target variable/output and the accuracies we quite low. 

# Create Data Frame 3----------------------------------------------------------
#Determine which variables are relevant in determining if our client had payment difficulties or Not 
#SKU_ID_CURR, TARGET, CODE_GENDER, PERCENT_INCOME, NAME_FAMILY_STATUS
df3 <- credit %>% select(TARGET,CODE_GENDER, PERCENT_OF_INCOME, NAME_FAMILY_STATUS)

# For this data frame we chose to explore a less money based variables and more 
# people based variables. 

library(tidyr)
df3 <- drop_na(df3) #Drop rows with missing records
nrow(df3)
#Split data into training and valuation sets (DF3).............................
set.seed(1000)
train_index_df3 <- sample(1:nrow(df3), 0.6*nrow(df3))
valid_index_df3 <- setdiff(1:nrow(df3), train_index_df3)
train_df3 <- df3[train_index_df3,]
valid_df3 <- df3[valid_index_df3,]
nrow(train_df3) #Checks to see the number of rows after the split
nrow(valid_df3) #Checks to see the number of rows after the split
str(train_df3)
str(valid_df3)

#Factor categorical variables 

train_df3$CODE_GENDER <- as.factor(train_df3$CODE_GENDER)
train_df3$NAME_FAMILY_STATUS<- as.factor(train_df3$NAME_FAMILY_STATUS)
str(train_df3)

valid_df3$CODE_GENDER <- as.factor(valid_df3$CODE_GENDER)
valid_df3$NAME_FAMILY_STATUS<- as.factor(valid_df3$NAME_FAMILY_STATUS)
str(valid_df3)

#Deal with Unbalanced Data ...............................................................................................
train_df3_balance <- ROSE(TARGET ~  CODE_GENDER + PERCENT_OF_INCOME + 
                            NAME_FAMILY_STATUS , data = train_df3, seed = 1000)$data
#Lowest xerror .9836590 Use the corresponding cp value for the classification tree. .005315342

#Build a Classification Tree (DF3) .......................................................................................
class_tr1df3 <- rpart(TARGET ~ CODE_GENDER + PERCENT_OF_INCOME + NAME_FAMILY_STATUS,
                      data = train_df3_balance, method = "class", 
                      minbucket = 2,maxdepth = 7, cp = .005315342)
rpart.plot(class_tr1df3)

printcp(class_tr1df3) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
cptable <- as.data.frame(printcp(class_tr1df3))
#sort
cptable[order(cptable$xerror),] 

str(train_df3_balance)
str(class_tr1df3)

# Then we use the model to predict the outcome for training set
class_tr1df3_predict <- predict(class_tr1df3, train_df3_balance, type = "class")
t(t(head(class_tr1df3_predict,10)))
confusionMatrix(class_tr1df3_predict, as.factor(train_df3_balance$TARGET), positive = "1")

# Then we use the model to predict the outcome for validation set. 
# This will tell us how good (or lousy) our predictions are.
class_tr1df3_valid_predict <- predict(class_tr1df3, valid_df3, type = "class")
t(t(head(class_tr1df3_valid_predict,10)))
confusionMatrix(class_tr1df3_valid_predict, as.factor(valid_df3$TARGET), positive = "1")

# Compute the probabilities.
class_tr1df3_valid_predict_prob <- predict(class_tr1df3, valid_df3, type = "prob")
head(class_tr1df3_valid_predict_prob)

# The ROC curve.
library(ROSE)
## Loaded ROSE 0.0-3
ROSE::roc.curve(valid_df3$TARGET, class_tr1df3_valid_predict)

#Create Dataframe 4-------------------------------------------------------------------------------------------
#Determine which variables are relevant in determining if our client hasd payment difficulties or Not 
#SKU_ID_CURR, TARGET, CODE_GENDER, AMT_INCOME, NAME_HOUSING_TYPE, NAME_EDUCATION, CNT_FAM_MEMBERS
df4 <- credit %>% select(SK_ID_CURR,TARGET,CODE_GENDER, AMT_INCOME_TOTAL,NAME_HOUSING_TYPE, CNT_FAM_MEMBERS)
str(df4)
#Factor categorical variables for d4
df4$SK_ID_CURR <- as.factor(df4$SK_ID_CURR)
df4$CODE_GENDER <- as.factor(df4$CODE_GENDER)
df4$NAME_HOUSING_TYPE<- as.factor(df4$NAME_HOUSING_TYPE)

str(df4)
library(tidyr)
df4 <- drop_na(df4) #Drop rows with missing records
nrow(df4)
#Split data into training and valuation sets (DF4).......................................................
set.seed(1000)
train_index_df4 <- sample(1:nrow(df4), 0.6*nrow(df4))
valid_index_df4 <- setdiff(1:nrow(df4), train_index_df4)
train_df4 <- df4[train_index_df4,]
valid_df4 <- df4[valid_index_df4,]
nrow(train_df4) #Checks to see the number of rows after the split
nrow(valid_df4) #Checks to see the number of rows after the split
str(train_df4)
str(valid_df4)
#Deal with Unbalanced Data ..............................................................................
train_df4_balance <- ROSE(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + 
                            NAME_HOUSING_TYPE  + CNT_FAM_MEMBERS , 
                          data = train_df4, seed = 1000)$data

#Build a Classification Tree (DF4) ......................................................................
class_tr1df4 <- rpart(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + 
                        NAME_HOUSING_TYPE  + CNT_FAM_MEMBERS, 
                      data = train_df4_balance,method = "class", 
                      minbucket = 2, maxdepth = 7, cp = 0.0006498616)
rpart.plot(class_tr1df4)

printcp(class_tr1df4) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
cptable <- as.data.frame(printcp(class_tr1df4))
#sort
#Lowest xerror .9038594. Use the corresponding cp value for the classification tree. .0006498616
cptable[order(cptable$xerror),] 

# Then we use the model to predict the outcome for training set
class_tr1df4_predict <- predict(class_tr1df4, train_df4_balance, type = "class")
t(t(head(class_tr1df4_predict,10)))
confusionMatrix(class_tr1df4_predict, as.factor(train_df4_balance$TARGET), positive = "1")

# Then we use the model to predict the outcome for validation set. 
# This will tell us how good (or lousy) our predictions are.
class_tr1df4_valid_predict <- predict(class_tr1df4, valid_df4, type = "class")
t(t(head(class_tr1df4_valid_predict,10)))
confusionMatrix(class_tr1df4_valid_predict, as.factor(valid_df4$TARGET), positive = "1")

# Compute the probabilities.
class_tr1df4_valid_predict_prob <- predict(class_tr1df4, valid_df4, type = "prob")
head(class_tr1df4_valid_predict_prob)

# The ROC curve.
library(ROSE)
## Loaded ROSE 0.0-3
ROSE::roc.curve(valid_df4$TARGET, class_tr1df4_valid_predict)

#Create Dataframe 5 -------------------------------------------------------------
#Determine which variables are relevant in determining if our client hasd payment difficulties or Not 
#SKU_ID_CURR, TARGET, CODE_GENDER, AMT_INCOME, NAME_HOUSING_TYPE, NAME_EDUCATION, CNT_FAM_MEMBERS
df5 <- credit %>% select(SK_ID_CURR,TARGET,CODE_GENDER, AMT_INCOME_TOTAL,FLAG_OWN_REALTY)
#Factor categorical variables for d5
df5$SK_ID_CURR <- as.factor(df5$SK_ID_CURR)
df5$CODE_GENDER <- as.factor(df5$CODE_GENDER)
df5$FLAG_OWN_REALTY <- as.factor(df5$FLAG_OWN_REALTY)
str(df5)
library(tidyr)
df5 <- drop_na(df5) #Drop rows with missing records
nrow(df5)
#Split data into training and valuation sets (DF5).....................................
set.seed(1000)
train_index_df5 <- sample(1:nrow(df5), 0.6*nrow(df5))
valid_index_df5 <- setdiff(1:nrow(df5), train_index_df5)
train_df5 <- df5[train_index_df5,]
valid_df5 <- df5[valid_index_df5,]
nrow(train_df5) #Checks to see the number of rows after the split
nrow(valid_df5) #Checks to see the number of rows after the split
str(train_df5)
str(valid_df5)
#Deal with Unbalanced Data .....................................................
train_df5_balance <- ROSE(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + 
                            FLAG_OWN_REALTY , data = train_df5, seed = 1000)$data

#Build a Classification Tree (DF5) .............................................
class_tr1df5 <- rpart(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + FLAG_OWN_REALTY,
                      data = train_df5_balance, method = "class", 
                      minbucket = 2, maxdepth = 7, cp =0.0005891104)
rpart.plot(class_tr1df5)  

printcp(class_tr1df5) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
cptable <- as.data.frame(printcp(class_tr1df5))
#sort
cptable[order(cptable$xerror),] 

# Then we use the model to predict the outcome for training set
class_tr1df5_predict <- predict(class_tr1df5, train_df5_balance, type = "class")
t(t(head(class_tr1df5_predict,10)))
confusionMatrix(class_tr1df5_predict, as.factor(train_df5$TARGET), positive = "1")

# Then we use the model to predict the outcome for validation set. 
# This will tell us how good (or lousy) our predictions are.
class_tr1df5_valid_predict <- predict(class_tr1df5, valid_df5, type = "class")
t(t(head(class_tr1df5_valid_predict,10)))
confusionMatrix(class_tr1df5_valid_predict, as.factor(valid_df5$TARGET), positive = "1")

# Compute the probabilities.
class_tr1df5_valid_predict_prob <- predict(class_tr1df5, valid_df5, type = "prob")
head(class_tr1df5_valid_predict_prob)

# The ROC curve.
library(ROSE)
## Loaded ROSE 0.0-3
ROSE::roc.curve(valid_df5$TARGET, class_tr1df5_valid_predict)


### 
# Decision Tree Chosen: DataFrame1
# We will use the df1 decision tree to calculate predictions for the new customers.

#Import new customer data set 

creditnew <- read.csv("credit_test_4.csv", header = TRUE)

#We need to add our "Percent_of_Income variable to the new dataset
creditnew['PERCENT_OF_INCOME'] = creditnew['AMT_CREDIT'] /creditnew['AMT_INCOME_TOTAL'] *100


#Predict new customers using DataFrame1
credit_predict <- predict(class_tr1df1, newdata = creditnew)
credit_predict

# Results ----- 

# By choosing a model with greater variables but limiting the depth we were able 
#to produce a tree with a somewhat high accuracy between the training and 
#validation sets, as well as a somewhat strong, though not great ROC curve. 

# ROC curves for all models were very close to each other, the chosen model 
# has a higher accuracy and thus why it was chosen. 

# The given data, and new data can be used by Stark Inc to be used in the 
# future, specifically to understand new customers and their risk levels. 

# According to this model, customers one and three are likely to have high risk 
# and customers 2, 4, and 5 are not likely. 

# The data suggests that there is a clear line between high risk and not high 
# risk customers. 
