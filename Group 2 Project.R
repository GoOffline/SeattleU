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
setwd("//Users//alexandra//Desktop//Super Senior Year//BUAN 4310 01 Data Mining and Big Data") #Set working directory
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


df1 <- credit %>% select(SK_ID_CURR,TARGET,NAME_CONTRACT_TYPE, FLAG_OWN_CAR, FLAG_OWN_REALTY, CNT_CHILDREN, PERCENT_OF_INCOME, NAME_EDUCATION_TYPE, NAME_FAMILY_STATUS, NAME_HOUSING_TYPE, DAYS_EMPLOYED, OCCUPATION_TYPE)

#Factor categorical variables for d1
df1$SK_ID_CURR <- as.factor(df1$SK_ID_CURR)
df1$NAME_CONTRACT_TYPE <- as.factor(df1$NAME_CONTRACT_TYPE)
df1$FLAG_OWN_CAR <- as.factor(df1$FLAG_OWN_CAR)
df1$FLAG_OWN_REALTY <- as.factor(df1$FLAG_OWN_REALTY)
df1$NAME_EDUCATION_TYPE <- as.factor(df1$NAME_EDUCATION_TYPE)
df1$NAME_FAMILY_STATUS <- as.factor(df1$NAME_FAMILY_STATUS)
df1$NAME_HOUSING_TYPE <- as.factor(df1$NAME_HOUSING_TYPE)
df1$OCCUPATION_TYPE <- as.factor(df1$OCCUPATION_TYPE)
str(df1)
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

#Deal with Unbalanced Data ......................................................................................................................................................................................................................................................
train_df1_balance <- ROSE(TARGET ~ NAME_CONTRACT_TYPE + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN + PERCENT_OF_INCOME + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + DAYS_EMPLOYED + OCCUPATION_TYPE , data = train_df1, seed = 666)$data


#Build a Classification Tree (DF1) ..............................................................................................................................................................................................................................................
class_tr1df1 <- rpart(TARGET ~ NAME_CONTRACT_TYPE + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN + PERCENT_OF_INCOME + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + DAYS_EMPLOYED + OCCUPATION_TYPE,
                      data = train_df1_balance, cp = .0006498616)
rpart.plot(class_tr1df1)

#printcp(class_tr1df1) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
#cptable <- as.data.frame(printcp(class_tr1df1))
#sort
#cptable[order(cptable$xerror),] #Lowest xerror .9038594. Use the corresponding cp value for the classification tree. .0006498616



# Create Data Frame 2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Determine which variables are relevant in determining if our client hasd payment difficulties or Not 
#SkU_ID_CURR we will use as our identifier variable
# TARGET is our outcome variable
# CODE_GENDER , AMT_ANNUITY, NAME_INCOME_TYPE,  DAYS_REGISTRATION, FLAG_CONT_MOBILE, CNT_FAM_MEMBERS, ORGANIZATION_TYPE

df2 <- credit %>% select(SK_ID_CURR,TARGET,CODE_GENDER, AMT_ANNUITY, NAME_INCOME_TYPE,DAYS_REGISTRATION)
#Factor categorical variables for d2
df2$SK_ID_CURR <- as.factor(df2$SK_ID_CURR)
df2$CODE_GENDER <- as.factor(df2$CODE_GENDER)
df2$NAME_INCOME_TYPE <-as.factor(df2$NAME_INCOME_TYPE)
str(df2)
library(tidyr)
df2 <- drop_na(df2) #Drop rows with missing records
nrow(df2)
#Split data into training and valuation sets (DF2).............................................................................................................................................................................................................................
set.seed(666)
train_index_df2 <- sample(1:nrow(df2), 0.6*nrow(df2))
valid_index_df2 <- setdiff(1:nrow(df2), train_index_df2)

train_df2 <- df2[train_index_df2,]
valid_df2 <- df2[valid_index_df2,]
nrow(train_df2) #Checks to see the number of rows after the split
nrow(valid_df2) #Checks to see the number of rows after the split
str(train_df2)
str(valid_df2)
#Deal with Unbalanced Data ......................................................................................................................................................................................................................................................
train_df2_balance <- ROSE(TARGET ~ CODE_GENDER + AMT_ANNUITY + NAME_INCOME_TYPE + DAYS_REGISTRATION,data = train_df2, seed = 666)$data


#Build a Classification Tree (DF2) ..............................................................................................................................................................................................................................................
class_tr1df2 <- rpart(TARGET ~ CODE_GENDER + AMT_ANNUITY + NAME_INCOME_TYPE + DAYS_REGISTRATION, 
                      data = train_df2_balance, cp = 0.0006498616)
rpart.plot(class_tr1df2)

#printcp(class_tr1df2) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
#cptable <- as.data.frame(printcp(class_tr1df1))
#sort
#cptable[order(cptable$xerror),] #Lowest xerror .9031343. Use the corresponding cp value for the classification tree. .0006498616

# Create Data Frame 3----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Determine which variables are relevant in determining if our client hasd payment difficulties or Not 
#SKU_ID_CURR, TARGET, CODE_GENDER, PERCENT_INCOME, NAME_FAMILY_STATUS
df3 <- credit %>% select(SK_ID_CURR,TARGET,CODE_GENDER, PERCENT_OF_INCOME, NAME_FAMILY_STATUS)
#Factor categorical variables for d3
df3$SK_ID_CURR <- as.factor(df3$SK_ID_CURR)
df3$CODE_GENDER <- as.factor(df3$CODE_GENDER)
df3$NAME_FAMILY_STATUS<- as.factor(df3$NAME_FAMILY_STATUS)
str(df3)
library(tidyr)
df3 <- drop_na(df3) #Drop rows with missing records
nrow(df3)
#Split data into training and valuation sets (DF3).............................................................................................................................................................................................................................
set.seed(666)
train_index_df3 <- sample(1:nrow(df3), 0.6*nrow(df3))
valid_index_df3 <- setdiff(1:nrow(df3), train_index_df3)
train_df3 <- df3[train_index_df3,]
valid_df3 <- df3[valid_index_df3,]
nrow(train_df3) #Checks to see the number of rows after the split
nrow(valid_df3) #Checks to see the number of rows after the split
str(train_df3)
str(valid_df3)

#Deal with Unbalanced Data ......................................................................................................................................................................................................................................................
train_df3_balance <- ROSE(TARGET ~ CODE_GENDER + PERCENT_OF_INCOME + NAME_FAMILY_STATUS , data = train_df3, seed = 666)$data

#Build a Classification Tree (DF3) ..............................................................................................................................................................................................................................................
class_tr1df3 <- rpart(TARGET ~ CODE_GENDER + PERCENT_OF_INCOME + NAME_FAMILY_STATUS,
                      data = train_df3_balance, cp = .005315342)
rpart.plot(class_tr1df3)

#printcp(class_tr1df3) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
#cptable <- as.data.frame(printcp(class_tr1df3))
#sort
#cptable[order(cptable$xerror),] #Lowest xerror .9836590 Use the corresponding cp value for the classification tree. .005315342

#Create Dataframe 4---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
#Split data into training and valuation sets (DF4).............................................................................................................................................................................................................................
set.seed(666)
train_index_df4 <- sample(1:nrow(df4), 0.6*nrow(df4))
valid_index_df4 <- setdiff(1:nrow(df4), train_index_df4)
train_df4 <- df4[train_index_df4,]
valid_df4 <- df4[valid_index_df4,]
nrow(train_df4) #Checks to see the number of rows after the split
nrow(valid_df4) #Checks to see the number of rows after the split
str(train_df4)
str(valid_df4)
#Deal with Unbalanced Data ......................................................................................................................................................................................................................................................
train_df4_balance <- ROSE(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + NAME_HOUSING_TYPE  + CNT_FAM_MEMBERS , data = train_df4, seed = 666)$data

#Build a Classification Tree (DF4) ..............................................................................................................................................................................................................................................
class_tr1df4 <- rpart(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + NAME_HOUSING_TYPE  + CNT_FAM_MEMBERS,
                      data = train_df4_balance, cp = 0.0008496639)
rpart.plot(class_tr1df4)

#printcp(class_tr1df4) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
#cptable <- as.data.frame(printcp(class_tr1df4))
#sort
#cptable[order(cptable$xerror),] 

#Create Dataframe 5 ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
#Split data into training and valuation sets (DF5).............................................................................................................................................................................................................................
set.seed(666)
train_index_df5 <- sample(1:nrow(df5), 0.6*nrow(df5))
valid_index_df5 <- setdiff(1:nrow(df5), train_index_df5)
train_df5 <- df5[train_index_df5,]
valid_df5 <- df5[valid_index_df5,]
nrow(train_df5) #Checks to see the number of rows after the split
nrow(valid_df5) #Checks to see the number of rows after the split
str(train_df5)
str(valid_df5)
#Deal with Unbalanced Data ......................................................................................................................................................................................................................................................
train_df5_balance <- ROSE(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + FLAG_OWN_REALTY , data = train_df5, seed = 666)$data
#Build a Classification Tree (DF5) ..............................................................................................................................................................................................................................................
class_tr1df5 <- rpart(TARGET ~ CODE_GENDER + AMT_INCOME_TOTAL + FLAG_OWN_REALTY,
                      data = train_df5_balance, cp =0.0005891104)
rpart.plot(class_tr1df5)  

#printcp(class_tr1df5) #.24
#Create CP data frame to determine the appropriate CP for our classification tree
#cptable <- as.data.frame(printcp(class_tr1df5))
#sort
#cptable[order(cptable$xerror),] 




























