import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns

import os
train = pd.read_csv('/home/rutu/DSP/final_project/loan-prediction/train_loan.csv')
test = pd.read_csv('/home/rutu/DSP/final_project/loan-prediction/test_loan.csv')
train_copy = train.copy()
test_copy = test.copy()
test['Loan_Status'] = 0

# combining train and test datasets
combi = pd.concat([train, test])

# checking the shape of the combined dataset
print(combi.shape)

# Imputing the missing values in train dataset

train['Gender'].fillna(train['Gender'].mode()[0], inplace = True)
train['Married'].fillna(train['Married'].mode()[0], inplace = True)
train['Dependents'].fillna(train['Dependents'].mode()[0], inplace = True)
train['Self_Employed'].fillna(train['Self_Employed'].mode()[0], inplace = True)
train['LoanAmount'].fillna(train['LoanAmount'].mode()[0], inplace = True)
train['Loan_Amount_Term'].fillna(train['Loan_Amount_Term'].mode()[0], inplace = True)
train['Credit_History'].fillna(train['Credit_History'].mode()[0], inplace = True)


train.isnull().any()

test['Gender'].fillna(train['Gender'].mode()[0], inplace = True)
test['Married'].fillna(train['Married'].mode()[0], inplace = True)
test['Dependents'].fillna(train['Dependents'].mode()[0], inplace = True)
test['Self_Employed'].fillna(train['Self_Employed'].mode()[0], inplace = True)
test['LoanAmount'].fillna(train['LoanAmount'].mode()[0], inplace = True)
test['Loan_Amount_Term'].fillna(train['Loan_Amount_Term'].mode()[0], inplace = True)
test['Credit_History'].fillna(train['Credit_History'].mode()[0], inplace = True)


train.isnull().any()

train = train.drop(columns = 'Loan_ID')
test = test.drop(columns = 'Loan_ID')

# checking the new shapes
print(train.shape)
print(test.shape)

x = train.drop('Loan_Status', axis = 1)
y = train.Loan_Status

print(x.shape)
print(y.shape)

# converting categorical variables into numerical values
# One Hot Encoding

x = pd.get_dummies(x)
train = pd.get_dummies(train)
test = pd.get_dummies(test)

# checking the new shape
print(x.shape)

# splitting x and y

from sklearn.model_selection import train_test_split

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.3, random_state = 0)

# MODELLING
# CLASSIFICATION

# LOGISTIC REGRESSION

from sklearn.linear_model import LogisticRegression
from sklearn import tree
model = LogisticRegression()
model.fit(x_train, y_train)
y_pred1 = model.predict(x_test)
y_pred = model.predict_proba(x_test)

print("Training Accuracy :", model.score(x_train, y_train))
print("Testing Accuracy :", model.score(x_test, y_test))

train = pd.read_csv('/home/rutu/DSP/final_project/loan-prediction/train_loan.csv')
train=train.drop(train.index[0:429])
train=train.drop('Loan_Status', axis = 1)

import math
print(10*np.around(y_pred[:,1], decimals=1))

train['Loan_Status'] = 10*np.around(y_pred[:,1], decimals=1)

train.to_csv("/home/rutu/DSP/final_project/loan-prediction/new.csv")
