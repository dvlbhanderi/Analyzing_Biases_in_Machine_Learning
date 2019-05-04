"""
This script uses logistic regression to provide prediction probabilities to each
individual entry

"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

import os

class Classifier:
    def __init__(self, dataset, trainpath, testpath, x_train, y_train):
        """
        Initializes the Classifier class with the following parameters

        Argument
        --------
        dataset : string
            Name of the dataset on which user wants to run Classifier
        trainpath : string
            Local path of the csv file that contains the training dataset
        testpath : string
            Local path of the csv file that contains the testing dataset

        y_train : string
            Name of target column
        """
        self.dataset = dataset
        self.trainpath = trainpath
        self.testpath = testpath

    def log_reg(self):
        """
        Uses logistic regression to get prediction scores
        """
        train = pd.read_csv(self.trainpath)
        test = pd.read_csv(self.testpath)
        train_copy = train.copy()
        test_copy = test.copy()
        test[y_train] = 0

        # combining train and test datasets
        combi = pd.concat([train, test])

        # checking the shape of the combined dataset
        print(combi.shape)

        # Imputing the missing values in train dataset
        head_train= list(train_copy)
        for i in head_train:
            train[i].fillna(train[i].mode()[0], inplace = True)

        train.isnull().any()

        # Imputing the missing values in test dataset
        head_test= list(test_copy)
        for i in head_test:
            test[i].fillna(test[i].mode()[0], inplace = True)

        test.isnull().any()

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


        # LOGISTIC REGRESSION

        from sklearn.linear_model import LogisticRegression
        import math
        model = LogisticRegression()
        model.fit(x_train, y_train)
        y_pred1 = model.predict(x_test)
        y_pred = model.predict_proba(x_test)

        print("Training Accuracy :", model.score(x_train, y_train))
        print("Testing Accuracy :", model.score(x_test, y_test))

        train = pd.read_csv(self.trainpath)
        train = train.drop(train.index[0:429])
	LS = train['Loan_Status']

        train = train.drop('Loan_Status', axis = 1)


        train['Loan_Status'] = 10*np.around(y_pred[:,1], decimals=1)
	train['Actual_Status'] = LS

        train.to_csv("new_loan.csv")
