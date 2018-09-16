# -*- coding: utf-8 -*-
"""
Created on Mon Mar 26 11:18:14 2018

@author: lenovo
"""

from sklearn import svm
from sklearn.svm import SVC
from sklearn.datasets import make_classification
X, y = make_classification(n_features=4, random_state=0)
my_model = SVC(C=10, kernel="rbf")#max error=10 ,rbf means radius based function
my_model.fit(X, y)
X_new, y_new= make_classification(n_features=4, random_state=0)
my_model.score(X_new, y_new)#accurate score
my_model.decision_function(X)#evaluate the distance of the plane to get the confidence score


