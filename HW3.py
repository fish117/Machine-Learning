# -*- coding: utf-8 -*-
"""
Created on Mon Mar 26 19:32:09 2018

@author: lenovo
"""

import csv
import pandas as pd
from sklearn.svm import SVC
from sklearn.model_selection import train_test_split
import numpy as np


x=pd.DataFrame(x)
# Removing column
list_drop = [0,8]
x.drop(list_drop, axis=1, inplace=True)
x.drop([0], axis=0, inplace=True)
train, validation = train_test_split(
     x, test_size=0.40, random_state=42) 
x_train= train[train.columns[1:7]] 
y_train= train[train.columns[7]] 

x_validation= validation[validation.columns[1:7]] 
y_validation= validation[validation.columns[7]] 



from sklearn import svm
from sklearn.svm import SVC
from sklearn.datasets import make_classification
from sklearn.metrics import mean_squared_error
# fit the model and get the separating hyperplane  
clf_1 = svm.SVC(kernel='linear', C=1.0,gamma=0.01)  

clf_1.fit(x_train,y_train)
clf_1.predict(x_validation)#accurate scor
clf_1.score(x_validation, y_validation)
1-clf_1.score(x_validation, y_validation)

error_linear=list()

for i in np.arange(1,20):
    clf= svm.SVC(kernel='linear', C=i,gamma=i/100)  

    model_i=clf.fit(x_train,y_train)
    error_linear_i=1-model_i.score(x_validation, y_validation)
    
    error_linear.append(error_linear_i)
    
    


#rbf
clf_1 = svm.SVC(kernel='rbf', C=1.0)  

clf_1.fit(x_train,y_train)
error_rbf=list()

for i in np.arange(1,20):
    clf= svm.SVC(kernel='rbf', C=i, gamma=i/100)  

    model_i=clf.fit(x_train,y_train)
    predict_i=model_i.predict(x_validation)
     
    error_rbf_i=1-model_i.score(x_validation, y_validation)
    error_rbf.append(error_rbf_i)
    
import matplotlib.pyplot as plt
plt.ylim(0,0.52)
a=np.arange(19)
plt.plot(a,error_linear)

plt.plot(a,error_rbf)

