import requests 
from StringIO import StringIO
import numpy as np
import pandas as pd # pandas
import matplotlib.pyplot as plt # module for plotting 
import datetime as dt # module for manipulating dates and times
import numpy.linalg as lin # module for performing linear algebra operations
from __future__ import division
import matplotlib

import sklearn.decomposition
import sklearn.metrics
from sklearn import gaussian_process
from sklearn import cross_validation

pd.options.display.mpl_style = 'default'

df = pd.read_csv("13.reduced.csv", sep=";")
df.index = df['date']

df2 = df[['hour', 'dow', 'stMU62_IMI_tmed', 'season', 'energy']]


def setTrainTestSets(df, trainStart, trainEnd, testStart, testEnd, indY):
    trainSet = df[trainStart : trainEnd]
    testSet = df[testStart : testEnd]
    trainX = trainSet.values[:,0:-1]
    trainY = trainSet.values[:,indY]
    testX = testSet.values[:,0:-1]
    testY = testSet.values[:,indY]
    return trainX, trainY, testX, testY, testSet

trainStart = 1
trainEnd =  8000
testStart = 8001
testEnd = 9000



df = df2

trainX_hourlyElectricity, trainY_hourlyElectricity, testX_hourlyElectricity, testY_hourlyElectricity, \
    testSet_hourlyElectricity = setTrainTestSets(df, trainStart, trainEnd, testStart, testEnd, 4) 


def crossValidation_all(theta, nugget, nfold, trainX, trainY):
    thetaU = theta * 2
    thetaL = theta/2
    scores = np.zeros((len(nugget) * len(theta), nfold))
    labels = ["" for x in range(len(nugget) * len(theta))]
    k = 0
    for j in range(len(theta)):
        for i in range(len(nugget)):
            gp = gaussian_process.GaussianProcess(theta0 = theta[j], nugget = nugget[i])
            scores[k, :] = cross_validation.cross_val_score(gp, trainX, trainY, scoring='r2', cv = nfold)  
            labels[k] = str(theta[j]) + '|' + str(nugget[i])  
            k = k + 1
    plt.figure(figsize=(20,8))
    plt.boxplot(scores.T, sym='b+', labels = labels, whis = 0.5)
    plt.ylim([0,1])
    plt.title('R2 score as a function of nugget')
    plt.ylabel('R2 Score')
    plt.xlabel('Choice of theta | nugget')
    plt.show()

def crossValidation(theta, nugget, nfold, trainX, trainY):
    scores = np.zeros((len(theta), nfold))
    for i in range(len(theta)):
        gp = gaussian_process.GaussianProcess(theta0 = theta[i], nugget = nugget)
        scores[i, :] = cross_validation.cross_val_score(gp, trainX, trainY, scoring='r2', cv = nfold)
    plt.boxplot(scores.T, sym='b+', labels = theta, whis = 0.5)
    #plt.ylim(ylim)
    plt.title('R2 score as a function of theta0')
    plt.ylabel('R2 Score')
    plt.xlabel('Choice of theta0')
    plt.show()
    


def predictAll(theta, nugget, trainX, trainY, testX, testY, testSet, title):
    gp = gaussian_process.GaussianProcess(theta0=theta, nugget =nugget)
    gp.fit(trainX, trainY)
    predictedY, MSE = gp.predict(testX, eval_MSE = True)
    sigma = np.sqrt(MSE)
    results = testSet.copy()
    results['predictedY'] = predictedY
    results['sigma'] = sigma
    print "Train score R2:", gp.score(trainX, trainY)
    print "Test score R2:", sklearn.metrics.r2_score(testY, predictedY)
    plt.figure(figsize = (9,8))
    plt.scatter(testY, predictedY)
    plt.plot([min(testY), max(testY)], [min(testY), max(testY)], 'r')
    plt.xlim([min(testY), max(testY)])
    plt.ylim([min(testY), max(testY)])
    plt.title('Predicted vs. observed: ' + title)
    plt.xlabel('Observed')
    plt.ylabel('Predicted')
    plt.show()    
    return gp, results

nugget = 0.008
theta = np.arange(0.05, 0.5, 0.01)
crossValidation(theta, nugget, 10, trainX_hourlyElectricity, trainY_hourlyElectricity)


gp_hourlyElectricity, results_hourlyElectricity = predictAll(10, 4000, trainX_hourlyElectricity, trainY_hourlyElectricity, testX_hourlyElectricity, testY_hourlyElectricity, testSet_hourlyElectricity, 'Hourly Electricity (Partial)')
