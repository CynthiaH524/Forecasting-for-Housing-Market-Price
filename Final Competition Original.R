# Final Competition
# Cynthia Huang, Haiyue Wang

# load all libraries

library(tidyverse)
library(forecast)
library(rvest)
library(tidyr)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(lubridate)
library(dygraphs)
library(MLmetrics)
library(dplyr)
library(tidyquant)


################################################################################

# Data preparation

################################################################################
# Importing the data set
getSymbols("HOUSTNSA", src = 'FRED')
HOUSTNSA
MonthlyY = ts(HOUSTNSA, start=c(1959,01,01),end = c(2022,03,01), frequency = 12)

##################################################################################################

# •	Scenario 1:  

##################################################################################################
# Monthly predictions: Training set <2007. 
# Loop through all months in the testing set to predict next month’s value. 
# Each time refit the model.
monthlyY.train = window(MonthlyY, end = c(2006,12))
monthlyY.test = window(MonthlyY, start = c(2007,1))

N = length(window(MonthlyY, start = c(2006, 12)))
s1AccuracyTable = data.frame(Model = NA, trainAvgRMSE = NA,trainAvgMAPE = NA, testAvgRMSE= NA, testAvgMAPE = NA)

################################################################################
# Naïve model for Scenario 1:
################################################################################
S1Nmodel_accuracy_train=NULL
S1Nmodel_accuracy_test=NULL

for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+i))
  MF = snaive(train, h=length(test))
  S1Nmodel_accuracy_train = rbind(S1Nmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S1Nmodel_accuracy_test = rbind(S1Nmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s1TrainA = S1Nmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1TestA = S1Nmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1AccuracyTable = rbind(s1AccuracyTable, c("Naive", s1TrainA[,1], s1TrainA[,2], 
                                           s1TestA[,1], s1TestA[,2]))
s1AccuracyTable = s1AccuracyTable[-1,]


################################################################################
# Time series linear model for Scenario 1:
################################################################################
S1TSmodel_accuracy_train=NULL
S1TSmodel_accuracy_test=NULL

for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+i))
  M = tslm(train~trend+season, lambda = "auto") 
  MF = forecast(M, h = length(test), level = FALSE) 
  S1TSmodel_accuracy_train = rbind(S1TSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S1TSmodel_accuracy_test = rbind(S1TSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s1TrainTS = S1TSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1TestTS = S1TSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1AccuracyTable = rbind(s1AccuracyTable, c("TimeSeriesLinear", s1TrainTS[,1], s1TrainTS[,2], 
                                           s1TestTS[,1], s1TestTS[,2]))

################################################################################
# Exponential Smoothing model for Scenario 1:
################################################################################
S1ETSmodel_accuracy_train=NULL
S1ETSmodel_accuracy_test=NULL

for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+i))
  M = ets(train, lambda = "auto")
  MF = forecast(M, h = length(test), level = FALSE)
  S1ETSmodel_accuracy_train = rbind(S1ETSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S1ETSmodel_accuracy_test = rbind(S1ETSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s1TrainETS = S1ETSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1TestETS = S1ETSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1AccuracyTable = rbind(s1AccuracyTable, c("ETS", s1TrainETS[,1], s1TrainETS[,2], 
                                           s1TestETS[,1], s1TestETS[,2]))


################################################################################
# Arima model for Scenario 1:
################################################################################
S1Amodel_accuracy_train=NULL
S1Amodel_accuracy_test=NULL

# using auto arima for the first training set to determine the p,d,q coefficients
train_1a = window(MonthlyY, end = c(2006, 12))
test_1a = window(MonthlyY, start = c(2006, 12), end=c(2006, 12))
M_1a = auto.arima(train_1a, lambda = "auto") # ARIMA(2,0,0)(0,1,1)[12]
#MF_1a = forecast(M, h=length(test_1a), level = FALSE)
MFtable = NULL
for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+i))
  M = arima(train, order = c(2,0,0), seasonal = list(order = c(0,1,1)))
  MF = forecast(M, h=length(test), level = FALSE)
  S1Amodel_accuracy_train = rbind(S1Amodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S1Amodel_accuracy_test = rbind(S1Amodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
  MF=data.frame(MF)
  MFtable = rbind(MFtable,MF)
}
NAs=data.frame(rep(NA,577))
colnames(NAs)="Point.Forecast"
MFtable
MFtable = MFtable[1]
data.frame(MFtable)
ArimaS1 = data.frame(MFtable)
ArimaS1=rbind(NAs,ArimaS1)
ArimaS1$Time = seq(1:dim(ArimaS1)[1])


s1TrainA = S1Amodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1TestA = S1Amodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1AccuracyTable = rbind(s1AccuracyTable, c("Arima", s1TrainA[,1], s1TrainA[,2], 
                                           s1TestA[,1], s1TestA[,2]))

################################################################################
# Neural Network for Scenario 1:
################################################################################
S1NNmodel_accuracy_train=NULL
S1NNmodel_accuracy_test=NULL

pp=1:5
PP=1:3
# Try with neural network loop inside the loops for all
for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+i))
  nnresultsTr = data.frame(RMSE = NA, MAPE = NA)
  nnresultsTe = data.frame(RMSE = NA, MAPE = NA)
  for(p in pp){
    for(P in PP){
      Mt = nnetar(train, p=p, P=P, lambda = "auto")
      MFt = forecast(Mt, h = length(test), level = FALSE)
      nnresultsTr = rbind(nnresultsTr, accuracy(MFt, test)[1,c("RMSE", "MAPE")])
      nnresultsTe = rbind(nnresultsTe, accuracy(MFt, test)[2,c("RMSE", "MAPE")])
    }
  }
  nnresultsTr = nnresultsTr[-1,]
  nnresultsTe = nnresultsTe[-1,]
  accuracyTrain = nnresultsTr[nnresultsTr$MAPE == min(nnresultsTr$MAPE),]
  accuracyTest = nnresultsTe[nnresultsTe$MAPE == min(nnresultsTe$MAPE),]
  S1NNmodel_accuracy_train = rbind(S1NNmodel_accuracy_train, accuracyTrain)
  S1NNmodel_accuracy_test = rbind(S1NNmodel_accuracy_test, accuracyTest)
}


s1TrainNN = S1NNmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1TestNN = S1NNmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s1AccuracyTable = rbind(s1AccuracyTable, c("NeuralNetwork", s1TrainNN[,1], s1TrainNN[,2], 
                                           s1TestNN[,1], s1TestNN[,2]))



##################################################################################################

# •	Scenario 2:  

##################################################################################################
# 12-month predictions: Training set <2007. 
# Loop through all 12-month periods in the testing set to predict next 12 months 
# value. Each time refit the model.
monthlyY.train = window(MonthlyY, end = c(2006,12))
monthlyY.test = window(MonthlyY, start = c(2007,1))

N = length(window(MonthlyY, start = c(2006, 12)))
s2AccuracyTable = data.frame(Model = NA, trainAvgRMSE = NA,trainAvgMAPE = NA, testAvgRMSE= NA, testAvgMAPE = NA)

################################################################################
# Naïve model for Scenario 2:
################################################################################
S2Nmodel_accuracy_train=NULL
S2Nmodel_accuracy_test=NULL

for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+12+i))
  MF = snaive(train, h=length(test))
  S2Nmodel_accuracy_train = rbind(S2Nmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S2Nmodel_accuracy_test = rbind(S2Nmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s2TrainA = S2Nmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2TestA = S2Nmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2AccuracyTable = rbind(s2AccuracyTable, c("Naive", s2TrainA[,1], s2TrainA[,2], 
                                           s2TestA[,1], s2TestA[,2]))
s2AccuracyTable = s2AccuracyTable[-1,]

################################################################################
# Time series linear model for Scenario 2:
################################################################################
S2TSmodel_accuracy_train=NULL
S2TSmodel_accuracy_test=NULL

for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+12+i))
  M = tslm(train~trend+season, lambda = "auto") 
  MF = forecast(M, h = length(test), level = FALSE) 
  S2TSmodel_accuracy_train = rbind(S2TSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S2TSmodel_accuracy_test = rbind(S2TSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s2TrainTS = S1TSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2TestTS = S1TSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2AccuracyTable = rbind(s2AccuracyTable, c("TimeSeriesLinear", s2TrainTS[,1], s2TrainTS[,2], 
                                           s2TestTS[,1], s2TestTS[,2]))

################################################################################
# Exponential Smoothing model for Scenario 2:
################################################################################
S2ETSmodel_accuracy_train=NULL
S2ETSmodel_accuracy_test=NULL

for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+12+i))
  M = ets(train, lambda = "auto")
  MF = forecast(M, h = length(test), level = FALSE)
  S2ETSmodel_accuracy_train = rbind(S2ETSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S2ETSmodel_accuracy_test = rbind(S2ETSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s2TrainETS = S2ETSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2TestETS = S2ETSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2AccuracyTable = rbind(s2AccuracyTable, c("ETS", s2TrainETS[,1], s2TrainETS[,2], 
                                           s2TestETS[,1], s2TestETS[,2]))


################################################################################
# Arima model for Scenario 2:
################################################################################
S2Amodel_accuracy_train=NULL
S2Amodel_accuracy_test=NULL

# using auto arima for the first training set to determine the p,d,q coefficients
train_2a = window(MonthlyY, end = c(2006, 12))
test_2a = window(MonthlyY, start = c(2006, 12), end=c(2006, 12+12))
M_2a = auto.arima(train_2a, lambda = "auto")
#ARIMA(2,0,0)(0,1,1)[12]

for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+12+i))
  M = arima(train, order = c(2,0,0), seasonal = list(order = c(0,1,1)))
  #M= auto.arima(train, lambda = "auto")
  MF = forecast(M, h=length(test), level = FALSE)
  S2Amodel_accuracy_train = rbind(S2Amodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S2Amodel_accuracy_test = rbind(S2Amodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}


s2TrainA = S2Amodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2TestA = S2Amodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2AccuracyTable = rbind(s2AccuracyTable, c("Arima", s2TrainA[,1], s2TrainA[,2], 
                                           s2TestA[,1], s2TestA[,2]))

################################################################################
# Neural Network for Scenario 2:
################################################################################
S2NNmodel_accuracy_train=NULL
S2NNmodel_accuracy_test=NULL

pp=1:5
PP=1:3
# Try with neural network loop inside the loops for all
for(i in 1:N){
  train = window(MonthlyY, end = c(2006, 12+i-1))
  test = window(MonthlyY, start = c(2006, 12+i), end=c(2006, 12+12+i))
  nnresultsTr = data.frame(RMSE = NA, MAPE = NA)
  nnresultsTe = data.frame(RMSE = NA, MAPE = NA)
  for(p in pp){
    for(P in PP){
      Mt = nnetar(train, p=p, P=P, lambda = "auto")
      MFt = forecast(Mt, h = length(test), level = FALSE)
      nnresultsTr = rbind(nnresultsTr, accuracy(MFt, test)[1,c("RMSE", "MAPE")])
      nnresultsTe = rbind(nnresultsTe, accuracy(MFt, test)[2,c("RMSE", "MAPE")])
    }
  }
  nnresultsTr = nnresultsTr[-1,]
  nnresultsTe = nnresultsTe[-1,]
  accuracyTrain = nnresultsTr[nnresultsTr$MAPE == min(nnresultsTr$MAPE),]
  accuracyTest = nnresultsTe[nnresultsTe$MAPE == min(nnresultsTe$MAPE),]
  S2NNmodel_accuracy_train = rbind(S2NNmodel_accuracy_train, accuracyTrain)
  S2NNmodel_accuracy_test = rbind(S2NNmodel_accuracy_test, accuracyTest)
}

s2TrainNN = S2NNmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2TestNN = S2NNmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s2AccuracyTable = rbind(s2AccuracyTable, c("NeuralNetwork", s2TrainNN[,1], s2TrainNN[,2], 
                                           s2TestNN[,1], s2TestNN[,2]))


##################################################################################################

# •	Scenario 3:  

##################################################################################################
# Quarterly predictions: Training set <2007. 
# Loop through all quarters in the testing set to predict next quarter’s value. 
# Each time refit the model.
Monthly = ts(HOUSTNSA, start =c(1943,01,01), end = c(2021,12,01),frequency = 12)
QuarterlyY  = aggregate(Monthly, nfrequency = 4)
N3 = length(window(QuarterlyY, start = c(2006, 4)))
s3AccuracyTable = data.frame(Model = NA, trainAvgRMSE = NA,trainAvgMAPE = NA, testAvgRMSE= NA, testAvgMAPE = NA)

################################################################################
# Naïve model for Scenario 3:
################################################################################
S3Nmodel_accuracy_train=NULL
S3Nmodel_accuracy_test=NULL

for(i in 1:N3){
  train = window(QuarterlyY, end = c(2006, 4+i-1))
  test = window(QuarterlyY, start = c(2006, 4+i), end=c(2006, 4+i))
  MF = snaive(train, h=length(test))
  S3Nmodel_accuracy_train = rbind(S3Nmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S3Nmodel_accuracy_test = rbind(S3Nmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s3TrainN = S3Nmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3TestN = S3Nmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3AccuracyTable = rbind(s3AccuracyTable, c("Naive", s3TrainN[,1], s3TrainN[,2], 
                                           s3TestN[,1], s3TestN[,2]))
s3AccuracyTable = s3AccuracyTable[-1,]

################################################################################
# Time series linear model for Scenario 3:
################################################################################
S3TSmodel_accuracy_train=NULL
S3TSmodel_accuracy_test=NULL

for(i in 1:N3){
  train = window(QuarterlyY, end = c(2006, 4+i-1))
  test = window(QuarterlyY, start = c(2006, 4+i), end=c(2006, 4+i))
  M = tslm(train~trend+season, lambda = "auto") 
  MF = forecast(M, h = length(test), level = FALSE) 
  S3TSmodel_accuracy_train = rbind(S3TSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S3TSmodel_accuracy_test = rbind(S3TSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s3TrainTS = S3TSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3TestTS = S3TSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3AccuracyTable = rbind(s3AccuracyTable, c("TimeSeriesLinear", s3TrainTS[,1], s3TrainTS[,2], 
                                           s3TestTS[,1], s3TestTS[,2]))

################################################################################
# Exponential Smoothing model for Scenario 3:
################################################################################
S3ETSmodel_accuracy_train=NULL
S3ETSmodel_accuracy_test=NULL

for(i in 1:N3){
  train = window(QuarterlyY, end = c(2006, 4+i-1))
  test = window(QuarterlyY, start = c(2006, 4+i), end=c(2006, 4+i))
  M = ets(train, lambda = "auto")
  MF = forecast(M, h = length(test), level = FALSE)
  S3ETSmodel_accuracy_train = rbind(S3ETSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S3ETSmodel_accuracy_test = rbind(S3ETSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s3TrainETS = S3ETSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3TestETS = S3ETSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))

s3AccuracyTable = rbind(s3AccuracyTable, c("ETS", s3TrainETS[,1], s3TrainETS[,2], 
                                           s3TestETS[,1], s3TestETS[,2]))

################################################################################
# Arima model for Scenario 3:
################################################################################
S3Amodel_accuracy_train=NULL
S3Amodel_accuracy_test=NULL


for(i in 1:N3){
  train = window(QuarterlyY, end = c(2006, 4+i-1))
  test = window(QuarterlyY, start = c(2006, 4+i), end=c(2006, 4+i))
  M = auto.arima(train, lambda = "auto")
  MF = forecast(M, h=length(test), level = FALSE)
  S3Amodel_accuracy_train = rbind(S3Amodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S3Amodel_accuracy_test = rbind(S3Amodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}


s3TrainA = S3Amodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3TestA = S3Amodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))

s3AccuracyTable = rbind(s3AccuracyTable, c("Arima", s3TrainA[,1], s3TrainA[,2], 
                                           s3TestA[,1], s3TestA[,2]))

################################################################################
# Neural Network for Scenario 3:
################################################################################
S3NNmodel_accuracy_train=NULL
S3NNmodel_accuracy_test=NULL

pp=1:5
PP=1:3

# Try with neural network loop inside the loops for all
for(i in 1:N){
  train = window(QuarterlyY, end = c(2006, 4+i-1))
  test = window(QuarterlyY, start = c(2006, 4+i), end=c(2006, 4+i))
  nnresultsTr = data.frame(RMSE = NA, MAPE = NA)
  nnresultsTe = data.frame(RMSE = NA, MAPE = NA)
  for(p in pp){
    for(P in PP){
      Mt = nnetar(train, p=p, P=P, lambda = "auto")
      MFt = forecast(Mt, h = length(test), level = FALSE)
      nnresultsTr = rbind(nnresultsTr, accuracy(MFt, test)[1,c("RMSE", "MAPE")])
      nnresultsTe = rbind(nnresultsTe, accuracy(MFt, test)[2,c("RMSE", "MAPE")])
    }
  }
  nnresultsTr = nnresultsTr[-1,]
  nnresultsTe = nnresultsTe[-1,]
  accuracyTrain = nnresultsTr[nnresultsTr$MAPE == min(nnresultsTr$MAPE),]
  accuracyTest = nnresultsTe[nnresultsTe$MAPE == min(nnresultsTe$MAPE),]
  S3NNmodel_accuracy_train = rbind(S3NNmodel_accuracy_train, accuracyTrain)
  S3NNmodel_accuracy_test = rbind(S3NNmodel_accuracy_test, accuracyTest)
}

s3TrainNN = S3NNmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3TestNN = S3NNmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s3AccuracyTable = rbind(s3AccuracyTable, c("NeuralNetwork", s3TrainNN[,1], s3TrainNN[,2], 
                                           s3TestNN[,1], s3TestNN[,2]))



##################################################################################################

# •	Scenario 4:  

##################################################################################################
# Annual predictions: Training set <2007. 
# Loop through all years in the testing set to predict next year’s value. 
#Each time refit the model.
YearlyY = ts(aggregate(MonthlyY, nfrequency = 1), frequency =1, start = 1959,end=2022)
#yearlyY.train = window(YearlyY, end = c(2006))
#yearlyY.test = window(YearlyY, start = c(2006))
N4 = length(window(YearlyY, start = c(2006, 1)))
s4AccuracyTable = data.frame(Model = NA, trainAvgRMSE = NA,trainAvgMAPE = NA, testAvgRMSE= NA, testAvgMAPE = NA)

################################################################################
# Naïve model for Scenario 4:
################################################################################
S4Nmodel_accuracy_train=NULL
S4Nmodel_accuracy_test=NULL

for(i in 1:N4){
  train = window(YearlyY, end = c(2006, 1+i-1))
  test = window(YearlyY, start = c(2006, 1+i), end=c(2006, 1+i))
  MF = snaive(train, h=length(test))
  S4Nmodel_accuracy_train = rbind(S4Nmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S4Nmodel_accuracy_test = rbind(S4Nmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s4TrainN = S4Nmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s4TestN = S4Nmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))

s4AccuracyTable = rbind(s4AccuracyTable, c("Naive", s4TrainN[,1], s4TrainN[,2], 
                                           s4TestN[,1], s4TestN[,2]))
s4AccuracyTable = s4AccuracyTable[-1,]

################################################################################
# Time series linear model for Scenario 4:
################################################################################
S4TSmodel_accuracy_train=NULL
S4TSmodel_accuracy_test=NULL

for(i in 1:N4){
  train = window(YearlyY, end = c(2006, 1+i-1))
  test = window(YearlyY, start = c(2006, 1+i), end=c(2006, 1+i))
  M = tslm(train~trend, lambda = "auto") 
  MF = forecast(M, h = length(test), level = FALSE) 
  S4TSmodel_accuracy_train = rbind(S4TSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S4TSmodel_accuracy_test = rbind(S4TSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s4TrainTS = S4TSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s4TestTS = S4TSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))

s4AccuracyTable = rbind(s4AccuracyTable, c("TimeSeriesLinear", s4TrainTS[,1], s4TrainTS[,2], 
                                           s4TestTS[,1], s4TestTS[,2]))

################################################################################
# Exponential Smoothing model for Scenario 4:
################################################################################
S4ETSmodel_accuracy_train=NULL
S4ETSmodel_accuracy_test=NULL

for(i in 1:N4){
  train = window(YearlyY, end = c(2006, 1+i-1))
  test = window(YearlyY, start = c(2006, 1+i), end=c(2006, 1+i))
  M = ets(train, lambda = "auto")
  MF = forecast(M, h = length(test), level = FALSE)
  S4ETSmodel_accuracy_train = rbind(S4ETSmodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S4ETSmodel_accuracy_test = rbind(S4ETSmodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
}

s4TrainETS = S4ETSmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s4TestETS = S4ETSmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))

s4AccuracyTable = rbind(s4AccuracyTable, c("ETS", s4TrainETS[,1], s4TrainETS[,2], 
                                           s4TestETS[,1], s4TestETS[,2]))
################################################################################
# Arima model for Scenario 4:
################################################################################
S4Amodel_accuracy_train=NULL
S4Amodel_accuracy_test=NULL
MFtableS4=NULL

for(i in 1:N4){
  train = window(YearlyY, end = c(2006, 1+i-1))
  test = window(YearlyY, start = c(2006, 1+i), end=c(2006, 1+i))
  M = auto.arima(train, lambda = "auto")
  MF = forecast(M, h=length(test), level = FALSE)
  S4Amodel_accuracy_train = rbind(S4Amodel_accuracy_train, accuracy(MF, test)[1,c("RMSE", "MAPE")])
  S4Amodel_accuracy_test = rbind(S4Amodel_accuracy_test, accuracy(MF, test)[2,c("RMSE", "MAPE")])
  MFtableS4=rbind(MFtableS4,MF)
}
# 79
# NAs=data.frame(rep(NA,577))
# colnames(NAs)="Point.Forecast"
# MFtable
# MFtable = MFtable[1]
# data.frame(MFtable)
# ArimaS1 = data.frame(MFtable)
# ArimaS1=rbind(NAs,ArimaS1)
# ArimaS1$Time = seq(1:dim(ArimaS1)[1])

s4TrainA = S4Amodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s4TestA = S4Amodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))

s4AccuracyTable = rbind(s4AccuracyTable, c("Arima", s4TrainA[,1], s4TrainA[,2], 
                                           s4TestA[,1], s4TestA[,2]))

################################################################################
# Neural Network for Scenario 4:
################################################################################
S4NNmodel_accuracy_train=NULL
S4NNmodel_accuracy_test=NULL
 
pp=1:5

# Try with neural network loop inside the loops for all
for(i in 1:N){
  train = window(YearlyY, end = c(2006, 1+i-1))
  test = window(YearlyY, start = c(2006, 1+i), end=c(2006, 1+i))
  nnresultsTr = data.frame(RMSE = NA, MAPE = NA)
  nnresultsTe = data.frame(RMSE = NA, MAPE = NA)
  for(p in pp){
    Mt = nnetar(train, p=p, lambda = "auto")
    MFt = forecast(Mt, h = length(test), level = FALSE)
    nnresultsTr = rbind(nnresultsTr, accuracy(MFt, test)[1,c("RMSE", "MAPE")])
    nnresultsTe = rbind(nnresultsTe, accuracy(MFt, test)[2,c("RMSE", "MAPE")])
  }
  nnresultsTr = nnresultsTr[-1,]
  nnresultsTe = nnresultsTe[-1,]
  accuracyTrain = nnresultsTr[nnresultsTr$MAPE == min(nnresultsTr$MAPE),]
  accuracyTest = nnresultsTe[nnresultsTe$MAPE == min(nnresultsTe$MAPE),]
  S4NNmodel_accuracy_train = rbind(S4NNmodel_accuracy_train, accuracyTrain)
  S4NNmodel_accuracy_test = rbind(S4NNmodel_accuracy_test, accuracyTest)
}


s4TrainNN = S4NNmodel_accuracy_train %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))
s4TestNN = S4NNmodel_accuracy_test %>% data.frame() %>% summarize(mean(RMSE), mean(MAPE))

s4AccuracyTable = rbind(s4AccuracyTable, c("NeuralNetwork", s4TrainNN[,1], s4TrainNN[,2], 
                                           s4TestNN[,1], s4TestNN[,2]))
################################################################################

# LM model

################################################################################

# import the data from FRED
data = getSymbols("HOUSTNSA",src="FRED",auto.assign = FALSE)
data = data.frame(data)
head(data)

# Format the dataset
data$Date = rownames(data)
rownames(data) = seq(,dim(data)[1])
data = data[c("Date", "HOUSTNSA")]

# Look at the first and last few rows:
head(data)
tail(data)

# Look at the structure of data
# what is the type of each column 
str(data)
glimpse(data)

# Convert year into datetime object
data$Date = as.Date(data$Date, format="%Y-%m-%d")
data$Time = 1:dim(data)[1]
head(data)

# Check if there is null data
is.na(data)

# Create a graph of HOUSTNSA versus Time 
data %>% ggplot(aes(x=Lag2,y=HOUSTNSA))+geom_line()

# Plot 1 Time of data
years = 20
data %>% slice(1:(12*years)) %>% 
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+geom_point()
data %>% 
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+geom_point()

seq(12,(years-1)*12,by=12)
data[1:(12*years),] %>% 
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_vline(xintercept = seq(12,(years-1)*12,by=12),color="red")

# The data has annual seasonality (12 data points)
data$Month = c(rep(seq(1,12),dim(data)[1]/12),seq(1,3))
data$Month = factor(data$Month)
head(data,24)

# head(data,24)
dim(data)[1]
759/120
vec = c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),rep(7,12),
        rep(8,12),rep(9,12),rep(10,12))

# replicate each integer in vec 3 times
rep(vec, each = 6)
data$Year = c(10,rep(vec, 6),rep(1,12),rep(2,12),rep(3,12),rep(4,2))
data$Year = factor(data$Year)
head(data,24)

data$Trend =data$Time

glimpse(data)
head(data,24)
tail(data,24)

############# Create 1 lag
data$Lag1=lag(data$HOUSTNSA,1)
# Create 2 lag ACF PACF
data$Lag2=lag(data$HOUSTNSA,2)
# Create lag 3 for 12 
# data$Lag4=lag(data$HOUSTNSA,12)
data$Lag3=lag(data$HOUSTNSA,12)
data
# SPlitting the training and testing set
(2006-1959)*12

head(data)
tail(data)
traindata = data[1:576,1:dim(data)[2]]
head(traindata)
testdata = data[577:dim(data)[1],1:dim(data)[2]]
head(testdata)

data$Month = data$Month
tail(data,50)
glimpse(data)
# M2 =  monthly seasonality + annual seasonality
glimpse(traindata)

tail(traindata,24)

M1 = lm(HOUSTNSA ~ Trend+Month+Year,data = traindata)
summary(M1)
tsdisplay(M1$residuals)
M2 = lm(HOUSTNSA ~ Trend+Month+Year+Lag1+Lag2+Lag3,data = traindata)
summary(M2)
tsdisplay(M2$residuals)

tail(data)
M3 = lm(HOUSTNSA ~ Trend+Lag1+Lag2+Lag3,data = traindata)
summary(M3)
tsdisplay(M3$residuals)
#################################################################################
# Final model
###############################################################################
M4 = lm(HOUSTNSA ~ Trend+Lag1+Lag2+Lag3
        +Month+Trend:Month+Month:I(Trend^2)+Month:I(Trend^3),data = traindata)
summary(M4)

# Save prediction in column M2
traindata$M4 = predict(M4,newdata = traindata)
testdata$M4 = predict(M4,newdata = testdata)
head(traindata)
# plot data and overlay predictions
traindata %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=M4),color="red")+ theme_bw()

traindata =traindata[13:(dim(traindata)[1]),]
# RMSE & MAPE
RMSE(traindata$HOUSTNSA, traindata$M4)
MAPE(traindata$HOUSTNSA, traindata$M4)

################################################################################
# LM Scenario 1
################################################################################

S1LMmodel_accuracy_train=NULL
S1LMmodel_accuracy_test=NULL
ypred=NULL
N = dim(data[577:dim(data)[1],1:dim(data)[2]])[1]
for(i in 1:N){
  train = data[13:(576+i-1),1:dim(data)[2]]
  test = data[(576+i):(576+i),1:dim(data)[2]]
  LM = lm(HOUSTNSA ~ Trend+I(Trend^2)+I(Trend^3)+Lag1+Lag2+Lag3
          +Month+Trend:Month+Month:I(Trend^2)+Month:I(Trend^3),data = train)
  train$LM = predict(LM,newdata = train)
  test$LM = predict(LM,newdata = test)
  ypred = rbind(ypred,predict(LM,newdata = test))
  accuracy = c(RMSE(test$HOUSTNSA, test$LM),(MAPE(test$HOUSTNSA, test$LM))*100)
  S1LMmodel_accuracy_train = rbind(S1LMmodel_accuracy_train,
                                   c(RMSE(train$HOUSTNSA, train$LM),
                                     (MAPE(train$HOUSTNSA, train$LM))*100))
  S1LMmodel_accuracy_test = rbind(S1LMmodel_accuracy_test,accuracy)
}
# Create RMSE and MAPE table
s1TrainLM = S1LMmodel_accuracy_train %>% data.frame()%>% summarise_all(mean)
colnames(s1TrainLM)= c("RMSE","MAPE")
s1TestLM = S1LMmodel_accuracy_test %>% data.frame() %>% summarise_all(mean)
colnames(s1TestLM)= c("RMSE","MAPE")
s1TrainLM
s1TestLM
s1AccuracyTable = rbind(s1AccuracyTable, c("LM", s1TrainLM[,1], s1TrainLM[,2], 
                                           s1TestLM[,1], s1TestLM[,2]))
# each prediction point
ypred=data.frame(ypred)
colnames(ypred)="Ypred"
NAs=data.frame(rep(NA,576))
colnames(NAs)="Ypred"
ypredn=data.frame(rbind(NAs,ypred))
data = cbind(data,ypredn)

data$YPRED = ypredn
data %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=Ypred),color="red")+geom_vline(xintercept = 577,color="magenta")


# At time 2006 forecast 2007 
train = data[13:(576),1:dim(data)[2]]
test = data[(577):dim(data)[1],1:dim(data)[2]]
LM = lm(HOUSTNSA ~ Trend+I(Trend^2)+I(Trend^3)+Lag1+Lag2+Lag3
        +Month+Trend:Month+Month:I(Trend^2)+Month:I(Trend^3),data = train)
train$LMF = predict(LM,newdata = train)
test$LMF = predict(LM,newdata = test)
newData = rbind(train,test)
newData %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LMF),color="red")+
  geom_vline(xintercept = 577,color="magenta")


################################################################################
################################################################################
# LM Scenario 2
###############################################################################

S2LMmodel_accuracy_train=NULL
S2LMmodel_accuracy_test=NULL
tail(data)
head(data)

N2 = dim(data[577:dim(data)[1],1:dim(data)[2]])[1]-11
for(i in 1:N2){
  train = data[13:(576+i-1),1:dim(data)[2]]
  test = data[(576+i):(587+i),1:dim(data)[2]]
  LM = lm(HOUSTNSA ~ Trend+I(Trend^2)+I(Trend^3)+Lag1+Lag2+Lag3
          +Month+Trend:Month+Month:I(Trend^2)+Month:I(Trend^3),data = train)
  train$LMS2 = predict(LM,newdata = train)
  test$LMS2 = predict(LM,newdata = test)
  accuracy = c(RMSE(test$HOUSTNSA, test$LMS2),(MAPE(test$HOUSTNSA, test$LMS2))*100)
  S2LMmodel_accuracy_train = rbind(S2LMmodel_accuracy_train,
                                   c(RMSE(train$HOUSTNSA, train$LMS2),
                                     (MAPE(train$HOUSTNSA, train$LMS2))*100))
  S2LMmodel_accuracy_test = rbind(S2LMmodel_accuracy_test,accuracy)
}

s2TrainLM = S2LMmodel_accuracy_train %>% data.frame()%>% summarise_all(mean)
colnames(s2TrainLM)= c("RMSE","MAPE")
s2TestLM = S2LMmodel_accuracy_test %>% data.frame() %>% summarise_all(mean)
colnames(s2TestLM)= c("RMSE","MAPE")
s2TrainLM
s2TestLM
s2AccuracyTable = rbind(s2AccuracyTable, c("LM", s2TrainLM[,1], s2TrainLM[,2], 
                                           s2TestLM[,1], s2TestLM[,2]))



newdata = rbind(train,test)
newdata %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LMS2),color="red")+
  geom_vline(xintercept = 577,color="magenta")


################################################################################
# LM scenario 3
################################################################################

tail(data)
data = getSymbols("HOUSTNSA",src="FRED",auto.assign = FALSE)
data = data.frame(data)
data
# Format the dataset
data$Date = rownames(data)
dim(data)[1]
rownames(data) = seq(,dim(data)[1])
data = data[c("Date", "HOUSTNSA")]
Q = rep("Q",759)
quarter = quarter(data$Date)

quarter = paste(year(data$Date),Q,quarter)
data$YearQuarter = quarter
data = data[c("Date","YearQuarter","HOUSTNSA")]
head(data)

# Create quarterly data
Qdata = data %>% group_by(YearQuarter) %>% 
  summarise_at(vars(HOUSTNSA),funs(sum(.,na.rm=TRUE))) %>% 
  data.frame()

Qdata$Time = 1:dim(Qdata)[1]

head(Qdata)
glimpse(Qdata)

# Create quarter
Quarter = c(rep(seq(1:4),63))
Qdata$Quarter =c(rep(seq(1:4),63),1)
Qdata$Quarter = factor(Qdata$Quarter)
glimpse(Qdata)

# Create a graph of HOUSTNSA versus Time 
years = 10
Qdata %>% ggplot(aes(x=Time,y=HOUSTNSA))+geom_line()

Qdata[1:(4*years),] %>% 
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_vline(xintercept = seq(4,(years-1)*4,by=4),color="red")

# Create Year (0-10) for one decade 

Qdata$Trend =Qdata$Time
glimpse(Qdata)
head(Qdata)
tail(Qdata)

# M1
M1 = lm(HOUSTNSA ~ Trend+Quarter,data = Qdata)
summary(M1)
tsdisplay(M1$residuals)

# Create lags
Qdata$Lag1=lag(Qdata$HOUSTNSA,1)
# Create 2 lag ACF PACF
Qdata$Lag2=lag(Qdata$HOUSTNSA,4)

Qdata$Lag3=lag(Qdata$HOUSTNSA,5)

M2 = lm(HOUSTNSA ~ Trend+Quarter+Lag1+Lag2,data = Qdata)
summary(M2)
tsdisplay(M2$residuals)
M3 = lm(HOUSTNSA ~ Trend+Quarter+Lag1+Lag2+Lag3,data = Qdata)
summary(M3)
tsdisplay(M3$residuals)

#################################################################################
# Final model
###############################################################################

M4 = lm(HOUSTNSA ~ Trend+I(Trend^2)+I(Trend^3)+Lag1+Lag2+Lag3
        +Quarter+Trend:Quarter+Quarter:I(Trend^2)+Quarter:I(Trend^3),
        data = Qdata)
summary(M4)
head(Qdata)
# Save prediction in column M2
Qdata$LMS3 = predict(M4,newdata = Qdata)
head(Qdata)
# plot data and overlay predictions
nyears=60
Qdata %>% ggplot(aes(x=Time,y=HOUSTNSA))+geom_line()
Qdata %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LMS3),color="red")+ geom_point(aes(x=Time,y=LMS3),color="red")+
  theme_bw()
head(Qdata)

# Delete NA
S3data = Qdata[6:dim(Qdata)[1],1:dim(Qdata)[2]]

RMSE(S3data$HOUSTNSA, S3data$LMS3)
MAPE(S3data$HOUSTNSA, S3data$LMS3)

# Scenario 3

S3LMmodel_accuracy_train=NULL
S3LMmodel_accuracy_test=NULL
S3testdata=S3data[188:dim(S3data)[1],1:dim(S3data)[2]]
ypred=NULL
N = dim(S3testdata)[1]
for(i in 1:N){
  train = Qdata[6:(192+i-1),1:dim(Qdata)[2]]
  test = Qdata[(192+i):(192+i),1:dim(Qdata)[2]]
  LM = lm(HOUSTNSA ~ Trend+I(Trend^2)+I(Trend^3)+Lag1+Lag2+Lag3
          +Quarter+Trend:Quarter+Quarter:I(Trend^2)+Quarter:I(Trend^3),
          data = Qdata)
  train$LMS3 = predict(LM,newdata = train)
  test$LMS3 = predict(LM,newdata = test)
  ypred = rbind(ypred,predict(LM,newdata = test))
  accuracy = c(RMSE(test$HOUSTNSA, test$LMS3),(MAPE(test$HOUSTNSA, test$LMS3))*100)
  S3LMmodel_accuracy_train = rbind(S3LMmodel_accuracy_train,
                                   c(RMSE(train$HOUSTNSA, train$LMS3),
                                     (MAPE(train$HOUSTNSA, train$LMS3))*100))
  S3LMmodel_accuracy_test = rbind(S3LMmodel_accuracy_test,accuracy)
}

s3TrainLM = S3LMmodel_accuracy_train %>% data.frame()%>% summarise_all(mean)
colnames(s3TrainLM)= c("RMSE","MAPE")
s3TestLM = S3LMmodel_accuracy_test %>% data.frame() %>% summarise_all(mean)
colnames(s3TestLM)= c("RMSE","MAPE")
s3TrainLM 
s3TestLM 
s3AccuracyTable = rbind(s3AccuracyTable, c("LM", s3TrainLM[,1], s3TrainLM[,2], 
                                           s3TestLM[,1], s3TestLM[,2]))

ypred=data.frame(ypred)
colnames(ypred)="Ypred"
NAs=data.frame(rep(NA,192))
colnames(NAs)="Ypred"
ypredn=data.frame(rbind(NAs,ypred))
Qdata = cbind(Qdata,ypredn)

Qdata %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=Ypred),color="red")+geom_vline(xintercept = 192,color="magenta")


S3data = rbind(train,test)
S3data %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LMS3),color="red")+
  geom_point(aes(x=Time,y=LMS3),color="red")+
  geom_vline(xintercept = 192,color="magenta")+theme_bw()

#################################################################################
# LM Scenario 4
################################################################################
tail(data)
data = getSymbols("HOUSTNSA",src="FRED",auto.assign = FALSE)
data = data.frame(data)
data

# Format the dataset
data$Date = rownames(data)
dim(data)[1]
rownames(data) = seq(,dim(data)[1])
Year = data.frame(year(data$Date))
colnames(Year) = "Year"
data$Year = Year
data = data[c("Date","Year", "HOUSTNSA")]
head(data)

# Create annual data
Ydata = data %>% group_by(Year) %>% 
  summarise_at(vars(HOUSTNSA),funs(sum(.,na.rm=TRUE))) %>% 
  data.frame()

Ydata$Time = 1:dim(Ydata)[1]
head(Ydata)
glimpse(Ydata)

# Create a graph of HOUSTNSA versus Time 

Ydata %>% ggplot(aes(x=Time,y=HOUSTNSA))+geom_line()+geom_point()

glimpse(Ydata)
head(Ydata)
tail(Ydata)

# M1
M1S3 = lm(HOUSTNSA ~ Trend,data = Ydata)
summary(M1S3)
tsdisplay(M1S3$residuals)

# Create lag 1
Ydata$Lag1=lag(Ydata$HOUSTNSA,1)

M2S3 = lm(HOUSTNSA ~ Trend+Lag1,data = Ydata)
summary(M2S3)
tsdisplay(M2S3$residuals)

# Create lag2
Ydata$Lag2=lag(Ydata$HOUSTNSA,2)
M3S3 = lm(HOUSTNSA ~ Trend+Lag1+Lag2,data = Ydata)
summary(M3S3)
tsdisplay(M3S3$residuals)

head(Ydata)

#################################################################################
# Final model
###############################################################################

LMS4 = lm(HOUSTNSA ~ Trend+Lag1+Lag2,
          data = Ydata)
summary(LMS4)
tsdisplay(LMS4$residuals)

head(Ydata)
# Save prediction in column M2
Ydata$LMS4 = predict(LMS4,newdata = Ydata)
# plot data and overlay predictions
Ydata %>% ggplot(aes(x=Time,y=HOUSTNSA))+geom_line()
Ydata %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LMS4),color="red")+ geom_point(aes(x=Time,y=LMS4),color="red")+
  theme_bw()

# Delete NA
S4data = Ydata[2:64,1:dim(Ydata)[2]]

#########
S4LMmodel_accuracy_train=NULL
S4LMmodel_accuracy_test=NULL
tail(S4data)
S4testdata=S4data[48:dim(S4data)[1],1:dim(S4data)[2]]
train = Ydata[3:48,1:dim(Ydata)[2]]
tail(train)
N = dim(S4testdata)[1]
for(i in 1:N){
  train = Ydata[3:(48+i-1),1:dim(Ydata)[2]]
  test = Ydata[(48+i):(48+i),1:dim(Ydata)[2]]
  LM = lm(HOUSTNSA ~ Trend+Lag1+Lag2,
          data = Ydata)
  train$LMS4 = predict(LM,newdata = train)
  test$LMS4 = predict(LM,newdata = test)
  accuracy = c(RMSE(test$HOUSTNSA, test$LMS4),(MAPE(test$HOUSTNSA, test$LMS4))*100)
  S4LMmodel_accuracy_train = rbind(S4LMmodel_accuracy_train,
                                   c(RMSE(train$HOUSTNSA, train$LMS4),
                                     (MAPE(train$HOUSTNSA, train$LMS4))*100))
  S4LMmodel_accuracy_test = rbind(S4LMmodel_accuracy_test,accuracy)
}

s4TrainLM = S4LMmodel_accuracy_train %>% data.frame()%>% summarise_all(mean)
colnames(s4TrainLM)= c("RMSE","MAPE")
s4TestLM = S4LMmodel_accuracy_test %>% data.frame() %>% summarise_all(mean)
colnames(s4TestLM)= c("RMSE","MAPE")
s4TrainLM 
s4TestLM

s4AccuracyTable = rbind(s4AccuracyTable, c("LM", s4TrainLM[,1], s4TrainLM[,2], 
                                           s4TestLM[,1], s4TestLM[,2]))


S4data = rbind(train,test)

S4data %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LMS4),color="red")+
  geom_point(aes(x=Time,y=LMS4),color="red")+
  geom_vline(xintercept = 48,color="magenta")+theme_bw()

################################################################################

# Forming a big form for all methods under difference scenarios

################################################################################
s1AccuracyTable
s2AccuracyTable
s3AccuracyTable
s4AccuracyTable


# Average  MAPE per model across all scenarios
# Naive
NaiveTrainAvgMAPE = (as.numeric(s1AccuracyTable[1,3])+
                       as.numeric(s2AccuracyTable[1,3])+
                       as.numeric(s3AccuracyTable[1,3])+
                       as.numeric(s4AccuracyTable[1,3]))/4
NaiveTestAvgMAPE = (as.numeric(s1AccuracyTable[1,5])+
                      as.numeric(s2AccuracyTable[1,5])+
                      as.numeric(s3AccuracyTable[1,5])+
                      as.numeric(s4AccuracyTable[1,5]))/4
NaiveMAPE = rbind(NaiveTrainAvgMAPE,NaiveTestAvgMAPE)
colnames(NaiveMAPE)="Naive"

# TimeSeriesLinear
TSTrainAvgMAPE = (as.numeric(s1AccuracyTable[2,3])+
                    as.numeric(s2AccuracyTable[2,3])+
                    as.numeric(s3AccuracyTable[2,3])+
                    as.numeric(s4AccuracyTable[2,3]))/4
TSTestAvgMAPE = (as.numeric(s1AccuracyTable[2,5])+
                   as.numeric(s2AccuracyTable[2,5])+
                   as.numeric(s3AccuracyTable[2,5])+
                   as.numeric(s4AccuracyTable[2,5]))/4
TSMAPE = rbind(TSTrainAvgMAPE,TSTestAvgMAPE)
colnames(TSMAPE)="TS"

# ETS
ETSTrainAvgMAPE = (as.numeric(s1AccuracyTable[3,3])+
                     as.numeric(s2AccuracyTable[3,3])+
                     as.numeric(s3AccuracyTable[3,3])+
                     as.numeric(s4AccuracyTable[3,3]))/4
ETSTestAvgMAPE = (as.numeric(s1AccuracyTable[3,5])+
                    as.numeric(s2AccuracyTable[3,5])+
                    as.numeric(s3AccuracyTable[3,5])+
                    as.numeric(s4AccuracyTable[3,5]))/4
ETSMAPE=rbind(ETSTrainAvgMAPE,ETSTestAvgMAPE)
colnames(ETSMAPE)="ETS"

# Arima
ATrainAvgMAPE = (as.numeric(s1AccuracyTable[4,3])+
                   as.numeric(s2AccuracyTable[4,3])+
                   as.numeric(s3AccuracyTable[4,3])+
                   as.numeric(s4AccuracyTable[4,3]))/4
ATestAvgMAPE = (as.numeric(s1AccuracyTable[4,5])+
                  as.numeric(s2AccuracyTable[4,5])+
                  as.numeric(s3AccuracyTable[4,5])+
                  as.numeric(s4AccuracyTable[4,5]))/4
cbind(ATrainAvgMAPE,ATestAvgMAPE)

# Neural Network
NNTrainAvgMAPE = (as.numeric(s1AccuracyTable[5,3])+
                    as.numeric(s2AccuracyTable[5,3])+
                    as.numeric(s3AccuracyTable[5,3])+
                    as.numeric(s4AccuracyTable[5,3]))/4
NNTestAvgMAPE = (as.numeric(s1AccuracyTable[5,5])+
                   as.numeric(s2AccuracyTable[5,5])+
                   as.numeric(s3AccuracyTable[5,5])+
                   as.numeric(s4AccuracyTable[6,5]))/4
NNMAPE=rbind(NNTrainAvgMAPE,NNTestAvgMAPE)
colnames(NNMAPE)="NN"

# LM
LMTrainAvgMAPE = (as.numeric(s1AccuracyTable[6,3])+
                    as.numeric(s2AccuracyTable[6,3])+
                    as.numeric(s3AccuracyTable[6,3])+
                    as.numeric(s4AccuracyTable[6,3]))/4
LMTestAvgMAPE = (as.numeric(s1AccuracyTable[6,5])+
                   as.numeric(s2AccuracyTable[6,5])+
                   as.numeric(s3AccuracyTable[6,5])+
                   as.numeric(s4AccuracyTable[6,5]))/4
LMMAPE= rbind(LMTrainAvgMAPE,LMTestAvgMAPE)
colnames(LMMAPE)="LM"

# TABLE with train/test MAPE for all models
MAPETable = data.frame(Model = NA, trainAvgMAPE = NA, testAvgMAPE = NA)
MAPETable = rbind(MAPETable, c("Naive", NaiveTrainAvgMAPE, NaiveTestAvgMAPE))
MAPETable = MAPETable[-1,]
MAPETable = rbind(MAPETable, c("TimeSeriesLinear", TSTrainAvgMAPE, TSTestAvgMAPE))
MAPETable = rbind(MAPETable, c("ETS", ETSTrainAvgMAPE, ETSTestAvgMAPE))
MAPETable = rbind(MAPETable, c("Arima", ATrainAvgMAPE, ATestAvgMAPE))
MAPETable = rbind(MAPETable, c("NN", NNTrainAvgMAPE, NNTestAvgMAPE))
MAPETable = rbind(MAPETable, c("LM", LMTrainAvgMAPE, LMTestAvgMAPE))
MAPETable

##############################################################################
#Visualization 
##############################################################################

# Scenario 1 (Arima Model)
data
ArimaS1 = cbind(data,ArimaS1["Point.Forecast"],ArimaS1["Time"])
ArimaS1
ArimaS1 %>% ggplot(aes(x=Time,y=HOUSTNSA))+ geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=Point.Forecast),color="red")+
  geom_point(aes(x=Time,y=Point.Forecast),color="red")+
  geom_vline(xintercept=577,color="red")
data

# Scenario 1 LM model 
data %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=Ypred),color="red")+geom_vline(xintercept = 577,color="magenta")


# Scenario 2 (LM model)
newdata %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LMS2),color="red")+
  geom_point(aes(x=Time,y=LMS2),color="red")+
  geom_vline(xintercept = 577,color="magenta")

# Scenario 3 (LM Model)
Qdata %>%
  ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=Ypred),color="red")+geom_vline(xintercept = 192,color="magenta")


# Scenario 4 ()

S4NNmodel_accuracy_train=NULL
S4NNmodel_accuracy_test=NULL

pp=1:5
train = window(YearlyY, end = c(2006, 1))
test = window(YearlyY, start = c(2007, 1), end=c(2022, 1))
for(p in pp){
  Mt = nnetar(train, p=p, lambda = "auto")
  MFt = forecast(Mt, h = length(test), level = FALSE)
  nnresultsTr = rbind(nnresultsTr, accuracy(MFt, test)[1,c("RMSE", "MAPE")])
  nnresultsTe = rbind(nnresultsTe, accuracy(MFt, test)[2,c("RMSE", "MAPE")])
  MFtableS4 = rbind(MFtableS4,MFt) 
}
autoplot(MFt)+autolayer(test)


pp=1:5
MFtableS4 = NULL
# Try with neural network loop inside the loops for all
for(i in 1:N){
  train = window(YearlyY, end = c(2006, 1+i-1))
  test = window(YearlyY, start = c(2006, 1+i), end=c(2006, 1+i))
  nnresultsTr = data.frame(RMSE = NA, MAPE = NA)
  nnresultsTe = data.frame(RMSE = NA, MAPE = NA)
  for(p in pp){
    Mt = nnetar(train, p=p, lambda = "auto")
    MFt = forecast(Mt, h = length(test), level = FALSE)
    nnresultsTr = rbind(nnresultsTr, accuracy(MFt, test)[1,c("RMSE", "MAPE")])
    nnresultsTe = rbind(nnresultsTe, accuracy(MFt, test)[2,c("RMSE", "MAPE")])
    MFtableS4 = rbind(MFtableS4,MFt) 
  }
  nnresultsTr = nnresultsTr[-1,]
  nnresultsTe = nnresultsTe[-1,]
  accuracyTrain = nnresultsTr[nnresultsTr$MAPE == min(nnresultsTr$MAPE),]
  accuracyTest = nnresultsTe[nnresultsTe$MAPE == min(nnresultsTe$MAPE),]
  S4NNmodel_accuracy_train = rbind(S4NNmodel_accuracy_train, accuracyTrain)
  S4NNmodel_accuracy_test = rbind(S4NNmodel_accuracy_test, accuracyTest)
}
MFt
autoplot(MFt)+autolayer(test)


# LM model
train = Ydata[3:(48),1:dim(Ydata)[2]]
test = Ydata[(49):dim(Ydata)[1],1:dim(Ydata)[2]]
LM = lm(HOUSTNSA ~ Trend+Lag1+Lag2,
        data = Ydata)
train$LM4 = predict(LM,newdata = train)
test$LM4 = predict(LM,newdata = test)
LMS4data = rbind(train,test)
LMS4data %>% ggplot(aes(x=Time,y=HOUSTNSA)) + geom_line()+ geom_point()+
  geom_line(aes(x=Time,y=LM4),color="red")+
  geom_point(aes(x=Time,y=LM4),color="red")+
  geom_vline(xintercept = 48,color="magenta")+theme_bw()


# Q4:
MAPETable
# According to our MAPE table, the most accurate models are NN and LM. The Arima 
# model also performs well, and it is ranked as the third most efficient model.
# The model has the least traning and testing MAPE is NN model, for about 6.87 and 
# 8.75 in average for the four scenario. However, it takes long time to run the model,
# so it's not the most time and cost efficient model. T

# As the consulting firm, we would recommend LM for the private housing. 
# LM model is easy to modify and it performs well in most of the scenario,
# especially it has the least MAPE score for Scenario 2.

S4LMmodel_accuracy_test
S4NNmodel_accuracy_test

S4LMmodel_accuracy_test
S4NNmodel_accuracy_test


# Extra Credit:
# To improve the forecast, we can divide the housing market into smaller segments, 
# for example, high-price band, middle-price band, and low-price band (or by geographical regions). 
# With predictions from each finer categories weighted and added into the final housing trends, 
# the forecasts can be more precise.
# Last semester, my group and I conducted research on LA individual housing prices 
# using economic model. We include housing features, such as the number of beds, 
# area of houses, and ages built, and parameters measuring the community from education,
# medical service, safety factors, entertainment, economic level, 
# and population density (zip code or city). So, the independent variables include 
# number of baths and bedrooms in the house, lot size, area of the house in square feet, 
# year built, total crime in the zip code, the estimated number of homeless (per mi sq), 
# number of food stamp, graduate enrollment, undergraduate enrollment, 
# park space acres per capita, economic hardship index, hospital dummy, 
# good schools dummy (0 or 1), median income, house type, and population density (per mi sq). 
# The regression can capture 82.6 per cent of the variations in individual housing prices. 
# 
# Although economic model cannot be a useful way of prediction as the independent 
# variables are unknown in the future, it can lead us to think more of making predictions 
# for each finer category and sum it up for a better prediction.
