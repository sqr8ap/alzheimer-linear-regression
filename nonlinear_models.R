library(tidyverse)

## Random forest for predicing MMSE
library(randomForest)

alz_data = read.csv('alzheimers_disease_data.csv')
alz_data = alz_data[,-35]
alz_data = alz_data[,-1]

randomForest(MMSE~., data=alz_data, importance=TRUE, proximity=TRUE) # not much better than our linear models

## SVM for predicting diagnosis
library(e1071)

model <- svm(Diagnosis~., alz_data, type='C-classification')
pred <- fitted(model)

table(pred, alz_data$Diagnosis) # confusion matrix
# Accuracy: (1347+669) / (1347+669+91+42) = 93.81% of diagnoses predicted correctly

alz_data_pred <- alz_data%>%
  mutate(pred = fitted(model))




