library(tidyverse)

## Random forest for predicting MMSE
library(randomForest)

alz_data = read.csv('alzheimers_disease_data.csv')
alz_data = alz_data[,-35]
alz_data = alz_data[,-1]

randomForest(MMSE~., data=alz_data, importance=TRUE, proximity=TRUE) # not much better than our linear models, RMSE of 8.2

## SVM for predicting diagnosis
library(e1071)

model <- svm(Diagnosis~., alz_data, type='C-classification')
pred <- fitted(model)

table(pred, alz_data$Diagnosis) # confusion matrix
# Accuracy: (1347+669) / (1347+669+91+42) = 93.81% of diagnoses predicted correctly

alz_data_pred <- alz_data%>%
  mutate(pred = fitted(model))


## SVM for predicting MMSE
model2 <- svm(MMSE~., alz_data, type='eps-regression', kernel='linear')
pred <- fitted(model2)
alz_data_pred <- alz_data_pred%>%
  mutate(pred_mmse = fitted(model2))

library(caret)
RMSE(alz_data_pred$MMSE, alz_data_pred$pred_mmse)
# 8.345
# Our final MLR model had a residual standard error of 8.607, so the SVM performed better but not by much

##################################################################################
#
# Results indicate that even when using alternative approaches such as the random 
#  forest or SVM, we could not predict MMSE well with this data.
#
# However, the SVM model predicted diagnosis very well, with an accuracy of almost
#  94%. We'll have to compare this to the performance of our logistic regression
#  model. 




