# HepB-LiveTest-validation
A bi-national external validation study for Hep B LiveTest
---
title: "Model validation study"
author: "Busayo Ajuwon"
date: "24 October 2022"
output:
  pdf_document: default
  word_document: default
  html_document: default
editor_options: 
  chunk_output_type: inline
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Install necessary packages 
```{r cars}
library(ggplot2)
library(plotly)
library(psych)
library(caret)
library(rattle)
```

Import derivation dataset- Allcases

```{r}
library(readr)
Allcases <- read_csv("Allcases.csv")
View(Allcases)
```

Import the U validation cohort

```{r}
library(readr)
uithdata<- read_csv("uithdata.csv")
View(uithdata)
```

Import the A validation dataset

```{r}
library(readr)
ozdata <- read_csv("ozdata.csv")
View(ozdata)
```

Confirm the number of positive and negative HBSA infections in derivation dataset

```{r}
Allcases$HBSA <- as.factor(Allcases$HBSA)
summary (Allcases$HBSA)
```

Confirm th numbe rof psoitive and negative HBSA infections in U validation dataset

```{r}
uithdata$HBSA <- as.factor(uithdata$HBSA)
summary (uithdata$HBSA)
```

Confirm the number of positive and negative HBSA infections in the A validation dataset

```{r}
ozdata$HBSA <- as.factor(ozdata$HBSA)
summary (ozdata$HBSA)
```

Feature selection and setting parameters for the derivation model

```{r}

set.seed(100)
subsets <- c(1:5, 10, 15, 20)

ctrl <- rfeControl(functions = rfFuncs,
method = "repeatedcv",
repeats = 5,
verbose = FALSE)

lmProfile <- rfe(x=Allcases[, 1:20], y=Allcases$HBSA,
sizes = subsets,
rfeControl = ctrl)
lmProfile
```

Train and tune the derivaton model 

```{r}
set.seed(100)
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(Allcases$HBSA, p=0.7, list=FALSE)

# Step 2: Create the training dataset
trainData <- Allcases[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- Allcases[-trainRowNumbers,]

# step 4: create the datasets for validation 
valdata <- uithdata

valdata_2 <- ozdata

```

```{r}
fitControl <- trainControl(
  method = 'repeatedcv',            # repeated cross validation
  number = 10,                      # number of folds
  repeats = 10,                    # number of repeats 
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
)
```

Support Vector Machine Analysis- Train with SVM: top predictors only

```{r}
set.seed(100)
model_svmRadial = train(HBSA ~ AST + WBC + Age + ALT + ALB, data=trainData, method='svmRadial', tuneLength=5, trControl = fitControl)

model_svmRadial

```

Predict on test data

```{r}
#Predict on test data
predicted <- predict(model_svmRadial, testData)
head(predicted)

#Compute the confusion matrix
confusionMatrix(reference = testData$HBSA, data = predicted, mode='everything', positive='positive')
```

Predict on U validation data

```{r}
#Predict on test data
predicted <- predict(model_svmRadial, valdata)
head(predicted)

#Compute the confusion matrix
confusionMatrix(reference = valdata$HBSA, data = predicted, mode='everything', positive='positive')
```

PLOT ROC CURVE- Assess performance of the derivation model (model_svmRadial) on U validation dataset (valdata)

load pROC library

```{r}
library(pROC)
svm_prediction_val <- predict(model_svmRadial, valdata, type ="prob")

par(pty = "s") 
roc(valdata$HBSA, svm_prediction_val[,2], plot=TRUE, legacy.axes = TRUE, xlab="False positive rate (1-Specifity)", ylab="True postive rate (Sensitivity)", col="#377eb8", lwd=4, print.auc=TRUE)
```

Experiment 2 
Retrain the derivation model without AST (Since AST was missing from the ozdataset) and plot ROC

```{r}
set.seed(100)
model_svmRadial_2 = train(HBSA ~ WBC + Age + ALT + ALB, data=trainData, method='svmRadial', tuneLength=5, trControl = fitControl)
model_svmRadial_2
```

```{r}

#Predict on val data_2 (A validation data)
predicted_3 <- predict(model_svmRadial_2, valdata_2)
head(predicted_3)
#Compute the confusion matrix
confusionMatrix(reference = valdata_2$HBSA, data = predicted_3, mode='everything', positive='positive')
```

PLOT ROC to assess peformance of svm_prediction_val_2 on A validation cohort (without the AST marker)

```{r}
svm_prediction_val_2 <- predict(model_svmRadial_2, valdata_2, type ="prob")

par(pty = "s") 
roc(valdata_2$HBSA, svm_prediction_val_2 [,2], plot=TRUE, legacy.axes = TRUE, xlab="False positive rate (1-Specifity)", ylab="True postive rate (Sensitivity)", col="#377eb8", lwd=4, print.auc=TRUE)
```

Layer U and A validation performance ROC graphs 

```{r}
par(pty = "s") 
roc(valdata$HBSA, svm_prediction_val[,2], plot=TRUE, legacy.axes = TRUE, xlab="False positive rate (1-Specifity)", ylab="True postive rate (Sensitivity)", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(valdata_2$HBSA, svm_prediction_val_2 [,2], col="#4daf4a", lwd=4, print.auc=TRUE, add= TRUE, print.auc.y=.4)

legend("bottomright", legend=c("UITH validation cohort", "SNP validation cohort"), col=c("#377eb8", "#4daf4a"), lwd=4)
```
