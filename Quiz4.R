# Quiz 4 Answers, JHU Practical Machine Learning @ Coursera
# Regularized regression, ensemble models, time series prediction, unsupervised learning

## Q1
library(ElemStatLearn); library(caret)
data(vowel.train); data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

modRF <- train(y~., method="rf", data=vowel.train)
modGBM <- train(y~., method="gbm", data=vowel.train)


predRF <- predict(modRF, vowel.test)
predGBM <- predict(modGBM, vowel.test)

# prediction accuracy
missClassFactor = function(values,prediction){sum(prediction != values)/length(values)}
1- missClassFactor(vowel.test$y, predRF)
1- missClassFactor(vowel.test$y, predGBM)

isAgreed <- predRF == predGBM
pred.agreed <- predRF[isAgreed]
test.agreed <- vowel.test[isAgreed, ]
1- missClassFactor(test.agreed$y, pred.agreed)


## Q2
library(gbm); library(AppliedPredictiveModeling); library(caret)
set.seed(3433)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
modRF <- train(diagnosis~., method="rf", data=training)
modGBM <- train(diagnosis~., method="gbm", data=training)
modLDA <- train(diagnosis~., method="lda", data=training)

predRF <- predict(modRF, testing)
predGBM <- predict(modGBM, testing)
predLDA <- predict(modLDA, testing)

# ensembling all three models
predDF <- data.frame(predRF, predGBM, predLDA, wage=testing$diagnosis)
combMod <- train(wage~., method="rf", data=predDF)

predComb <- predict(combMod, testing$diagnosis)

accComb <- confusionMatrix(predComb, testing$diagnosis)$overall[1]
accRF <- confusionMatrix(predRF, testing$diagnosis)$overall[1]
accGBM <- confusionMatrix(predGBM, testing$diagnosis)$overall[1]
accLDA <- confusionMatrix(predLDA, testing$diagnosis)$overall[1]
print(paste(accComb, accRF, accGBM, accLDA))

#1- missClassFactor(testing$diagnosis, predComb)
#1- missClassFactor(testing$diagnosis, predRF)
#1- missClassFactor(testing$diagnosis, predGBM)
#1- missClassFactor(testing$diagnosis, predLDA)


## Q3
library(AppliedPredictiveModeling)
library(caret)
library(elasticnet)
library(glmnet)

data(concrete)
set.seed(3523)

inTrain <- createDataPartition(y=concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]

set.seed(233)
# using glmnet package - have not figured out
#modLasso <- glmnet(x=as.matrix(subset(training, select=-c(CompressiveStrength))),
#                 y=training$CompressiveStrength)
#plot.glmnet(modLasso, ,xvar="lambda", label=TRUE)

modLasso <- train(CompressiveStrength~., method="lasso", data=training)
plot.enet(modLasso$finalModel, xvar = "penalty", use.color = T)

## Q4
library(lubridate)
library(forecast)       # for Time Series forecast

raw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", header = TRUE)
raw <- raw[, -1]
training <- raw[year(raw$date)<2012, ]
testing <- raw[year(raw$date)>2011, ]
tstrain <- ts(training$visitsTumblr)

# fit a time series model
tsfit <- bats(tstrain)

pred <- forecast(tsfit, level = 95, h=dim(testing)[1])
predComb <- cbind(testing, data.frame(pred))

# calculate % of actual values fall into the 95 percentile prediction interval
predComb$in95 <- (predComb$Lo.95 < predComb$visitsTumblr) & (predComb$visitsTumblr < predComb$Hi.95)
prop.table(table(predComb$in95))[2]


## Q5
# Unsupervised Learning

set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)  # SVM

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain, ]
testing = concrete[-inTrain, ]

set.seed(325)

modsvm <- svm(CompressiveStrength~., data = training)
predsvm <- predict(modsvm, testing[, -9])

#RMSE <- sqrt(sum((predsvm-testing$CompressiveStrength)^2)/length(predsvm))
acc <- accuracy(predsvm, testing$CompressiveStrength)
acc[2]



