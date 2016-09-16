# Quiz 3 Answers, JHU Practical Machine Learning @ Coursera
# Predicting with Trees, Random Forest, Gradient Boosting Machine, Model-based Predictions



## Q1
library(AppliedPredictiveModeling); library(caret); library(ElemStatLearn); library(pgmm); library(rpart); library(rattle)

data(segmentationOriginal)

training <- segmentationOriginal[segmentationOriginal$Case == "Train", ]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test", ]

# remove 'Case' variable
training <- training[, -2]
testing <- testing[, -2]

set.seed(125)

modCART <- train(Class~., method = "rpart", data = training)
fancyRpartPlot(modCART$finalModel)

pred <- predict(modCART, testing)

#obs <- data.frame(TotalIntenCh2 = c(23000, 50000, 57000, NA), 
#                  FiberWidthCh1 = c(10, 10, 8, 1),
#                  PerimStatusCh1 = c(2, NA, NA, 2),
#                  VarIntenCh4 = c(NA, 100, 100, 100)
#)

obs <- testing[1010, ]

obs[obs!=0] <- 0
#obs$TotalIntenCh2 <- 57000
obs$FiberWidthCh1 <- 8
obs$PerimStatusCh1 <- 2
obs$VarIntenCh4 <- 100

pred1 <- predict(modCART, obs)
pred1


## Q3
library(pgmm)
data(olive)
olive = olive[,-1]

olive$Area <- as.factor(olive$Area)

modTree <- train(Area ~ ., method = "rpart", data = olive)
plot(modTree$finalModel, uniform = TRUE, main = "Classification Tree")
text(modTree$finalModel, use.n = TRUE, all = TRUE, cex = .8)

newdata = as.data.frame(t(colMeans(olive)))

predict(modTree, newdata)


## Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modLR <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, method="glm", family="binomial", data=trainSA)
predTrain <- predict(modLR, trainSA)
predTest <- predict(modLR, testSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predTrain)
missClass(testSA$chd, predTest)



## Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

modRF <- train(y~., method = "rf", data = vowel.train, prox = TRUE)

varImp(modRF)


