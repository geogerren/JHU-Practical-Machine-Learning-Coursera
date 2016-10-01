##################################################################################
##  Final Project, JHU Practical Machine Learning @ Coursera
##  10/1/2016
##

library(caret)
library(rattle)

#### Read data
rawTrain <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header=TRUE)
rawTest <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header=TRUE)
#rawTrain <- read.csv("rawTrain.csv", header=TRUE)
#rawTest <- read.csv("rawTest.csv", header=TRUE)
write.csv(rawTrain, "rawTrain.csv")
write.csv(rawTest, "rawTest.csv")
#write.csv(t(summary(rawTrain)), "trainSummary.csv")

#### Preprocessing - Data Cleasing
rawTrain <- rawTrain[, -1]
rawTest <- rawTest[, -1]

# remove near-zero values
nzv <- nearZeroVar(rawTrain, saveMetrics = TRUE)
training <- rawTrain[, nzv$zeroVar==FALSE & nzv$nzv==FALSE]

# Remove features with high missing value percentage
missPerc <- colMeans(is.na(training))       # calculate NA percentage
training <- training[, missPerc <= 0.8]

# remove a few username and timestamp vars not helpful for predicting
training <- subset(training, select = -c(1:5))

write.csv(t(summary(training)), "trainingSetSum.csv")

# splitting training and validation sets
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
training_final <- training[inTrain, ]
validation <- training[-inTrain, ]

# Fit a rpart decision tree
fit.rpart <- train(classe~., method="rpart", data=training_final)
fit.gbm <- train(classe~., method="gbm", data=training_final)

plot(fit.rpart$finalModel, uniform = TRUE, main = "Classification Tree")
text(fit.rpart$finalModel, use.n = TRUE, all = TRUE, cex = .8)

fancyRpartPlot(fit.rpart$finalModel)

pred <- predict(fit.rpart, training_final)

missClass = function(values,prediction){sum(prediction != values)/length(values)}

missClass(training_final$classe, pred)

confusionMatrix(training_final$classe, pred)




