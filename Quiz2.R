# Quiz 2 Answers, JHU Practical Machine Learning @ Coursera
# Caret package, PCA, Multivariate Regression


library(AppliedPredictiveModeling); library(Hmisc); library(ggplot2); library(caret)

data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#
qplot(y = CompressiveStrength, colour = cut2(training$FlyAsh, g=4), data=training)
qplot(y = CompressiveStrength, colour = cut2(training$Age, g=3), data=training)

#
qplot(Superplasticizer, data = training, bins = 30)

#
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain, ]

# how many PCs to capture .90 of the variance
preProc <- preProcess(training[, c(58:69)], method="pca", thresh = .80)
pcFit <- predict(preProc, training[, c(58:69)])

#
train.new <- training[, c(1, 58:69)]
test.new <- testing[, c(1, 58:69)]

mod <- train(diagnosis ~ ., method = "glm", data = train.new)
modFit <- predict(mod, test.new)

preProc <- preProcess(train.new, method="pca", pcaComp = 7)     # 7 PCs capture .80 of the variance
modFit.pca <- predict(preProc, test.new)

# calculate and compare accuracy
table(modFit == test.new$diagnosis)
table(modFit.pca[,1] == test.new$diagnosis)



