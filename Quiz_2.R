#Quiz 2
library(caret)

#1
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)
library(ggplot2)

training <- training[order(training$CompressiveStrength),]

ggplot(training, aes(y=CompressiveStrength, x = seq(1,length(training$CompressiveStrength),1))) + geom_bar(stat="identity")


cutAge <- cut2(training$Age, g = 4)

qplot(cutAge, CompressiveStrength, data=training, fill=cutAge, geom=c("boxplot"))

cutFlyAsh <- cut2(training$FlyAsh, g = 4)

qplot(cutFlyAsh, CompressiveStrength, data=training, fill=cutFlyAsh, geom=c("boxplot"))

ggplot(training, aes(y=Superplasticizer)) + geom_histogram(stat="identity")

#3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

preProcess(training$Superplasticizer)
hist(training$Superplasticizer, breaks=10)
hist(log(training$Superplasticizer), breaks=10)

range(training$Superplasticizer)

#4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

PCAindex <- grep("^IL", colnames(training))
preProc <- preProcess(training[,PCAindex], method="pca", thresh = 0.80)
preProc

#5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

PCAindex <- grep("^IL", colnames(training), ignore.case = TRUE)
diagindex <- grep("diagnosis", colnames(training))
neededcolumns <- c(PCAindex,diagindex)
training <- training[,neededcolumns]
testing <- testing[,neededcolumns]

modelFit1 <- train(diagnosis~., method="glm", data=training)
pred <- predict(modelFit1, testing)
confusionMatrix(testing$diagnosis, pred)

preProc <- preProcess(training[,1:12], method="pca", thresh = 0.80)
trainPC <- predict(preProc, training)
modelfit2 <- train(training$diagnosis ~ ., method="glm", data=trainPC)
testPC <- predict(preProc, testing)
pred <- predict(modelfit2, testPC)
confusionMatrix(testing$diagnosis, pred)

modelfit2
