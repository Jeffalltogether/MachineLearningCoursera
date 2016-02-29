### Quiz 4
library(caret)
#Question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

training <- vowel.train
testing <- vowel.test

training$y <- as.factor(training$y)
testing$y <- as.factor(testing$y)

set.seed(33833)
rforest <- train(y ~ ., method = "rf", data=training)
boosting <- train(y ~ ., method = "gbm", data=training) 

pred1 <- predict(rforest, testing)
confusionMatrix(testing$y, pred1)$overall[1]

pred2 <- predict(boosting, testing)
confusionMatrix(testing$y, pred2)$overall[1]

predDF <- data.frame(pred1, pred2, y = testing$y, agree=pred1 == pred2)

accuracy <- sum(pred1[predDF$agree] == predDF$y[predDF$agree]) / sum(predDF$agree)
accuracy

#Question 2
library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
rforest <- train(diagnosis ~ ., method = "rf", data=training)
boosting <- train(diagnosis ~ ., method = "gbm", data=training) 
lda <- train(diagnosis ~ ., method = "gbm", data=training) 

pred1 <- predict(rforest, testing)
confusionMatrix(testing$diagnosis, pred1)

pred2 <- predict(boosting, testing)
confusionMatrix(testing$diagnosis, pred2)

pred3 <- predict(lda, testing)
confusionMatrix(testing$diagnosis, pred3)

predDF <- data.frame(pred1, pred2, pred3, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~., method="gam", data = predDF)

combPred <- predict(combModFit, predDF)
confusionMatrix(predDF$diagnosis, combPred)

# A: Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.

#Question 3
set.seed(3523)
library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

lasso <- train(CompressiveStrength ~ ., method = "lasso", data = training)

# Since we are interested in the shrinkage of coefficients as the penalty(lambda) increases, "
# penalty" looks promising for an xvar argument value.
plot.enet(lasso$finalModel, xvar = "penalty", use.color = TRUE)

#A: cement

#Question 4
setwd("C:/Users/jeffthatcher/Cloud Drive/RRepos/PracticalMachineLearning/MachineLearningCoursera")
library(lubridate) # For year() function below
library(forecast)
dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

#train the model
fitTS <- bats(tstrain)

# check how long the test set is, so you can predict beyond trainign
h <- dim(testing)[1]

# forecast the model for remaining time points
fcast <- forecast(fitTS, level = 95, h = h)

# get the accuracy
accuracy(fcast, testing$visitsTumblr)

# check what percentage of times that the actual number of visitors was within
# 95% confidence interval

result <- c()
l <- length(fcast$lower)

for (i in 1:l){
        x <- testing$visitsTumblr[i]
        a <- fcast$lower[i] < x & x < fcast$upper[i]
        result <- c(result, a)
}

sum(result)/l * 100

# A: 96%

#Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]
set.seed(325)
SVM <- svm(CompressiveStrength ~., data=training)

pred <- predict(fit, testing)

acc <- accuracy(pred, testing$CompressiveStrength)

acc[2] # RMSE 6.715009
