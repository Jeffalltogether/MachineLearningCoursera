### Quiz 3
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

DF <- data.frame(segmentationOriginal)
training = DF[DF$Case == "Train",]

set.seed(125)

decisionTree <- train(Class ~ ., method="rpart", data=training)

library(rattle)
library(rpart.plot)
fancyRpartPlot(decisionTree$finalModel)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10; VarIntenCh4 = 100
# WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8; VarIntenCh4 = 100
# PS
# d. FiberWidthCh1 = 8; VarIntenCh4 = 100; PerimStatusCh1=2
# Not Possible



# 2. If K is small in a K-fold cross validation is the bias in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger? If K is small is the 
# variance in the estimate of out-of-sample (test set) accuracy smaller or 
# bigger. Is K large or small in leave one out cross validation?

# A. The bias is larger and the variance is smaller. Under leave one out cross 
# validation K is equal to the sample size.

# 3. 
set.seed(777)
library(pgmm)
data(olive)
olive = olive[,-1]

olive <- data.frame(olive)

oliveTree <- train(Area ~ ., method="rpart", data=olive) #ignore warnings

newdata = as.data.frame(t(colMeans(olive)))

predict(oliveTree, newdata)

# 4. 
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

# Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol 
# consumption, obesity levels, cumulative tabacco, type-A behavior, 
# and low density lipoprotein cholesterol as predictors

LDA <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = "binomial", data = trainSA)

missClass = function(values,prediction){
        sum(((prediction > 0.5)*1) != values)/length(values)
        }

pred <- predict(LDA, trainSA)
train <- missClass(trainSA$chd, pred)
pred <- predict(LDA, testSA)
test <- missClass(testSA$chd, pred)
test
train
# 5.
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

trainDF <- data.frame(vowel.train)
testDF <- data.frame(vowel.test)

trainDF$y <- factor(trainDF$y)
testDF$y <- factor(testDF$y)

DF <- rbind(trainDF, testDF)

set.seed(33833)

model <- randomForest(y ~ ., data = trainDF)

order(varImp(model), decreasing = T)