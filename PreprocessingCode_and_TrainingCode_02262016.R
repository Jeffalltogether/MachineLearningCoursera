


#Separate training and test data
# We will remove one person from each level of the variable `classe` as the test set
set.seed(1000)
trainIndex = createDataPartition(featuresDF$classe, p = 0.60,list=FALSE)
training = featuresDF[trainIndex,]
testing = featuresDF[-trainIndex,]


###Linear Discriminant Analysis
## remove predictors with linear dependencies
colToRemove <- c(grep("classe", colnames(featuresDF)), 
                 grep("user_name", colnames(featuresDF)))

predictors <- data.frame(featuresDF[,-colToRemove])
outcome <- featuresDF$classe

predictors <- predictors[, -findCorrelation(cor(predictors), .60)]

subFeatureDF <- as.data.frame(cbind(outcome, predictors))

### Train lda Classifier
#fit model
Lda <- train(outcome ~ ., method="lda", data=subFeatureDF)

#predict test data
pred <- predict(Lda, testing)
confusionMatrix(testing$classe, pred)

###Trees
library(rattle)
library(rpart.plot)

#fit model
tree <- train(classe ~ ., method="rpart", data=featuresDF)

#predict test data
pred <- predict(tree, testing)
confusionMatrix(testing$classe, pred)

prp(tree$finalModel)

###Bagging
#fit model
bagging <- train(classe ~ ., method="treebag", data=featuresDF)

#predict test data
pred <- predict(bagging, testing)
confusionMatrix(testing$classe, pred)

#Plot tree # 25
fancyRpartPlot(summary(bagging)$mtrees[[25]]$btree)


###PCA preprocessing with LDA
### Fit model with PCA preprocessing
#pre-process with PCA
preProc <- preProcess(training[1:65], method="pca", thresh = 0.80)
trainPC <- predict(preProc, training)

ggplot(trainPC, aes(PC1, PC2, color=classe))+geom_point()

#fit models
LdaPC <- train(training$classe ~ ., method="lda", data=trainPC)

#apply pre-processing to test data
testPC <- predict(preProc, testing)

#predict test data
pred <- predict(LdaPC, testPC)
confusionMatrix(testing$classe, pred)


### remove some zero variance predictors and linear dependencies
## remove some zero variance predictors and linear dependencies
colToRemove <- c(grep("classe", colnames(featuresDF)), 
                 grep("user_name", colnames(featuresDF)))

predictors <- data.frame(featuresDF[,-colToRemove])
outcome <- featuresDF$classe

predictors <- predictors[, -nearZeroVar(predictors)]
predictors <- predictors[, -findCorrelation(cor(predictors), .95)]