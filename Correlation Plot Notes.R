### Correlation Plot Options

setwd("C:/Users/jeffthatcher/Cloud Drive/RRepos/PracticalMachineLearning/PracticalMachineLearning")
library(caret)
library(ggplot2)
library(Hmisc)
library(psych)
library(corrplot)

### Separate data in to training and test sets
set.seed(1000)

#load training dataset
trainData <- as.data.frame(read.csv("pml-training.csv", na.strings=c("","NA")))
#Eliminate `new_window` = yes
trainData <- trainData[-which(trainData$new_window == "yes"),]

#load testing dataset
validation <- as.data.frame(read.csv("pml-testing.csv", na.strings=c("","NA")))

cleanData <- function(dataframe){
        
        #Eliminate columns with NA values
        dataframe <- dataframe[, colSums(is.na(dataframe)) == 0]
        
        #Eliminate columns with unrelated data
        dataframe <- dataframe[,-c(1,3,4,5,6)]
        
        return(dataframe)
}
trainData <- cleanData(trainData)
validation <- cleanData(validation)
### Separate into training and test data sets
trainIndex = createDataPartition(trainData$classe, p = 0.60,list=FALSE)
training = trainData[trainIndex,]
testing = trainData[-trainIndex,]


##Remove highly correlated variables
# calculate correlation matrix
correlationMatrix <- cor(training[,2:54])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.6)
# print columns to remove to reduce pair-wise correlations
print(highlyCorrelated)

library(corrplot)
corrplot(correlationMatrix)

correlationMatrix <- data.frame(correlationMatrix)
correlationMatrix$x1 <- rownames(correlationMatrix)

library(reshape2)
correlationMatrix <- melt(correlationMatrix)
head(correlationMatrix)

ggplot(data = correlationMatrix, aes(x=x1, y=variable, fill=value)) + 
        geom_tile()

ggplot(correlationMatrix, aes)