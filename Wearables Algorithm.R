#Set working directory
setwd("~/Desktop/Practical Machine Learning")

#Load packages
library(caret)

#Download files
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              "pml-training.csv", method = "curl")
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
              "pml-testing.csv", method = "curl")

#Read data
training <- read.csv("pml-training.csv")
testing<- read.csv("pml-testing.csv")

#Remove Columns that have more than 60% NA values, remove timestamps and ID
training <- training[,colSums(is.na(testing))/nrow(testing) <= .6]
training <- training[,6:60]

#Create a validation set from the training set
set.seed(2)
inTrain <- createDataPartition(y = training$classe, p = .75, list = FALSE)

training <- training[inTrain,]
validation <- training[-inTrain,]

#Model creation
set.seed(2)
modFitRF <- train(classe ~ ., method = "rf", prox = TRUE, data = training, 
                  trControl = trainControl(method = "cv", number = 5), allowParallel = TRUE)
modFitRF

#Fit model against validation set
predvalRF <- predict(modFitRF, newdata = validation)
confusionMatrix(predvalRF, validation$classe)

#Fit model against testing set
predTestRF <- predict(modFitRF, newdata = testing)
predTestRF

#Write prediction files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predTestRF)
