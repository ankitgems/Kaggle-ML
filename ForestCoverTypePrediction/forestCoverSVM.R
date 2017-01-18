#ForestCover
rm(list=ls())
train <- read.csv("~/Kaggle/forestCover/train.csv")
test <- read.csv("~/Kaggle/forestCover/test.csv")
source("C:/Users/exmachina/Documents/R/usefulFunc.r")
#combining data
test$Cover_Type<- NA
comb <- rbind(train,test)

#Feature engineering
comb$Soil <- NA
comb$Wilderness <- NA

comb$Soil <- apply(comb[grep("Soil_Ty+",colnames(comb))], 1 , function(x){which(x == 1)})
comb$Wilderness <- apply(comb[grep("Wilderness_Are+",colnames(comb))], 1 , function(x){which(x == 1)})
comb$Soil <- as.factor(comb$Soil)
comb$Wilderness <- as.factor(comb$Wilderness)
comb$Cover_Type <- as.factor(comb$Cover_Type)
comb[grep("Soil_Type+",colnames(comb))] <- NULL
comb[grep("Wilderness_Area+",colnames(comb))] <- NULL


train <- comb[1:15120,]
test <- comb[15121:581012,]

ran_index <- trainAndCv(train)
cv <- train[-ran_index,]
trainer <- train[ran_index,]
trainer <- trainer[-1]
cv <- cv[-1]

#feature Normalization
meanSd <- meanAndSd(trainer, 1,10)
trainer <- featureNormalize(trainer,1,10, meanSd)
cv <- featureNormalize(cv,1,10, meanSd)

#svm
library(e1071)
model <- svm(Cover_Type ~ ., trainer, cost = 64, epsilon = 0.001)
predictY <- predict(model,cv)
Accuracy(cv$Cover_Type, predictY)

tuneResult <- tune(svm, Cover_Type ~., data = trainer, ranges = list(epsilon = seq(0,0.02,0.01), cost = 2^(5:6)))
tuneModel <- tuneResult$best.model
tunepredictY <- predict(tuneModel, cv)
Accuracy(cv$Cover_Type, tunepredictY)