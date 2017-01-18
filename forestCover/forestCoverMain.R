#ForestCover
rm(list=ls())
train <- read.csv("~/Kaggle/Kaggle-ML/forestCover/train.csv")
test <- read.csv("~/Kaggle/Kaggle-ML/forestCover/test.csv")

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

library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
################
set.seed(415)
fit <- randomForest(Cover_Type ~ .,data=train[-1], importance=TRUE, ntree=2000, na.action = na.omit)
varImpPlot(fit)
predi <- predict(fit,test)
submit <- data.frame(Id = test$Id, Cover_Type = predi)
write.csv(submit, file = "~/Kaggle/forestCover/randomforestcover.csv", row.names = FALSE)
###################

###################
#set.seed(415)
#fit <- cforest(as.factor(Cover_Type) ~ Elevation + Aspect + Slope +  
#               + Horizontal_Distance_To_Fire_Points + Wilderness_Area1 + Wilderness_Area2 ,
#              data = train, controls=cforest_unbiased(ntree=2000, mtry=3))