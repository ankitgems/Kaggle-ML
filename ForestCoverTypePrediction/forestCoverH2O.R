#Data loading
rm(list=ls())
train <- read.csv("~/Kaggle/Kaggle-ML/forestCover/train.csv")
test <- read.csv("~/Kaggle/Kaggle-ML/forestCover/test.csv")

#loading some useful functions
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

#Data separatring
train <- comb[1:15120,]
test <- comb[15121:581012,]

# dividing between train and CV
ran_index <- dataSlice(train)
cv <- train[-ran_index,]
trainer <- train[ran_index,]
trainer <- trainer[-1]
cv <- cv[-1]