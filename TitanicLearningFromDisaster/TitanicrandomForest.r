rm(list=ls())
setwd("~/R")
train <- read.csv("~/R/train.csv")
test <- read.csv("~/R/test.csv")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#feature engineering
test$Survived <- NA
combi <- rbind(train,test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ','', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyId <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyId[combi$FamilySize <=2] <- 'small'
famIDs <- data.frame(table(combi$FamilyId))
famIDs <- famIDs[famIDs$Freq <=2,]
combi$FamilyId[combi$FamilyId %in% famIDs$Var1] <- 'small'
combi$FamilyId <-factor(combi$FamilyId)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age), ])
combi$Embarked[combi$Embarked == ''] <- 'S'
combi$Embarked <- factor(combi$Embarked)
combi$Fare[is.na(combi$Fare)] <- median(combi$Fare, na.rm = TRUE)
combi$FamilyId2 <- combi$FamilyId
combi$FamilyId2 <- as.character(combi$FamilyId2)
combi$FamilyId2[combi$FamilySize <= 3] <- 'small'
combi$FamilyId2 <- factor(combi$FamilyId2)

##Round 2
combi$CabinCode <- NA
combi$Cabin <- as.character(combi$Cabin)
combi$CabinCode <- sapply(combi$Cabin, FUN = function(x){strsplit(x, split = '[ 0-9]')[[1]][1][1]})
combi$CabinCode[!is.na(combi$CabinCode)] <- 1
combi$CabinCode[is.na(combi$CabinCode)] <- 0
combi$CabinCode <- as.integer(combi$CabinCode)

##mine code snippet 2
combi$TicketIni <- NA
combi$Ticket <- as.character(combi$Ticket)
combi$TicketIni <- sapply(combi$Ticket, FUN = function(x){tail(strsplit(x, split = '[ ]')[[1]], n=1)})
combi$TicketIni <- factor(combi$TicketIni)
combi$TicketIni[combi$TicketIni == 'LINE'] <- 346574
combi$TicketIni <- as.integer(combi$TicketIni)

train <- combi[1:891,]
test <- combi[892:1309, ]

library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyId2,
                    data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)
predi <- predict(fit,test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = predi)
write.csv(submit, file = "randomforest.csv", row.names = FALSE)

library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyId + TicketIni ,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
predi <- predict(fit,test, OOB = TRUE, type = 'response')
submit <- data.frame(PassengerId = test$PassengerId, Survived = predi)
write.csv(submit, file = "cabinforest.csv", row.names = FALSE)


