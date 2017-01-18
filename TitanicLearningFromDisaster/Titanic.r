rm(list=ls())
train <- read.csv("~/R/train.csv")
test <- read.csv("~/R/test.csv")
train$Fare2 <- '30+'
train$Fare2[train$Fare >=20 & train$Fare <30 ] <- '20-30'
train$Fare2[train$Fare >=10 & train$Fare <20 ] <- '10-20'
train$Fare2[train$Fare < 10 ] <- '<10'
aggregate(Survived~ Sex +Pclass + Fare2, data = train, FUN = function(x){sum(x)/length(x)})
test$Survived <- 0
test$Fare[is.na(test$Fare)] <- 12

rm(list=ls())
train 
<- read.csv("~/R/train.csv")
test <- read.csv("~/R/test.csv")
train$Fare2 <- '30+'
train$Fare2[train$Fare >=20 & train$Fare <30 ] <- '20-30'
train$Fare2[train$Fare >=10 & train$Fare <20 ] <- '10-20'
train$Fare2[train$Fare < 10 ] <- '<10'
aggregate(Survived~ Sex +Pclass + Fare2, data = train, FUN = function(x){sum(x)/length(x)})
test$Survived <- 0
test$Fare[is.na(test$Fare)] <- 12

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)


Prediction <- predict(fit, test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = 'sixtypercent.csv', row.names = F)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)



#Prediction <- predict(fit, test, type = 'class')
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = 'sixtypercent.csv', row.names = F)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data = train, method='class',  control=rpart.control(minsplit=2, cp=0))
new.fit <- prp(fit,snip = TRUE)$obj
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = 'class')

train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + 
               FamilySize + FamilyId, data=train, method = 'class')
fancyRpartPlot(fit)
prediction <- predict(fit, test, type='class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)