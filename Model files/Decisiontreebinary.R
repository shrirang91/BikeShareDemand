# Project: Bike Share Rental
# Author : Shrirang Adgaonkar
# Date   : 11-09-2015
#Decision tree and predict if biking is YES or NO

# loading files  CHANGE AS PER REQUIREMENT
train <- read.csv("~/Desktop/train.csv")
test <- read.csv("~/Desktop/test.csv")

#install libraries
library(lubridate)
library(rpart)
library(caret)
library(FSelector)
library(e1071)
library(party)

#Preprocessing ----------
#Compute hour of the day
train$hour <-hour(train$datetime)
test$hour <- hour(test$datetime)

#Compute day of the week
test$dow <- wday(test$datetime)
train$dow <- wday(train$datetime)

test$count<-0
#------------------------

#create binary count attribute
train$binarycount <- cut(train$count,br=c(-1,250,10000), labels= c("no","yes"))
test$binarycount <- cut(test$count,br=c(-1,250,10000), labels= c("no","yes"))


#entropy calculation
weights <- information.gain(binarycount~., train)
print(weights)


#create decision tree
fit <- ctree(binarycount ~ dow + hour + 
               temp + humidity +season +holiday+workingday+weather+windspeed, data = train)
#summary(fit)

#prune the tree
#pfit <- prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

#print the tree
#plot(fit, uniform = T, compress = T, margin = 0.1, branch = 0.3)
#text(fit, use.n = T, digits = 3, cex = 0.6)

#predict the test cases and write in a file
pre <- predict(fit,test)#,type="class")

#confusion matrix
confusionMatrix(pre,test$binarycount)

output <- data.frame(datetime = test$datetime, binarycount = pre)
write.csv(output, file = "DecisionBinary11.csv", row.names = FALSE)



