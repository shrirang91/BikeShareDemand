# Project: Predict Bike Sharing Demand using R
# Author : Shrirang Adgaonkar
# Date   : 11-14-2015
# Random Forest tree prediction of count

# loading files  CHANGE AS PER REQUIREMENT
train <- read.csv("~/Desktop/train.csv")
test <- read.csv("~/Desktop/test.csv")

#install libraries
library(lubridate)
library(randomForest)

#pre processing---------------
#Compute hour of the day
train$hour <-hour(train$datetime)
train$dow <- wday(train$datetime)


test$hour <- hour(test$datetime)
#Compute day of the week


test$dow <- wday(test$datetime)

test$count<-0
#----------------------------

#Create a random forest
fit <- randomForest(as.factor(count) ~ season + weather + dow + hour + temp + humidity , data=train, ntree = 250, importance=TRUE)
varImpPlot(fit)

#Predict values and save output
Prediction <- predict(fit, test)
output <- data.frame(datetime = test$datetime, count = Prediction)
summary(train)

write.csv(output, file = "randomforestOutputfeature.csv", row.names = FALSE)



