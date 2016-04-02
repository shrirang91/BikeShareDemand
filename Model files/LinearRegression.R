# Project: Bike Share Rental
# Author : Shrirang Adgaonkar
# Date   : 11-11-2015
#Linear Regression for predicting count

# loading files  CHANGE AS PER REQUIREMENT
train <- read.csv("~/Desktop/train.csv")
test <- read.csv("~/Desktop/test.csv")

#install libraries
library(lubridate)
library(lmtest)
library(miscTools)
library(boot)

#Preprocessing ----------
#Compute hour of the day
train$hour <-hour(train$datetime)
test$hour <- hour(test$datetime)

#Compute day of the week
test$dow <- wday(test$datetime)
train$dow <- wday(train$datetime)

test$count<-0
#------------------------

# apply Linear regression 

model <- lm(count ~season+ temp+ dow+workingday+weather+humidity, data = train)

plot(count ~ ., data = train)
meancount <- mean(test$count)
abline(h=meancount)
abline(model,col="red")
plot(model)

pre <- predict(model,test)


coeftest(model)

fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table 
vcov(model) #covariance matrix


layout(matrix(c(1,2,3,4),2,2)) #  4 graphs/page 
plot(model)

(r21 <- rSquared(test$count, test$count - pre))
(mse <- mean ((test$count - pre))^2)


#predict the test cases and write in a file

output <- data.frame(datetime = test$datetime, count = pre)
write.csv(output, file = "LinearRegression1.csv", row.names = FALSE)


