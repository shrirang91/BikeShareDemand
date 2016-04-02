# Project: Bike Share Rental
# Author : Shrirang Adgaonkar
# Date   : 11-11-2015
# Bike rentals feature selection

# loading files  CHANGE AS PER REQUIREMENT
train <- read.csv("~/Desktop/train.csv")
test <- read.csv("~/Desktop/test.csv")

#libraries required
library(ggplot2)
library(lubridate)
library(randomForest)

# processing data
extractFeatures <- function(data) {
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "hour")
  data$hour <- hour(ymd_hms(data$datetime))
  return(data[,features])
}

trainF <- extractFeatures(train)
testF  <- extractFeatures(test)

submission <- data.frame(datetime=test$datetime, count=NA)

# Only past data is used to make predictions on the test set,

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testSubset <- test[testLocs,]
    trainLocs  <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
    rf <- randomForest(extractFeatures(train[trainLocs,]), train[trainLocs,"count"], ntree=100)
    output[testLocs, "count"] <- predict(rf, extractFeatures(testSubset))
  }
}

write.csv(output, file = "1_random_forest_submission.csv", row.names=FALSE)

# Train a model across all the training data and plot the variable importance
rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

plot <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#00ff00") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("Importance level") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))


ggsave("2_feature_importance.png", plot)
