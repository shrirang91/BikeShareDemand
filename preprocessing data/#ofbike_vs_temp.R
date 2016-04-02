# Project: Bike Share Rental
# Author : Shrirang Adgaonkar
# Date   : 11-09-2015
#Time vs #of rentals plot with Temperature and Humidity variation

# loading files  CHANGE AS PER REQUIREMENT
train <- read.csv("~/Desktop/train.csv")
test  <- read.csv("~/Desktop/test.csv")

# libraries required
library(ggplot2)
library(lubridate)
library(scales)

#axis definition
x_axis <- "jitter_times"
y_axis <- "count"
color  <- "temp"  # can be replaced by Humidity


#preprocessing
train$hour  <- hour(ymd_hms(train$datetime))
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$jitter_times <- train$times+minutes(round(runif(nrow(train),min=0,max=59)))
train$day <- wday(ymd_hms(train$datetime), label=TRUE)

#generating the plot
plot <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.8)) +
  theme_light(base_size=12) +
  xlab("Hour") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("# of Bike Rental") +
  scale_colour_gradientn("Temp(°C))", colours=c("#9400D3","#4B0082", "#0000FF", "#00FF00", "#FFFF00", "#FF7F00", "#FF0000")) +
  theme(plot.title=element_text(size=11))

ggsave("#ofbikerental_vs_time_temp.png", plot)

