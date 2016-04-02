# Project: Bike Share Rental
# Author : Shrirang Adgaonkar
# Date   : 11-11-2015
# Bike rentals vs weekdays

# loading files  CHANGE AS PER REQUIREMENT
train <- read.csv("~/Desktop/train.csv")

#include libraries
library(ggplot2)
library(lubridate)
library(readr)
library(scales)

#pre processing data
train$hour  <- hour(ymd_hms(train$datetime))
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$day   <- wday(ymd_hms(train$datetime), label=TRUE)

#plotting the graph
plot <- ggplot(train, aes(x=times, y=count, color=day)) +
  geom_smooth(ce=FALSE, fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Hour") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("# of Bike Rentals") +
  scale_color_manual(values = c("red","green","blue","cyan","orange","magenta","yellow")) +
  ggtitle("People rent bikes commutes on weekdays and weekends\n") +
  theme(plot.title=element_text(size=12))

ggsave("#bikerental_day.png")

