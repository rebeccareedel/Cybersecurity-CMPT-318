#CMPT318 Assignment 3
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)
library(dplyr)


data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 
print(data)
data$Time = as.POSIXct(data$Time, format = '%H:%M:%S')
data$week = strftime(data$Date, format = "%a")
data = na.omit(data)
#TODO: scale data, data needs to be numeric first
#data$Global_reactive_power <- as.numeric(data$Global_active_power)
#scaled_data <- scale(data$Global_reactive_power)
data[] = lapply(data, as.numeric)
scaled_data = scale(data, center = TRUE, scale = TRUE)
print(scaled_data)


# Group data by time windows and calculate mean of Global Intensity
grouped_data = scaled_data %>%
  group_by(week, Time)

print(grouped_data)
n = rep(c(120),each=52)
model <- depmix(response = Average_Global_intensity, data = grouped_data, n_states = n)


