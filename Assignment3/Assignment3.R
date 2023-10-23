#CMPT318 Assignment 3
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)

data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 
#TODO: scale data, data needs to be numeric first
data$week = strftime(data$Date, format = "%a")
# Group data by time windows and calculate mean of Global Intensity
data = data %>%
  group_by(week, Time)

print(data)
n = rep(c(120),each=52)
model <- depmix(response = Average_Global_intensity, data = grouped_data, n_states = n)


