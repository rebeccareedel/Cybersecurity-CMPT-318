#CMPT318 Assignment 3
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)
library(dplyr)

# read table
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 

# convert data to be numeric
data$Date = as.Date(data$Date, '%d/%m/%Y')
data$Time = as.POSIXct(data$Time, format = '%H:%M:%S')
data$week = strftime(data$Date, format = "%a")
data = na.omit(data)

# scale data
data[] = lapply(data, as.numeric)
scaled_data = scale(data, center = TRUE, scale = TRUE)
print(scaled_data)

n = rep(c(120),each=52)
model <- depmix(response = Average_Global_intensity, data = grouped_data, n_states = n)


