# CMPT318 Assignment 3
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava (Student No. 301436340)
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)
library(dplyr)

# read table
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 

# convert data to be numeric
data$Date = as.Date(data$Date, '%d/%m/%Y')
data$Time = as.POSIXct(data$Time, format = '%H:%M:%S')
data$weekno = strftime(data$Date, format = "%V")
data$weekday = strftime(data$Date, format = "%a")
data = na.omit(data)

# scale data
scaled_data = data %>% mutate(across(where(is.numeric), scale))
print(scaled_data)

data_sat = scaled_data[scaled_data$weekday == 'Sat' & scaled_data$weekno == '01', ]
data_satall = scaled_data[scaled_data$weekday == 'Sat', ]

plot(data_sat$Time, data_sat$Global_reactive_power)

plot(data_satall$Time, data_satall$Global_reactive_power)

# determine a time window for a specific weekday that shows a
#clearly recognizable electricity consumption pattern over a time period  >120 &<240 minutes. 
# for global_reactive_power
# Extract the same time window for each week of the dataset

time_window = data_satall[data_satall$Time >= "2023-11-01 00:00:00" & data_satall$Time < "2023-11-01 04:00:00", ]

# train a number of univariate HMMs, with number of states >=3, <=16
# NOTE: You may not need to train HMMs for each and every number of states within the range by
# making smart choices.

# For each HMM, compute the log-likelihood measure on the training dataset

# for each HMM, compute the Bayesian information criterion (BIC) = measure of complexity of model
# NOTE: want highest log-like and lowest BIC

# goal is to find the intercept of the two plots for log-likelihood
# and BIC values respectively so as to determine the best model (avoiding overfitting).

n = rep(c(120),each=52)
model <- depmix(response = Average_Global_intensity, data = grouped_data, n_states = n)


