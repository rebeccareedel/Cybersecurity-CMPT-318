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
#data$Time = as.POSIXct(data$Time, format = '%H:%M:%S')
data$week = strftime(data$Date, format = "%V")
data = na.omit(data)

# scale data
scaled_data = data %>% mutate(across(where(is.numeric), scale))
print(scaled_data)

# determine a time window for a specific weekday that shows a
#clearly recognizable electricity consumption pattern over a time period  >120 &<240 minutes. 
# for global_reactive_power
grouped_data = scaled_data[scaled_data$Time > "183000" & scaled_data$Time < "210000", ]
grouped_data = grouped_data$Global_reactive_power
print(grouped_data)
grouped_data = aggregate(x = scaled_data$Global_reactive_power,                # Specify data column
                            by = list(scaled_data$week),              # Specify group indicator
                            FUN = mean) 

print(grouped_data)
# Extract the same time window for each week of the dataset

# concatenate the extracted time windows to build a dataset for the training of HMMs

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


