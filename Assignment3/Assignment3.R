# CMPT318 Assignment 3
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

# determine a time window for a specific weekday that shows a
#clearly recognizable electricity consumption pattern over a time period  >120 &<240 minutes. 
# for global_reactive_power


# Extract the same time window for each week of the dataset

# concatenate the extracted time windows to build a dataset for the training of HMMs

# train a number of univariate HMMs, with number of states >=3, <=16
# NOTE: You may not need to train HMMs for each and every number of states within the range by
# making smart choices.

# For each HMM, compute the log-likelihood measure on the training dataset

# for each HMM, compute the Bayesian information criterion (BIC) = measure of complexity of model

# goal is to find the intercept of the two plots for log-likelihood
# and BIC values respectively so as to determine the best model (avoiding overfitting).

n = rep(c(120),each=52)
model <- depmix(response = Average_Global_intensity, data = grouped_data, n_states = n)


