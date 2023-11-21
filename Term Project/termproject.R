# CMPT318 Term Project Part 1
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)
library(dplyr)
library(ggplot2)

# read table
data = read.table('TermProjectData.txt', header =TRUE, sep =',') 

# PART 1. FEATURE ENGINEERING

# scale the raw data using standardization prior to applying PCA.
# convert data to be numeric
data$Date = as.Date(data$Date, '%d/%m/%Y')
data$Time = as.POSIXct(data$Time, format = '%H:%M:%S')
data$weekno = strftime(data$Date, format = "%V")
data$weekday = strftime(data$Date, format = "%a")

# scale data
scaled_data = data %>% mutate(across(where(is.numeric), scale))

#make a seperate data frame for numeric response variables to undergo PCA
scaled_data = scaled_data[c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
scaled_data = na.omit(scaled_data)
# For deciding on the subset of variables that are most suitable for training your models,
# you need to perform a Principal Component Analysis (PCA)
pca = prcomp(scaled_data, scale = TRUE)
summary(pca)
# Choose a subset of the response variables for training of multivariate
# HMMs on normal electricity consumption data. 

# Provide a 1 proper rational for your final choice of response variables based on your PCA results. 


# PART 2. HMM TRAINING AND TESTING

# Partition your scaled data into train and test. 

# Choose a weekday or a weekend day and a time window between 2 to 6 hours on that day. 

# For this time window, train various multivariate Hidden Markov Models on the train data with different
# numbers of states. For models with at least 4 and not more than 24 states -- not for every number of states

# compare the results of log-likelihood and BIC to select the ‘best performing’ model(s) with
# an overall good fit on the train data. 

# Finally, calculate the log-likelihood of the test data for your selected models to decide on the best one 

# Note that you need to compare normalized log-likelihood of the train data and the test data.
# Hint: For calculating the log-likelihood of the test data, look at the fit-section on Page 15
# and the forwardbackward-section on Page 21.


# PART 3. ANOMALY DETECTION

# Using the above multivariate HMM, compute the log-likelihood for the
# respective observation sequences associated with the same time window in each of three
# datasets with injected anomalies that are provided on the course page under Term Project.

# That is, for each dataset compute the log-likelihood over all instances of the time window
# over one full year. 

# Compare and interpret the three datasets regarding the degree of
# anomalies present in each of the datasets in some detail.
