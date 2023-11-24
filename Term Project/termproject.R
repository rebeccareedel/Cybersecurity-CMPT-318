# CMPT318 Term Project Part 1
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(depmixS4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggbiplot)

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
#scaled_data = na.omit(scaled_data)

# For deciding on the subset of variables that are most suitable for training your models,
# you need to perform a Principal Component Analysis (PCA)
response_variables = scaled_data[c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
pca = prcomp(response_variables, center = TRUE, scale. = FALSE)
summary(pca)
#ggbiplot(pca)

# Plot the PCA results using ggbiplot
ggbiplot(pca,
         choices = c(1,2),
         obs.scale = 1, var.scale = 1,  # Scaling of axis
         labels.size = 4,
         varname.size = 5,
         varname.abbrev = TRUE,  # Abbreviate variable names (TRUE)
         var.axes = FALSE,      # Remove variable vectors (TRUE)
         circle = FALSE,        # Add unit variance circle (TRUE)
         ellipse = TRUE) # Adding ellipses

# Choose a subset of the response variables for training of multivariate
subset = scaled_data[c("Date", "Time", "weekday", "weekno", "Global_active_power", "Global_reactive_power", "Voltage")]

# HMMs on normal electricity consumption data. 

# Provide a 1 proper rational for your final choice of response variables based on your PCA results. 
#PC1 + PC2 = 70% of data representation (approximately), plus choosing PC3 = 90% of all the data representation.

# PART 2. HMM TRAINING AND TESTING

# Choose a weekday or a weekend day and a time window between 2 to 6 hours on that day. 
data_wedall = subset[subset$weekday == 'Wed',] #154 wednesdays between 2006 and 2009
time_window = data_wedall[data_wedall$Time >= "2023-11-23 00:00:00" & data_monall$Time < "2023-11-23 04:00:00", ]

time_window = subset(time_window, select = c(Date,Time, weekno, Global_active_power, Global_reactive_power, Voltage)) # MODIFY WITH PCA RESP VARIABLES
time_window = na.omit(time_window)
#nrow(time_window)
# Partition your scaled data into train and test. 
#Test-Train-Split Code adapted from https://www.statology.org/train-test-split-r/
#use 60% of dataset as training set and 40% as test set
#make this example reproducible
set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(time_window), replace=TRUE, prob=c(0.7,0.3))
train  <- time_window[sample, ]
test   <- time_window[!sample, ]

# Partition your scaled data into train and test. 

# For this time window, train various multivariate Hidden Markov Models on the train data with different
# numbers of states. For models with at least 4 and not more than 24 states -- not for every number of states
#n = rep(c(240),each=108)
n_times = aggregate(Time ~ Date, train, FUN = length)$Time

model1 <- depmix(response = Global_active_power + Global_reactive_power + Voltage ~ 1, data = train, nstates = 4, ntimes = n_times)
fit1 <- fit(model1)
summary(fit1)

model2 <- depmix(response = Global_active_power + Global_reactive_power + Voltage ~ 1, data = train, nstates = 8, ntimes = n_times)
fit2 <- fit(model2)
summary(fit2)

model3 <- depmix(response = Global_active_power + Global_reactive_power + Voltage ~ 1, data = train, nstates = 12, ntimes = n)
fit3 <- fit(model3)
summary(fit3)

model4 <- depmix(response = Global_active_power + Global_reactive_power + Voltage ~ 1, data = train, nstates = 16, ntimes = n)
fit4 <- fit(model4)
summary(fit4)

# For each HMM, compute the log-likelihood measure on the training dataset
print(logLik(fit1))
print(logLik(fit2))
print(logLik(fit3))
print(logLik(fit4))

# for each HMM, compute the Bayesian information criterion (BIC) = measure of complexity of model
print(BIC(fit1))
print(BIC(fit2))
print(BIC(fit3))
print(BIC(fit4))

# compare the results of log-likelihood and BIC to select the ‘best performing’ model(s) with
# an overall good fit on the train data. 
# NOTE: want highest log-like and lowest BIC

# make HMM with n_states that was best performing -> with test data

# Finally, calculate the log-likelihood of the test data for your selected models to decide on the best one 
# NOTE: that you need to compare normalized log-likelihood of the train data and the test data.

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
