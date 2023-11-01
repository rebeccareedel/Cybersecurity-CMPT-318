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

# scale data
scaled_data = data %>% mutate(across(where(is.numeric), scale))
print(scaled_data)

data_mon = scaled_data[scaled_data$weekday == 'Mon' & scaled_data$weekno == '01', ]
data_monall = scaled_data[scaled_data$weekday == 'Mon', ]

plot(data_mon$Time, data_mon$Global_reactive_power)

plot(data_monall$Time, data_monall$Global_reactive_power)

# determine a time window for a specific weekday that shows a
#clearly recognizable electricity consumption pattern over a time period  >120 &<240 minutes. 
# for global_reactive_power
# Extract the same time window for each week of the dataset

time_window = data_monall[data_monall$Time >= "2023-11-01 10:00:00" & data_monall$Time < "2023-11-01 14:00:00", ]
time_window = subset(time_window, select = c(Time, Global_reactive_power))
time_window = na.omit(time_window)

# train a number of univariate HMMs, with number of states >=3, <=16
n = rep(c(240),each=52)
model1 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 4, ntimes = n)
fit1 <- fit(model1)
summary(fit1)

model2 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 8, ntimes = n)
fit2 <- fit(model2)
summary(fit2)

model3 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 12, ntimes = n)
fit3 <- fit(model3)
summary(fit3)

model4 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 16, ntimes = n)
fit4 <- fit(model4)
summary(fit4)

# NOTE: You may not need to train HMMs for each and every number of states within the range by
# making smart choices.

# For each HMM, compute the log-likelihood measure on the training dataset
print(logLik(fit1))
print(logLik(fit2))
print(logLik(fit3))
print(logLik(fit4))

# for each HMM, compute the Bayesian information criterion (BIC) = measure of complexity of model
# NOTE: want highest log-like and lowest BIC
print(BIC(fit1))
print(BIC(fit2))
print(BIC(fit3))
print(BIC(fit4))

# BIC and log-likelihood values for all fits
BIC_val <- c(BIC(fit1), BIC(fit2), BIC(fit3), BIC(fit4))
logLik_val <- c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4))

# plot BIC values
plot(1:4, BIC_val, type = "b", col = "blue", xlab = "Model", ylab = "BIC", ylim = range(c(BIC_values, logLik_values)))

# add log-likelihood values
lines(1:4, logLik_val, type = "b", col = "red", lty = 2)

legend("topright", legend = c("BIC", "Log-Likelihood"), col = c("blue", "red"), lty = c(1, 2))



