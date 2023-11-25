# CMPT318 Term Project Part 1
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
install.packages("devtools")
install.packages("factoextra")
library(devtools)
install_github("vqv/ggbiplot")
library(depmixS4)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggbiplot)
library(factoextra)

# read table
data = read.table('TermProjectData.txt', header =TRUE, sep =',') 

# PART 1. FEATURE ENGINEERING

# convert data to be numeric
data$Date = as.Date(data$Date, '%d/%m/%Y')
data$Time = as.POSIXct(data$Time, format = '%H:%M:%S')
data$weekno = strftime(data$Date, format = "%V")
data$weekday = strftime(data$Date, format = "%a")

# scale the raw data using standardization prior to applying PCA.
scaled_data = data %>% mutate(across(where(is.numeric), scale))
scaled_data = na.omit(scaled_data)

# perform a Principal Component Analysis (PCA)
response_variables = scaled_data[c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
pca = prcomp(response_variables, center = TRUE, scale. = FALSE)
summary(pca)

# code to get contribution factor table. 
var = get_pca_var(pca)
head(var$contrib, 4)

# code to get percentage involved plot
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

# plots the biplot of the PCA results -- basic version
# ggbiplot(pca)

# Plot the PCA results using biplot
#ggbiplot(pca,
        # choices = c(1,2),
       #  obs.scale = 1, var.scale = 1,  # Scaling of axis
        # labels.size = 4,
        # varname.size = 5,
        # varname.abbrev = TRUE,  # Abbreviate variable names (TRUE)
         #var.axes = FALSE,      # Remove variable vectors (TRUE)
         #circle = FALSE,        # Add unit variance circle (TRUE)
         #ellipse = TRUE) # Adding ellipses

# subset of the response variables for training of HMM on normal electricity consumption
subset = scaled_data[c("Date", "Time", "weekday", "weekno", "Global_active_power", "Global_reactive_power", "Global_intensity")]

# Provide a 1 proper rational for your final choice of response variables based on your PCA results. 
#PC1 + PC2 = 70% of data representation (approximately), plus choosing PC3 = 90% of all the data representation.


# PART 2. HMM TRAINING AND TESTING

# Choose a weekday or a weekend day and a time window between 2 to 6 hours on that day. 
data_wedall = subset[subset$weekday == 'Wed',] #154 wednesdays between 2006 and 2009
time_window = data_wedall[data_wedall$Time >= "2023-11-25 00:00:00" & data_wedall$Time < "2023-11-25 04:00:00", ] # MODIFY FOR DATE U RUN
time_window = subset(time_window, select = c(Date,Time, weekno, Global_active_power, Global_reactive_power, Global_intensity)) 
time_window = na.omit(time_window)

# Test-Train-Split Code adapted from https://www.statology.org/train-test-split-r/
# use 70% of dataset as training set and 30% as test set
set.seed(1) # make this example reproducible

sample <- sample(c(TRUE, FALSE), nrow(time_window), replace=TRUE, prob=c(0.7,0.3))
train  <- time_window[sample, ]
test   <- time_window[!sample, ]

# For this time window, train various multivariate Hidden Markov Models on the train data with different
# numbers of states. For models with at least 4 and not more than 24 states -- not for every number of states
n_times = aggregate(Time ~ Date, train, FUN = length)$Time

model1 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 4, ntimes = n_times)
fit1 <- fit(model1)
summary(fit1)

model2 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 8, ntimes = n_times)
fit2 <- fit(model2)
summary(fit2)

model3 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 12, ntimes = n_times)
fit3 <- fit(model3)
summary(fit3)

model4 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 16, ntimes = n_times)
fit4 <- fit(model4)
summary(fit4)

model5 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 20, ntimes = n_times)
fit5 <- fit(model5)
summary(fit5)

model6 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 24, ntimes = n_times)
fit6 <- fit(model6)
summary(fit6)

#closely inspect number_states to enhance model performance 
model7 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 18, ntimes = n_times)
fit7 <- fit(model7)
summary(fit7)

model8 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 19, ntimes = n_times)
fit8 <- fit(model8)
summary(fit8)

model9 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 21, ntimes = n_times)
fit9 <- fit(model9)
summary(fit9)

model10 <- depmix(response = Global_active_power + Global_reactive_power + Global_intensity ~ 1, data = train, nstates = 22, ntimes = n_times)
fit10 <- fit(model10)
summary(fit10)

# For each HMM, compute the log-likelihood measure on the training dataset
print(logLik(fit1))
print(logLik(fit2))
print(logLik(fit3))
print(logLik(fit4))
print(logLik(fit5))
print(logLik(fit6))
print(logLik(fit7))
print(logLik(fit8))
print(logLik(fit9))
print(logLik(fit10))

# for each HMM, compute the Bayesian information criterion (BIC) = measure of complexity of model
print(BIC(fit1))
print(BIC(fit2))
print(BIC(fit3))
print(BIC(fit4))
print(BIC(fit5))
print(BIC(fit6))
print(BIC(fit7))
print(BIC(fit8))
print(BIC(fit9))
print(BIC(fit10))

# compare the results of log-likelihood and BIC to select the ‘best performing’ model(s) with
# an overall good fit on the train data. 
# NOTE: want highest log-like and lowest BIC

# make HMM with n_states that was best performing -> with test data

# Finally, calculate the log-likelihood of the test data for your selected models to decide on the best one 
# NOTE: that you need to compare normalized log-likelihood of the train data and the test data.

# Hint: For calculating the log-likelihood of the test data, look at the fit-section on Page 15
# and the forwardbackward-section on Page 21.


# PART 3. ANOMALY DETECTION

# get all 3 injected anomalous data sets
anom_data1 = read.table('DataWithAnomalies1.txt', header =TRUE, sep =',') 
anom_data2 = read.table('DataWithAnomalies2.txt', header =TRUE, sep =',')
anom_data3 = read.table('DataWithAnomalies3.txt', header =TRUE, sep =',')

# filter data -- so only instances of our time frame
anom_data1 = anom_data1[anom_data1$weekday == 'Wed',]
anom_data1 = anom_data1[anom_data1$Time >= "2023-11-25 00:00:00" & anom_data1$Time < "2023-11-24 04:00:00", ] 
anom_data1 = subset(anom_data1, select = c(Date,Time, Global_active_power, Global_reactive_power, Global_intensity)) 
anom_data1 = na.omit(anom_data1)

anom_data2 = anom_data2[anom_data2$weekday == 'Wed',]
anom_data2 = anom_data2[anom_data2$Time >= "2023-11-25 00:00:00" & anom_data2$Time < "2023-11-24 04:00:00", ] 
anom_data2 = subset(anom_data2, select = c(Date,Time, Global_active_power, Global_reactive_power, Global_intensity)) 
anom_data2 = na.omit(anom_data2)

anom_data3 = anom_data3[anom_data3$weekday == 'Wed',]
anom_data3 = anom_data3[anom_data3$Time >= "2023-11-25 00:00:00" & anom_data3$Time < "2023-11-24 04:00:00", ] 
anom_data3 = subset(anom_data3, select = c(Date,Time, Global_active_power, Global_reactive_power, Global_intensity)) 
anom_data3 = na.omit(anom_data3)

# Using the above multivariate HMM, compute the log-likelihood in each of three
# datasets with injected anomalies that are provided on the course page under Term Project.

# compute the log-likelihood over all instances of the time window

# Compare and interpret the three datasets regarding the degree of
# anomalies present in each of the datasets in some detail.
