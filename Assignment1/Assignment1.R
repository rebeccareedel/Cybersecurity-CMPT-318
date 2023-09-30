# Group Project #24, Rebecca Reedel 301454910, Asmita Srivastava 301436340, Mrinal Goshalia
install.packages("corrplot")
install.packages("Hmisc")
library(corrplot)
library(Hmisc)
library(ggplot2)

###PART 1 

# mode function adapted from https://statisticsglobe.com/mode-in-r-programming-example
mode <- function(x) {                   
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

# read dataset file
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',' )
data$Date <- as.Date(data$Date, '%d/%m/%Y')
data = na.omit(data)
#print(data)

#extract data spanning one full week from Monday to Sunday. 
#The week assigned to your group is determined by your group number
#As per our group number, we extracted Week 24 of 2007 which is June 11-June 17
data = data[data$Date >='2007-06-11' & data$Date < '2007-06-18', ]
#print(data)


# 1. compute arithmetic and geometric mean, median, mode and standard dev. 
#for A, B, C features.  

A_a_mean <-mean(data$Global_active_power)
A_g_mean <-exp(mean(log(data$Global_active_power)))
A_median <-median(data$Global_active_power)
A_mode <-mode(data$Global_active_power)
A_std <-sd(data$Global_active_power)

B_a_mean <-mean(data$Global_reactive_power)
B_g_mean <-exp(mean(log(data$Global_reactive_power)))
B_median <-median(data$Global_reactive_power)
B_mode <-mode(data$Global_reactive_power)  
B_std <-sd(data$Global_reactive_power)

C_a_mean <-mean(data$Voltage)
C_g_mean <-exp(mean(log(data$Voltage)))
C_median <-median(data$Voltage)
C_mode <-mode(data$Voltage) 
C_std <-sd(data$Voltage)

print(A_a_mean)
print(A_g_mean)
print(A_median)
print(A_mode)
print(A_std)

print(B_a_mean)
print(B_g_mean)
print(B_median)
print(B_mode)
print(B_std)

print(C_a_mean)
print(C_g_mean)
print(C_median)
print(C_mode)
print(C_std)

#In order to extract specific days from a time series you will need this command:
#code below is adapted from https://stackoverflow.com/questions/54163708/how-to-create-a-day-night-factor-from-posixct-variable
data$week = strftime(data$Date, format = "%a")
data$daynight = with(data, ifelse(data$Time > "060000" & data$Time < "200000", "Day", "Night")) 
print(data)

#values on weekdays and weekend days during day hours and night hours
data_weekdays = data[data$week != 'Sat' & data$week != 'Sat', ]
data_weekends = data[data$week == 'Sun' | data$week == 'Sat', ]

day_week = data_weekdays[data_weekdays$daynight == 'Day', ]
night_week = data_weekdays[data_weekdays$daynight == 'Night', ]

day_wkend = data_weekends[data_weekends$daynight == 'Day', ]
night_wkend = data_weekends[data_weekends$daynight == 'Night', ]
print(day_week)

#For features A and B compute the min and max
min_A_weekdays_day = min(day_week$Global_active_power)
print(min_A_weekdays_day)
min_A_weekdays_night = min(night_week$Global_active_power)
print(min_A_weekdays_night)
max_A_weekdays_day = max(day_week$Global_active_power)
print(max_A_weekdays_day)
max_A_weekdays_night = max(night_week$Global_active_power)
print(max_A_weekdays_night)

min_A_weekends_day = min(day_wkend$Global_active_power)
print(min_A_weekends_day)
min_A_weekends_night = min(night_wkend$Global_active_power)
print(min_A_weekends_night)
max_A_weekends_day = max(day_wkend$Global_active_power)
print(max_A_weekends_day)
max_A_weekends_night = max(night_wkend$Global_active_power)
print(max_A_weekends_night)

min_B_weekdays_day = min(day_week$Global_reactive_power)
print(min_B_weekdays_day)
min_B_weekdays_night = min(night_week$Global_reactive_power)
print(min_B_weekdays_night)
max_B_weekdays_day = max(day_week$Global_reactive_power)
print(max_B_weekdays_day)
max_B_weekdays_night = max(night_week$Global_reactive_power)
print(max_B_weekdays_night)

min_B_weekends_day = min(day_wkend$Global_reactive_power)
print(min_B_weekends_day)
min_B_weekends_night = min(night_wkend$Global_reactive_power)
print(min_B_weekends_night)
max_B_weekends_day = max(day_wkend$Global_reactive_power)
print(max_B_weekends_day)
max_B_weekends_night = max(night_wkend$Global_reactive_power)
print(max_B_weekends_night)



### PART 2

# Calculating the correlation coefficient
A_val = (data$Global_active_power)
B_val = (data$Global_reactive_power)
C_val = (data$Voltage)
D_val = (data$Global_intensity)
E_val = (data$Sub_metering_1)
F_val = (data$Sub_metering_2)
G_val = (data$Sub_metering_3)

# store all values into a dataframe
df <- data.frame(A=c(A_val),
                 B=c(B_val),
                 C= c(C_val),
                 D=c(D_val),
                 E=c(E_val),
                 F=c(F_val),
                 G=c(G_val))

# find the correlation coefficients for each disjoint pair 
cor(df)

# display the correlation coefficients as disjoint pairs 
rcorr(as.matrix(df))

# plot 
corrplot(cor(df))


###PART3

#Create 4 time series with averaged Global_intensity values over grouped times.
#Code for the following aggregate function is adapted from https://statisticsglobe.com/r-sum-by-group-example

grouped_weekday = aggregate(x = day_week$Global_intensity,                # Specify data column
                            by = list(day_week$Time),              # Specify group indicator
                            FUN = mean) 


grouped_weeknight = aggregate(x = night_week$Global_intensity,                # Specify data column
                              by = list(night_week$Time),              # Specify group indicator
                              FUN = mean) 


grouped_wkendday = aggregate(x = day_wkend$Global_intensity,                # Specify data column
                             by = list(day_wkend$Time),              # Specify group indicator
                             FUN = mean) 


grouped_wkendnight = aggregate(x = night_wkend$Global_intensity,                # Specify data column
                               by = list(night_wkend$Time),              # Specify group indicator
                               FUN = mean) 

# convert Group.1 to a time format so it can be used for plotting
grouped_weekday$Group.1 <- as.POSIXct(grouped_weekday$Group.1, format = "%H:%M:%S")
grouped_weeknight$Group.1 <- as.POSIXct(grouped_weeknight$Group.1, format = "%H:%M:%S")
grouped_wkendday$Group.1 <- as.POSIXct(grouped_wkendday$Group.1, format = "%H:%M:%S")
grouped_wkendnight$Group.1 <- as.POSIXct(grouped_wkendnight$Group.1, format = "%H:%M:%S")

# linear regression for each time series with LSM
linear_weekday <- lm(x ~ Group.1, data=grouped_weekday)
#plot(grouped_weekday$Group.1, linear_weekday$residuals)
#plot(grouped_weekday$Group.1, grouped_weekday$x)
#abline(linear_weekday)
linear_weeknight <- lm(x ~ Group.1, data=grouped_weeknight)
linear_wkendday <- lm(x ~ Group.1, data=grouped_wkendday)
linear_wkendnight <- lm(x ~ Group.1, data=grouped_wkendnight)

ggplot() +
  geom_smooth(aes(x = Group.1, y = x), data = grouped_weekday, 
              method = "lm", se = FALSE, color = "red") + 
  geom_smooth(aes(x = Group.1, y = x), data = grouped_weeknight, 
              method = "lm", se = FALSE, color = "blue") + 
  geom_smooth(aes(x = Group.1, y = x), data = grouped_wkendday, 
              method = "lm", se = FALSE, color = "purple") + 
  geom_smooth(aes(x = Group.1, y = x), data = grouped_wkendnight, 
              method = "lm", se = FALSE, color = "green") + 
  geom_point(aes(x = Group.1, y = x), data = grouped_weekday, color = "red") + 
  geom_point(aes(x = Group.1, y = x), data = grouped_weeknight, color = "blue")+
  geom_point(aes(x = Group.1, y = x), data = grouped_wkendday, color = "purple") +
  geom_point(aes(x = Group.1, y = x), data = grouped_wkendnight, color = "green")+
  labs(title="Linear Regression -> Time vs. Global Intensity", x = "Time", y = "Global Intensity")+
  scale_color_manual(values = c("Week Day" = "red", "Week Night" = "blue", "Weekend Day" = "purple", "Weekend Night" = "green"))


# create the combined plot for all linear fits
ggplot() +
  geom_line(data=grouped_weekday, aes(x =Group.1, y = x, color = "Week Day")) +
  geom_line(data =grouped_weeknight, aes(x =Group.1, y = x, color = "Week Night")) +
  geom_line(data =grouped_wkendday, aes(x =Group.1, y = x, color = "Weekend Day")) +
  geom_line(data =grouped_wkendnight, aes(x =Group.1, y = x, color = "Weekend Night")) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title="Linear Regression -> Time vs. Global Intensity", x = "Time", y = "Global Intensity") +
  scale_color_manual(values = c("Week Day" = "red", "Week Night" = "blue", "Weekend Day" = "purple", "Weekend Night" = "green"))
   

# polynomial regression for each time series 
degree <- 3 # modify to get nicer fit
poly_weekday <- lm(x ~ poly(Group.1, degree, raw=TRUE), data=grouped_weekday)
poly_weeknight <- lm(x ~ poly(Group.1, degree, raw=TRUE), data=grouped_weeknight)
poly_wkendday <- lm(x ~ poly(Group.1, degree, raw=TRUE), data=grouped_wkendday)
poly_wkendnight <- lm(x ~ poly(Group.1, degree, raw=TRUE), data=grouped_wkendnight)

# create the combined plot for all poly fits
ggplot() +
  geom_line(data=grouped_weekday, aes(x =Group.1, y = x, color = "Week Day")) +
  geom_line(data =grouped_weeknight, aes(x =Group.1, y = x, color = "Week Night")) +
  geom_line(data =grouped_wkendday, aes(x =Group.1, y = x, color = "Weekend Day")) +
  geom_line(data =grouped_wkendnight, aes(x =Group.1, y = x, color = "Weekend Night")) +
  geom_point() +
  stat_smooth(method = "lm", formula = y~poly(x,2), se = FALSE) +
  labs(title="Polynomial Regression-> Time vs. Global Intensity", x = "Time", y = "Global Intensity")+
  scale_color_manual(values = c("Week Day" = "red", "Week Night" = "blue", "Weekend Day" = "purple", "Weekend Night" = "green"))

########################### ADDED STUFF
# Load the necessary libraries
library(ggplot2)
library(gridExtra)

# Define a function for linear regression
perform_linear_regression <- function(data, title) {
  linear_model <- lm(x ~ Group.1, data = data)
  
  # Create a ggplot for the linear regression
  linear_plot <- ggplot(data, aes(x = Group.1, y = x)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = title, x = "Time", y = "Global Intensity")
  
  return(list(model = linear_model, plot = linear_plot))
}

# Define a function for polynomial regression
perform_polynomial_regression <- function(data, degree, title) {
  poly_model <- lm(x ~ poly(Group.1, degree, raw = TRUE), data = data)
  
  # Create a ggplot for the polynomial regression
  poly_plot <- ggplot(data, aes(x = Group.1, y = x))+
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, degree), se = FALSE, color = "red") +
    labs(title = title, x = "Time", y = "Global Intensity")
  
  return(list(model = poly_model, plot = poly_plot))
}

# Perform linear regression for each time series
linear_weekday_result <- perform_linear_regression(grouped_weekday, "Linear Regression - Weekday")
linear_weeknight_result <- perform_linear_regression(grouped_weeknight, "Linear Regression - Weeknight")
linear_wkendday_result <- perform_linear_regression(grouped_wkendday, "Linear Regression - Weekend Day")
linear_wkendnight_result <- perform_linear_regression(grouped_wkendnight, "Linear Regression - Weekend Night")

# Perform polynomial regression for each time series
degree <- 5  # Modify the degree as needed
poly_weekday_result <- perform_polynomial_regression(grouped_weekday, degree, "Polynomial Regression - Weekday")
poly_weeknight_result <- perform_polynomial_regression(grouped_weeknight, degree, "Polynomial Regression - Weeknight")
poly_wkendday_result <- perform_polynomial_regression(grouped_wkendday, degree, "Polynomial Regression - Weekend Day")
poly_wkendnight_result <- perform_polynomial_regression(grouped_wkendnight, degree, "Polynomial Regression - Weekend Night")

# Combine all plots into one grid
linear_plots <- grid.arrange(
  linear_weekday_result$plot,
  linear_weeknight_result$plot,
  linear_wkendday_result$plot,
  linear_wkendnight_result$plot,
  ncol = 2
)

poly_plots <- grid.arrange(
  poly_weekday_result$plot,
  poly_weeknight_result$plot,
  poly_wkendday_result$plot,
  poly_wkendnight_result$plot,
  ncol = 2
)

# Print the combined plots
print(linear_plots)
print(poly_plots)

