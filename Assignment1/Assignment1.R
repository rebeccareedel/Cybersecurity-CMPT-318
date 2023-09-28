# Group Project #24, Rebecca Reedel 301454910, Asmita Srivastava 301436340, Mrinal Goshalia

# mode function adapted from https://statisticsglobe.com/mode-in-r-programming-example
mode <- function(x) {                   
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

# read dataset file
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',' )
data$Date <- as.Date(data$Date, '%d/%m/%Y')
#print(data)

#extract data spanning one full week from Monday to Sunday. 
#The week assigned to your group is determined by your group number
data = data[data$Date >='2007-06-11' & data$Date < '2007-06-18', ]
print(data)


# 1. compute arithmetic and geometric mean, median, mode and standard dev. 
#for A, B, C features.  

A_a_mean <-mean(data$Global_active_power)
A_g_mean <-exp(mean(log(data$Global_active_power)))
A_median <-median(data$Global_active_power)
A_mode <-mode(data$Global_active_power)
A_std <-sd(data$Global_active_power)

B_a_mean <-mean(data$Global_reactive_power, na.rm=TRUE)
B_g_mean <-exp(mean(log(data$Global_reactive_power), na.rm=TRUE))
B_median <-median(data$Global_reactive_power, na.rm=TRUE)
B_mode <-mode(data$Global_reactive_power)  
B_std <-sd(data$Global_reactive_power, na.rm=TRUE)

C_a_mean <-mean(data$Voltage, na.rm=TRUE)
C_g_mean <-exp(mean(log(data$Voltage), na.rm=TRUE))
C_median <-median(data$Voltage, na.rm=TRUE)
C_mode <-mode(data$Voltage) 
C_std <-sd(data$Voltage, na.rm=TRUE)

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
data$week = strftime(data$Date, format = "%a")
data$daynight = with(data, ifelse(data$Time > "053000" & data$Time < "200000", "Day", "Night")) 
print(data)



#values on weekdays and weekend days during day hours and night hours
data_weekdays = data[data$week != 'Sat' & data$week != 'Sat', ]
data_weekends = data[data$week == 'Sun' | data$week == 'Sat', ]
#code below is adapted from https://stackoverflow.com/questions/54163708/how-to-create-a-day-night-factor-from-posixct-variable
day_week = data_weekdays[data_weekdays$daynight == 'Day', ]
night_week = data_weekdays[data_weekdays$daynight == 'Night', ]

day_wkend = data_weekends[data_weekends$daynight == 'Day', ]
night_wkend = data_weekends[data_weekends$daynight == 'Night', ]
print(day_week)
#For features A and B compute the min and max
min_A_weekdays_day = min(day_week$Global_active_power)
min_A_weekdays_night = min(night_week$Global_active_power)
max_A_weekdays_day = max(day_week$Global_active_power)
max_A_weekdays_night = max(night_week$Global_active_power)

min_A_weekends_day = min(day_wkend$Global_active_power)
min_A_weekends_night = min(night_wkend$Global_active_power)
max_A_weekends_day = max(day_wkend$Global_active_power)
max_A_weekends_night = max(night_wkend$Global_active_power)

min_B_weekdays_day = min(day_week$Global_reactive_power, na.rm=TRUE)
min_B_weekdays_night = min(night_week$Global_reactive_power, na.rm=TRUE)
max_B_weekdays_day = max(day_week$Global_reactive_power, na.rm=TRUE)
max_B_weekdays_night = max(night_week$Global_reactive_power, na.rm=TRUE)

min_B_weekends_day = min(day_wkend$Global_reactive_power, na.rm=TRUE)
min_B_weekends_night = min(night_wkend$Global_reactive_power, na.rm=TRUE)
min_B_weekends_day = min(day_wkend$Global_reactive_power, na.rm=TRUE)
min_B_weekends_night = min(night_wkend$Global_reactive_power, na.rm=TRUE)


