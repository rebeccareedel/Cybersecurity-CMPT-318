# Group Project #24, Rebecca Reedel 301454910, Asmita Srivastava, Mrinal Goshalia

# read dataset file
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',' )
print(data)

#extract data spanning one full week from Monday to Sunday. 
#The week assigned to your group is determined by your group number
#data = filter(data, Date =='')

# 1. compute arithmetic and geometric mean, median, mode and standard dev. 
#for A, B, C features. For features A and B compute the min and max
#values on weekdays and weekend days during day hours and night hours 

#The arithmetic mean (AM) = (x₁ + x₂ + x₃ + ... + xₙ) / n.
#The geometric mean (GM) = (x₁ · x₂ · x₃ · ... · xₙ)1/n.
A_a_mean <-mean(data$Global_active_power)
A_g_mean <-exp(mean(log(data$Global_active_power)))
A_median <-median(data$Global_active_power)
#A_mode <-mode(data$Global_active_power) no built in 
A_std <-sd(data$Global_active_power)

B_a_mean <-mean(data$Global_reactive_power)
B_g_mean <-exp(mean(log(data$Global_reactive_power)))
B_median <-median(data$Global_reactive_power)
#B_mode <-mode(data$Global_reactive_power) no built in 
B_std <-sd(data$Global_reactive_power)

C_a_mean <-mean(data$Voltage)
C_g_mean <-exp(mean(log(data$Voltage)))
C_median <-median(data$Voltage)
#C_mode <-mode(data$Voltage) no built in 
C_std <-sd(data$Voltage)

print(A_a_mean)
print(A_g_mean)
print(A_median)
#print(A_mode)
print(A_std)

print(B_a_mean)
print(B_g_mean)
print(B_median)
#print(B_mode)
print(B_std)

print(C_a_mean)
print(C_g_mean)
print(C_median)
#print(C_mode)
print(C_std)


#In order to extract specific days from a time series you will need this command:
  #as.POSIXlt(date, format = ""

