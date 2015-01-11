# Plot1: Combine Multi Plots on the same Graph

#step 1: set working directory 

setwd("D:/Coursera/Data_Science/Exploratory Data Analysis/Project Course 1")

# Step 2: Load file in dataset "raw_data"

raw_data<-read.table("household_power_consumption.txt",sep=";",
                     na.strings="?",stringsAsFactors=FALSE, header=TRUE)
dim(raw_data)
raw_data$Date<-as.Date(raw_data$Date,"%d/%m/%Y")

# Step 3: Creat dataset "data" for plotting Histogram by subsetting raw_data

date_begin<-as.Date("1/2/2007","%d/%m/%Y") # Beginning Date of the period to get data
date_end<-as.Date("2/2/2007","%d/%m/%Y")   # Ending Date of the period to get data

data<-raw_data[raw_data$Date >= date_begin & raw_data$Date <= date_end,] # Subsetting from raw_data
dim(data)

ymd_hms<-paste(data$Date,data$Time)   # Create vector of character in format "d/m/y h:m:s"
data$Date_Time<-strptime(ymd_hms,"%Y-%m-%d %H:%M:%S") # Create new colume Date_Time
dim(data)

# Step 4: Open a Graphics png File Device

png(filename = "plot4.png", 
    width = 480, height = 480, 
    units = "px", bg = "white")

# Step 5: Combine Multi Plots on png Device

# Firstly, set matrix of plots 2x2

par(mfrow = c(2, 2))

# Secondly, Plot non-NA Time Series of 
# variables Global_active_power at the top-left

plot(data$Date_Time[!is.na(data$Date_Time)], 
   data$Global_active_power[!is.na(data$Global_active_power)], 
   type = "l",
   xlab = "",
   ylab = "Global Active Power (kilowatts)")

# Thirdly, Plot non-NA Time Series of 
# variables Voltage at the top-right

plot(data$Date_Time[!is.na(data$Date_Time)], 
     data$Voltage[!is.na(data$Voltage)], 
     type = "l",
     xlab = "datetime",
     ylab = "Voltage")

# Fourthly, Plot Multi non-NA Time Series of 
# variables Sub_metering_1,Sub_metering_2
# and Sub_metering_3 on the same Graph at the bottom-left

plot(data$Date_Time[!is.na(data$Date_Time)], 
     data$Sub_metering_1[!is.na(data$Sub_metering_1)], 
     type = "l",
     col = "black",
     xlab = "", ylab = "Energy sub metering")

lines(data$Date_Time[!is.na(data$Date_Time)],
      data$Sub_metering_2[!is.na(data$Sub_metering_2)], col = "red")

lines(data$Date_Time[!is.na(data$Date_Time)],
      data$Sub_metering_3[!is.na(data$Sub_metering_3)], col = "blue")

legend("topright", 
       col = c("black", "red", "blue"),
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lwd = 1)

# Finally, Plot non-NA Time Series of 
# variables Global_reactive_power at the bottom-right

plot(data$Date_Time[!is.na(data$Date_Time)], 
     data$Global_reactive_power[!is.na(data$Global_reactive_power)], 
     type = "l",
     xlab = "datetime",
     ylab = "Global_reactive_power")

dev.off()