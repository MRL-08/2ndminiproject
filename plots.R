##Mary Rose G.Liguan
#BS in Statistics-3
#CMSC197-3
#SecondMiniProjectInR(plots)


##After downloading  the zipped file household_power_consumption.zip from the LMS,Unzipping the file and creating a  directory 'specdata' in a local file, Change the working directory using setwd() function  by using the specific path where the downloaded and unzipped file is located in your local computer. 
setwd("C:/Users/ASUS/Desktop/specdata")

##Read the file household_power_consumption.txt in table format and creating a data frame from it using read.table() and assign it to a variable named housedat.Missing values are coded as ?.
housedat <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

##Extracting data of housedat according to dates and choose the format using as.date()function
housedat$Date <- as.Date(housedat$Date, "%d/%m/%Y")

## Subsetting data from the dates 2007-02-01 and 2007-02-02 only using subset() and as.Date function and assign it to a variable named housedat
housedat <- subset(housedat,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

##return logical vectors with cases using complete.cases() function and assign it to a variable name housedat
housedat <- housedat[complete.cases(housedat),]

##Combine Date and Time using paste() function and assign it to a variable named DateTime 
DateTime <- paste(housedat$Date, housedat$Time)

##Set the name of vector DateTime as "DateTime" using setNames() functions and assign it to a variable named DateTime
DateTime <- setNames(DateTime, "DateTime")

##Remove Date and Time and add the DateTime column using cbind() function 
housedat <- housedat[ ,!(names(housedat) %in% c("Date","Time"))]
housedat <- cbind(DateTime, housedat)

##Format the DateTime column of housedat
housedat$DateTime <- as.POSIXct(DateTime)

##Create a histogram using Global_active_power of housedat as x-axis with a main title Global Active Power and assign red as color
hist(housedat$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

##Save file as plot1.png using dev.copy() function
dev.copy(png,"plot1.png", width=480, height=480)

##Close the devise using de.off() function
dev.off()

##Create Plot 2 using the Global Active Power (kilowatts) as y-axis and use type 1 graph
plot(housedat$Global_active_power~housedat$DateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

##Save file as plot2.png using dev.copy() function
dev.copy(png,"plot2.png", width=480, height=480)
 
##Close the devise using de.off() function
 dev.off()


##Create Plot3
 with(housedat, {
       plot(Sub_metering_1~DateTime, type="l",
                       ylab="Global Active Power (kilowatts)", xlab="")
       lines(Sub_metering_2~DateTime,col='Red')
       lines(Sub_metering_3~DateTime,col='Blue')
   })
 legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
                 c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

##Save file as png using dev.copy() function
dev.copy(png, file="plot3.png", height=480, width=480)

##Close the devise using de.off() function 
dev.off()

##Create Plot4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(housedat, {
       plot(Global_active_power~DateTime, type="l", 
                       ylab="Global Active Power (kilowatts)", xlab="")
       plot(Voltage~DateTime, type="l", 
                       ylab="Voltage (volt)", xlab="")
       plot(Sub_metering_1~DateTime, type="l", 
                       ylab="Global Active Power (kilowatts)", xlab="")
       lines(Sub_metering_2~DateTime,col='Red')
       lines(Sub_metering_3~DateTime,col='Blue')
      legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
                           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
       plot(Global_reactive_power~DateTime, type="l", 
                      ylab="Global Rective Power (kilowatts)",xlab="")
   })
##Save file as plot4.png using dev.copy() function
dev.copy(png, file="plot4.png", height=480, width=480)
##Close the devise using de.off() function 
dev.off()
