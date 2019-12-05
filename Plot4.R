#read data
data<-read.table("household_power_consumption.txt", sep=";", header = TRUE,na.strings = "?")
#establish timespan
timespan<-subset(data,data$Date=="2/2/2007"|data$Date=="1/2/2007")
# designing plot 4
GAP<-as.numeric(timespan$Global_active_power)
Daytime<-strptime(paste(timespan$Date, timespan$Time, sep = ""),"%d/%m/%Y %H:%M:%S")
VTG<-as.numeric(timespan$Voltage)
SM1<-as.numeric(timespan$Sub_metering_1)
SM2<-as.numeric(timespan$Sub_metering_2)
SM3<-as.numeric(timespan$Sub_metering_3)
GRP<-as.numeric(timespan$Global_reactive_power)
#graph 4 as required
png("plot4.png", width = 480, height = 480)
par(mfrow=c(2,2))
plot(Daytime,GAP, type = "l", xlab = "", ylab = "Global Active Power")
plot(Daytime,VTG, type = "l", xlab = "datetime", ylab = "Voltage")
plot(Daytime,SM1, type = "l", ylab = "Energy Submetering", xlab = "")
lines(Daytime,SM2, type = "l", col="red")
lines(Daytime,SM3, type = "l", col="blue")
legend("topright", c("Sub_metering_1","Sub_metering_2", "Sub_metering_3" ), lty=1, lwd = 1, col=c("black","red","blue"))
plot(Daytime,GRP, type="l", xlab = "datetime", ylab = "Global_reactive_power")
dev.off()