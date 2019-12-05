#Read file
house<-read.csv("household_power_consumption.txt", sep=";", header = TRUE,na.strings = "?")
#convert variables of time
start<-as.Date("01/02/2007","%d/%m/%Y")
end<-as.Date("02/02/2007","%d/%m/%Y")
alldates<-as.Date(house$Date,"%d/%m/%Y")
#subsetting
house_subset<-house[alldates>=start&alldates<=end,]
daytime<-paste(house_subset$Date,house_subset$Time)
#converting into date and time
converted_daytime<-strptime(daytime, format = "%d/%m/%Y %H:%M:%S")
#save file according to instructions
png("plot3.png",width = 480, height = 480)
#designing the graph 3
plot(converted_daytime,house_subset$Sub_metering_1, type = "l",xlab = "", ylab = "Energy submetering")
lines(converted_daytime,house_subset$Sub_metering_2, type = "l", col="red")
lines(converted_daytime,house_subset$Sub_metering_3, type = "l", col="blue")
legend("topright",legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty = 1:1,cex=0.8)
dev.off()