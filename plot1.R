library(readr)
household_power_consumption <- read_delim("~/R/Exploratory_Data/household_power_consumption.txt", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)
View(household_power_consumption)
start_date<-as.Date("01/02/2007","%d,%m,%Y")
end_date<-as.Date("02/02/2007","%d,%m,%Y")
all_dates<-as.Date(household_power_consumption$Date,"%d,%m,%Y")
subset<-household_power_consumption[all_dates>=start_date&all_dates<=end_date,]
daytime<-paste(subset$Date,subset$Time)
conversion<-strptime(daytime,format = "%d/%m/%Y %H:%M:%S")
hist(household_power_consumption$Global_active_power,col="red",main="global active power")
png("plot1.png",width = 480,height = 480)
