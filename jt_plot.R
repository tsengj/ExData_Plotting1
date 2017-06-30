#install.packages('dplyr')
library('dplyr')

#import table
if(!file.exists("exdata-data-household_power_consumption.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
  file <- unzip(temp)
  unlink(temp)
}

d1<- read.table(file, 
                sep=";",
                header = T,
                stringsAsFactors = F)

#clean up files
file.remove('household_power_consumption.txt')


#format dataframe

d1 <- d1 %>% mutate(DTime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M"),
                    Date = as.Date(Date,format="%d/%m/%Y"),
                    Time = as.POSIXct(Time, format="%H:%M:%S"),
                    Global_active_power = as.numeric(Global_active_power),
                    Global_reactive_power = as.numeric(Global_reactive_power),
                    Voltage = as.numeric(Voltage),
                    Global_intensity = as.numeric(Global_intensity),
                    Sub_metering_1 = as.numeric(Sub_metering_1),
                    Sub_metering_2 = as.numeric(Sub_metering_2),
                    Sub_metering_3 = as.numeric(Sub_metering_3)) %>% 
  filter(Date >= "2007-02-01" & Date <= "2007-02-02")

#plot 1
plot1 <- function() {
  hist(d1$Global_active_power,
       col='red',
       xlab="Global Active Power (kw)",
       ylab="Frequency",
       main = "Global Active Power")
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off()
  cat("Plot1.png has been saved in", getwd())
}
plot1()

plot1 <- function() {
  plot(df$timestamp,df$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.copy(png, file="plot2.png", width=480, height=480)
  dev.off()
  cat("plot2.png has been saved in", getwd())
}
plot1()
