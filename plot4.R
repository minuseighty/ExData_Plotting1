plot4 <- function() {
    
    
    
    rData <- file("household_power_consumption.txt", open = "rt") ##raw data
    x <- TRUE ##initialize 
    ## while x is true search for the exact line in txt file and start from there
    while(x) {
        x <- !grepl("31/1/2007;23:59:00;0.326;0.126;242.800;1.400;0.000;0.000;0.000", readLines(rData,n=1))
    }
    tData <- read.table(rData, header = FALSE, sep = ";", nrows = 2880, na.strings = "?")
    colnames(tData) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
        
    tData$Date <- as.Date(tData$Date, format = "%d/%m/%Y") ##convert date variable into "date" class
    tData$datetime <- paste(tData$Date,tData$Time) ##create "datetime" variable
    tData$datetime <- strptime(tData$datetime, format = "%Y-%m-%d %H:%M:%S") ##convert to "date" class
    close(rData)
   
    
    
    makePlotFour <- function() {
        png(filename = "plot4.png", height = 480, width = 480, units = "px")
        par(mfrow = c(2,2))
        with(tData, {
            plot(datetime, Global_active_power, ylab = "Global Active Power", xlab = "", type ="l")
            plot(datetime, Voltage, type = "l")
            plot(datetime,Sub_metering_1, type = "l", ylab = "Energy sub metering", xlab = "")
                lines(datetime, Sub_metering_2, col = "red")
                lines(datetime, Sub_metering_3, col = "blue")
                legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black","red","blue"),
                   lwd = 1, y.intersp =  0.9, x.intersp = 0.7)
            plot(datetime, Global_reactive_power, type = "l")
            dev.off()
        })
        
    }
    
    #getData()
    makePlotFour()
}