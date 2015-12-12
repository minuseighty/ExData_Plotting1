plot2 <- function() {

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
    
    
    makePlotTwo <- function() {
        png(filename = "plot2.png", height = 480, width = 480, units = "px")
        par(mar = c(3,5,1,1)) ##set parameters
        with(tData, plot(datetime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))
        dev.off() ##turn png file device off
        
    }
    
    
    makePlotTwo()
    
}
