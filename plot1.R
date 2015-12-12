plot1 <- function() {

    ##subset appropriate data for 2 day period in February
    
    rData <- file("household_power_consumption.txt", open = "rt") ##raw data
    x <- TRUE ##initialize 
    ## while x is true search for the exact line in txt file and start from there
    while(x) {
        x <- !grepl("31/1/2007;23:59:00;0.326;0.126;242.800;1.400;0.000;0.000;0.000", readLines(rData,n=1))
    }
    tData <- read.table(rData, header = FALSE, sep = ";", nrows = 2880, na.strings = "?")
    colnames(tData) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
    close(rData)
    

    ## create histogram for 'Global Active Power' in PNG format
    makePlotOne <- function() {
        png(filename = "plot1.png", height = 480, width = 480, units = "px")
        par(mar = c(5,5,2,1)) ##set parameters for histogram
        hist(tData$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
        dev.off() ##close png file device
    }

    
    makePlotOne()

}