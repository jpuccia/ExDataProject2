## plot2.R
##
## Task: Create a plot that shows the total emissions of PM2.5 by year for the 
##... city of Baltimore (fips=24510) to determine if emissions are increasing 
##... or decreasing.
##

plot2 <- function(){
        library(reshape2)
        ## Read the emissions data from the data folder under the working directory
        nei <- readRDS("./data/summarySCC_PM25.rds")
        
        ## Melt the data down to group Baltimore emissions by Pollutant and year.
        neiMelt <- melt(nei[nei$fips==24510,], id=c("Pollutant", "year"), measure.vars=c("Emissions"))
        
        ## Sum the Emissions by Pollutant and Year
        neiYearData <- dcast(neiMelt, year + Pollutant ~ variable,sum)
        
        ## Plot the data to a png file
        png(filename = "plot2.png", width = 480, height = 480, units = "px")
        plot(neiYearData$year, neiYearData$Emissions, type = "l", col = "blue",
             xlab="", ylab="Yearly Emissions (tons)", main = "Total Yearly Emissions for Baltimore, MD")
        
        ## Close the device to plot to a png file with the same name as this function.
        invisible(dev.off())
}