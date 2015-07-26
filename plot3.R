## plot3.R
##
## Task: Create a plot that shows the total emissions of PM2.5 by year and 
##... pollutant type for the city of Baltimore (fips=24510) to determine 
##... if emissions are increasing or decreasing.
##

plot3 <- function(){
        library(ggplot2)
        library(reshape2)
        ## Read the emissions data from the data folder under the working directory
        nei <- readRDS("./data/summarySCC_PM25.rds")
        
        ## Melt the data down to group Baltimore emissions by type and year.
        neiMelt <- melt(nei[nei$fips==24510,], id=c("type", "year"), measure.vars=c("Emissions"))
        
        ## Sum the Emissions by type and Year
        neiBaltType <- dcast(neiMelt, type + year ~ variable,sum)
        names(neiBaltType)[1] <- "Type"
        
        ## Plot the data to a png file
        png(filename = "plot3.png", width = 480, height = 480, units = "px")
        p <- qplot(year, Emissions, data = neiBaltType, group = Type, color = Type, 
              geom = c("point", "line"), ylab = "Yearly PM2.5 Emissions (tons)", 
              xlab = "Year", main = "Total PM2.5 Emissions in Baltimore by Type of Pollutant")
        print(p)
        
        ## Close the device to plot to a png file with the same name as this function.
        invisible(dev.off())
}