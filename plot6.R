## plot6.R
##
## Task: Create a plot that shows the total vehicle emissions of PM2.5 in 
##... Baltimore (fips=24510) and Los Angeles (fips=06037) by year to 
##... determine if emissions are increasing or decreasing and to 
##... compare the two cities.
##

plot6 <- function(){
        library(ggplot2)
        library(dplyr)
        library(reshape2)
        ## Read the emissions data from the data folder under the working directory
        nei <- readRDS("./data/summarySCC_PM25.rds")
        
        ## We need to load the scc data to determine the source of
        ##... emissions is from vehicles.
        scc <- readRDS("./data/Source_Classification_Code.rds")
        
        ## Join our SCC lookup table with the nei data by performing a merge
        neiMerge <- merge(nei, scc, by = "SCC")
        
        ## Melt the data down to group emissions by year and fips but only for 
        ##... vehicle related source data for Baltimore and Los Angeles.
        vehicleSources <- unique(scc$EI.Sector[grep("vehicle", scc$EI.Sector, ignore.case = TRUE)])
        neiMelt <- melt(
                neiMerge[(neiMerge$fips=="24510" | neiMerge$fips=="06037") & neiMerge$EI.Sector %in% vehicleSources,], 
                id=c("fips", "year"), measure.vars=c("Emissions"))
        
        ## Sum the Emissions by Year
        neiVehicle <- dcast(neiMelt, fips + year ~ variable,sum)
        
        ## Change the "fips" column to "City"
        names(neiVehicle)[1] <- "City"
        
        ## Change the fips code to the city name
        neiVehicle[neiVehicle$City=="24510",]$City <- "Baltimore"
        neiVehicle[neiVehicle$City=="06037",]$City <- "Los Angeles"
        
        ## Plot the data to a png file
        png(filename = "plot6.png", width = 480, height = 480, units = "px")
        p <- qplot(year, Emissions, data = neiVehicle, group = City, color = City, 
                   geom = c("point", "line"), ylab = "Yearly PM2.5 Emissions (tons)", 
                   xlab = "Year", main = "Total PM2.5 Vehicle Emissions in Baltimore and Los Angeles")
        print(p)
        
        ## Close the device to plot to a png file with the same name as this function.
        invisible(dev.off())
}