## plot5.R
##
## Task: Create a plot that shows the total vehicle emissions of PM2.5 in 
##... Baltimore by year to determine if emissions are increasing or decreasing.
##

plot5 <- function(){
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
        
        ## Melt the data down to group emissions by year but only for vehicle 
        ##... related source data for the city of Baltimore.
        vehicleSources <- unique(scc$EI.Sector[grep("vehicle", scc$EI.Sector, ignore.case = TRUE)])
        neiMelt <- melt(neiMerge[neiMerge$fips=="24510" & neiMerge$EI.Sector %in% vehicleSources,], 
                        id=c("year"), measure.vars=c("Emissions"))
        
        ## Sum the Emissions by Year
        neiVehicle <- dcast(neiMelt, year ~ variable,sum)
        
        ## Plot the data to a png file
        png(filename = "plot5.png", width = 480, height = 480, units = "px")
        plot(neiVehicle$year, neiVehicle$Emissions, type = "l", col = "blue",
             xlab="", ylab="Yearly PM2.5 Emissions (tons)", main = "Total Yearly PM2.5 Vehicle Emissions for Baltimore")
        
        ## Close the device to plot to a png file with the same name as this function.
        invisible(dev.off())
}