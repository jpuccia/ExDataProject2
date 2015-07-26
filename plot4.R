## plot4.R
##
## Task: Create a plot that shows the total emissions of PM2.5 across 
##... the U.S. by year for sources related to coal to determine 
##... if emissions are increasing or decreasing.
##

plot4 <- function(){
        library(ggplot2)
        library(dplyr)
        library(reshape2)
        ## Read the emissions data from the data folder under the working directory
        nei <- readRDS("./data/summarySCC_PM25.rds")
        
        ## We need to load the scc data to determine the source of
        ##... emissions is from coal.
        scc <- readRDS("./data/Source_Classification_Code.rds")

        ## Join our SCC lookup table with the nei data by performing a merge
        neiMerge <- merge(nei, scc, by = "SCC")
        
        ## Melt the data down to group emissions by year but only for coal related source data
        coalSources <- unique(scc$EI.Sector[grep("coal", scc$EI.Sector, ignore.case = TRUE)])
        neiMelt <- melt(neiMerge[neiMerge$EI.Sector %in% coalSources,], 
                        id=c("year"), measure.vars=c("Emissions"))
        
        ## Sum the Emissions by Year
        neiCoal <- dcast(neiMelt, year ~ variable,sum)
        
        ## Plot the data to a png file
        png(filename = "plot4.png", width = 480, height = 480, units = "px")
        plot(neiCoal$year, neiCoal$Emissions, type = "l", col = "green",
             xlab="", ylab="Yearly PM2.5 Emissions (tons)", main = "Total Yearly PM2.5 Coal Emissions for U.S")
        
        ## Close the device to plot to a png file with the same name as this function.
        invisible(dev.off())
}