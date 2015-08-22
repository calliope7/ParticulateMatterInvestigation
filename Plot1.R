# National Emissions Inventory (NEI)
# 

library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
plot1 <- function() {
        polByYear <- group_by(NEI, year)
        totalEmissionsByYear <- summarise(polByYear, sum(Emissions, na.rm=TRUE))
        names(totalEmissionsByYear)[2]<-"totalEmissions"
        
        png(filename = "Plot1.png", width = 480, height = 480)
        with(totalEmissionsByYear, plot(year, totalEmissions, ylab="Total Emissions (tons)", main="Total Emissions By Year"))
        dev.off()
}