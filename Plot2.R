# National Emissions Inventory (NEI)
# 

library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#            from 1999 to 2008?
plot2 <- function() {         
        polBaltimoreByYear <- filter(NEI, fips == "24510") %>% group_by(year)
        totalBaltimoreEmissionsByYear <- summarise(polBaltimoreByYear, sum(Emissions, na.rm=TRUE))
        names(totalBaltimoreEmissionsByYear)[2]<-"totalEmissions"
        
        png(filename = "Plot2.png", width = 480, height = 480)
        with(totalBaltimoreEmissionsByYear, plot(year, totalEmissions, ylab="Total Emissions (tons)", main="Total Baltimore Emissions By Year"))
        dev.off()
}