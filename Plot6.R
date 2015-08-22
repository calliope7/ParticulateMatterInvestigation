# National Emissions Inventory (NEI)
# 

library(dplyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# return city from fips
selectCity <- function(fips) {
     city = "Baltimore"
     if (as.character(fips)=="06037") {
             city = "Los Angeles"
     }
     city
}

calcDelta <- function(totalEmissions1, fips1, totalMotorVehicleEmissionsBaltimoreAndLA) {
        valueAt1999 <- filter(totalMotorVehicleEmissionsBaltimoreAndLA, fips==as.character(fips1) & year==1999) %>% select(totalEmissions)
        as.numeric(totalEmissions1) - valueAt1999[[1,2]]
}

#Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#            Which city has seen greater changes over time in motor vehicle emissions?
plot6 <- function() {
        
        motorVehicleIDs <- filter(SCC, SCC.Level.One == "Mobile Sources", SCC.Level.Two=="Highway Vehicles - Diesel" | SCC.Level.Two=="Highway Vehicles - Gasoline") %>% select(SCC) 
        motorVehicleIDs <- sapply(motorVehicleIDs, function(x) as.character(x))
        baltimoreAndLosAngelesByYear <- filter(NEI, fips == "24510" | fips == "06037") %>% filter(SCC %in% motorVehicleIDs) %>% group_by(fips, year)
        
        totalMotorVehicleEmissionsBaltimoreAndLA <- summarise(baltimoreAndLosAngelesByYear, sum(Emissions, na.rm=TRUE))
        names(totalMotorVehicleEmissionsBaltimoreAndLA)[3]<-"totalEmissions" 
        totalMotorVehicleEmissionsBaltimoreAndLA <- mutate(totalMotorVehicleEmissionsBaltimoreAndLA, city = selectCity(fips))
        totalMotorVehicleEmissionsBaltimoreAndLA <- mutate(totalMotorVehicleEmissionsBaltimoreAndLA, deltaEmissions = calcDelta(totalEmissions, fips, totalMotorVehicleEmissionsBaltimoreAndLA))
        
        #now let's see what it looks like!
        png(filename = "Plot6.png", width = 480, height = 480)
        plot <- ggplot(data=totalMotorVehicleEmissionsBaltimoreAndLA, aes(year, deltaEmissions))
        plot <- plot + geom_point(aes(color=city, shape=city), size=4)
        plot <- plot + geom_smooth(aes(color=city), method = "lm", se=FALSE)
        plot <- plot + labs(title="Changes in Emissions from Motor Vehicles (Baltimore vs. LA)", x="Year", y="Delta Emissions from 1999 (tons)")
        print(plot)
        dev.off()
}
