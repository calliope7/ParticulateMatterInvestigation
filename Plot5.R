# National Emissions Inventory (NEI)
# 

library(dplyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
plot5 <- function() {
        
        motorVehicleIDs <- filter(SCC, SCC.Level.One == "Mobile Sources", SCC.Level.Two=="Highway Vehicles - Diesel" | SCC.Level.Two=="Highway Vehicles - Gasoline") %>% select(SCC) 
        motorVehicleIDs <- sapply(motorVehicleIDs, function(x) as.character(x))
        targetNEIByYear <- filter(NEI, fips == "24510") %>% filter(SCC %in% motorVehicleIDs) %>% group_by(year)
        
        
        #now let's see what it looks like!
        totalMotorVehicleEmissionsBaltimore <- summarise(targetNEIByYear, sum(Emissions, na.rm=TRUE))
        names(totalMotorVehicleEmissionsBaltimore)[2]<-"totalEmissions"
        
        png(filename = "Plot5.png", width = 480, height = 480)
        plot <- ggplot(data=totalMotorVehicleEmissionsBaltimore, aes(year, totalEmissions))
        plot <- plot + geom_point(color="orange", size=4)
        plot <- plot + geom_smooth(color="black", method = "lm", se=FALSE)
        plot <- plot + labs(title="Trends for Total Emissions from Motor Vehicles in Baltimore", x="Year", y="Total Emissions (tons)")
        print(plot)
        dev.off()
}
