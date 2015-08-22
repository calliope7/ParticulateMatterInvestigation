# National Emissions Inventory (NEI)
# 

library(dplyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#            which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#            Which have seen increases in emissions from 1999–2008?
plot3 <- function() {         
        polBaltimoreByYear <- filter(NEI, fips == "24510") %>% group_by(type, year)
        totalBaltimoreEmissionsByYear <- summarise(polBaltimoreByYear, sum(Emissions, na.rm=TRUE))
        names(totalBaltimoreEmissionsByYear)[3]<-"totalEmissions"
        
        png(filename = "Plot3.png", width = 480, height = 480)
        plot <- ggplot(data=totalBaltimoreEmissionsByYear, aes(year, totalEmissions))
        plot <- plot + geom_point(aes(color=type), size=4)
        plot <- plot + geom_smooth(aes(color=type), method = "lm", se=FALSE)
        plot <- plot + labs(title="Emission Trends for Polution Source Types", x="Year", y="Total Emissions (tons)")
        print(plot)
        dev.off()
}