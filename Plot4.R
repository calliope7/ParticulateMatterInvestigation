# National Emissions Inventory (NEI)
# 

library(dplyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
plot4 <- function() {        
        # If we use the short name to find all observations with 'Coal' noted.. then we have the following set of Level.One classifiers:
        # External Combustion Boilers, Internal Combustion Engines, Stationary Source Fuel Combustion,
        # Mobile Sources, Industrial Processes, Storage and Transport, Miscellaneous Area Sources, Industrial Processes,
        # Waste Disposal
        # 
        # We found these with the following code-
        # measurements with 'Coal' in their short name:
        matches <- sapply(SCC[, "Short.Name"], function(x) {grepl("[^r][Cc]oal|^[Cc]oal", x)})

        # From these we can look closer to filter out non-combustion processes:
        # "Waste Disposal" shows only 'Open Burning' as a Level.Two having to do with coal combustion
        matchedWasteDisposal <- sapply(SCC[matches, "SCC.Level.One"], function(x) {x=="Waste Disposal"})
        matchedWasteDisposal2 <- sapply(SCC[matches,][matchedWasteDisposal, "SCC.Level.Three"], function(x) {grepl("Open Burning", x)})
        wastDispIDs <- as.character(SCC[matches,][matchedWasteDisposal,][matchedWasteDisposal2,"SCC"])
        
        # Mobile Sources where Coal it the fuel should be included (its a combustion process)
        matchedMob = sapply(SCC[matches, "SCC.Level.One"], function(x) {x=="Mobile Sources"})
        mobIDs <- as.character(SCC[matches,][matchedMob,"SCC"])
        
        # we Don't want Storage related because they are only about storage and transport Of coal not burning it
        #matchedStorage = sapply(SCC[matches, "SCC.Level.One"], function(x) {x=="Storage and Transport"})
        #SCC[matches,][matchedStorage,]
        
        # this Internal Combustion Engine burned gasified Coal so it is about coal combustion
        matchedIntCombEng = sapply(SCC[matches, "SCC.Level.One"], function(x) {x=="Internal Combustion Engines"})
        intCombEngIDs <- as.character(SCC[matches,][matchedIntCombEng,"SCC"])
        
        #Industrial Processes - some use coal - this is hard to sort but In-process Fuel Use of coal is a certanly hit
        matchedIndust = sapply(SCC[matches, "SCC.Level.One"], function(x) {x=="Industrial Processes"})
        matchedIndust2 <- sapply(SCC[matches,][matchedIndust, "SCC.Level.Two"], function(x) {x == "In-process Fuel Use"})
        industIDs <- as.character(SCC[matches,][matchedIndust,][matchedIndust2,"SCC"])
        
        #Stationary Source Fuel Combustion - All matched
        matchedSSFuelComb = sapply(SCC[matches, "SCC.Level.One"], function(x) {x=="Stationary Source Fuel Combustion"})
        ssFuelCombIDs <- as.character(SCC[matches,][matchedSSFuelComb,"SCC"])
        
        #External Combustion Boilers - All matched
        matchedExtComb = sapply(SCC[matches, "SCC.Level.One"], function(x) {x=="External Combustion Boilers"})
        extCombIDs <- as.character(SCC[matches,][matchedExtComb,"SCC"])
        
        #Join all the SCCs that have something to do with coal combustion
        sccIDs <- c(wastDispIDs, mobIDs, intCombEngIDs, industIDs, ssFuelCombIDs, extCombIDs)
        
        #filter in All these sources in the NEI dataset
        targetNEIByYear <- filter(NEI, SCC %in% sccIDs) %>% group_by(year)
        
        #now let's see what it looks like!
        totalCoalEmissionsByYear <- summarise(targetNEIByYear, sum(Emissions, na.rm=TRUE))
        names(totalCoalEmissionsByYear)[2]<-"totalEmissions"
        
        png(filename = "Plot4.png", width = 480, height = 480)
        plot <- ggplot(data=totalCoalEmissionsByYear, aes(year, totalEmissions))
        plot <- plot + geom_point(color="orange", size=4)
        plot <- plot + geom_smooth(color="black", method = "lm", se=FALSE)
        plot <- plot + labs(title="Trends for Total Emissions from Coal Combustion", x="Year", y="Total Emissions (tons)")
        print(plot)
        dev.off()
}
