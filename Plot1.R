### PLOT 1 ###

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# Read data
NEI <- readRDS("Data/summarySCC_PM25.rds")

# Aggregate by year
SumByYear <- aggregate(NEI$Emissions ~ NEI$year, FUN = sum)

# Constructing plot
png("Plot1.png",width=480,height=480)

barplot(height = SumByYear$`NEI$Emissions`,
        names.arg = SumByYear$`NEI$year`,
        main = "Total Emissions of PM2.5 in the US",
        ylab = "Emissions of PM2.5 (in tons)", 
        xlab = "Year")

dev.off()
