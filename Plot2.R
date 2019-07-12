### PLOT 2 ###

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510"
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Read data
NEI <- readRDS("Data/summarySCC_PM25.rds")

# Subsetting Baltimore & aggregating by year
NEI_Balt <- NEI[NEI$fips == "24510", ]

SumByYear <- aggregate(NEI_Balt$Emissions ~ NEI_Balt$year, FUN = sum)

# Constructing plot
png("Plot2.png",width=480,height=480)

barplot(height = SumByYear$`NEI_Balt$Emissions`,
        names.arg = SumByYear$`NEI_Balt$year`,
        main = "Total Emissions of PM2.5 in Baltimore",
        ylab = "Emissions of PM2.5 (in tons)", 
        xlab = "Year")

dev.off()
