### PLOT 5 ###

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Reading & data
NEI <- readRDS("Data/summarySCC_PM25.rds")

# Subsetting "ON-ROAD" in Baltimore & aggregating by year
NEI_Balt_Road <- NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD", ]

SumByYear <- aggregate(NEI_Balt_Road$Emissions ~ NEI_Balt_Road$year, FUN = sum)

# Constructing plot
png("Plot5.png",width=480,height=480)

barplot(height = SumByYear$`NEI_Balt_Road$Emissions`,
        names.arg = SumByYear$`NEI_Balt_Road$year`,
        main = "Motor vehicle related Emissions of PM2.5 in Baltimore",
        ylab = "Emissions of PM2.5 (in tons)", 
        xlab = "Year")

dev.off()
