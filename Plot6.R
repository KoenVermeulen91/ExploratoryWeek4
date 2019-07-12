### PLOT 6 ###

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County,
# California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

# Reading & data
NEI <- readRDS("Data/summarySCC_PM25.rds")

# Subsetting "ON-ROAD" in Baltimore & Los Angeles & aggregating by year
NEI_BaltLA_Road <- NEI[(NEI$fips == "24510" | NEI$fips == "06037") & NEI$type == "ON-ROAD", ]

SumByYear <- aggregate(NEI_BaltLA_Road$Emissions ~ NEI_BaltLA_Road$year + NEI_BaltLA_Road$fips, FUN = sum)

# Constructing plot
png("Plot6.png",width=480,height=480)

g <- ggplot(SumByYear, aes(SumByYear$`NEI_BaltLA_Road$year`, 
                           SumByYear$`NEI_BaltLA_Road$Emissions`, 
                           color = SumByYear$`NEI_BaltLA_Road$fips`))
g <- g + geom_line() +
        xlab("Year") +
        ylab("Emissions of PM2.5 (in tons)") +
        ggtitle("Motor Vehicle related Emissions of PM2.5 in Balitmore & LA") +
        theme(legend.title = element_blank())
print(g)

dev.off()
