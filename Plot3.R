### PLOT 3 ###

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

# Reading data
NEI <- readRDS("Data/summarySCC_PM25.rds")

# Subsetting Baltimore & aggregating by year & type
NEI_Balt <- NEI_SCC[NEI_SCC$fips == "24510", ]

SumByYearType <- aggregate(NEI_Balt$Emissions ~ NEI_Balt$year + NEI_Balt$type, FUN = sum)



g <- ggplot(SumByYearType, aes(SumByYearType$`NEI_Balt$year`, 
                               SumByYearType$`NEI_Balt$Emissions`, 
                               color = SumByYearType$`NEI_Balt$type`))

# Constructing plot
png("Plot3.png",width=480,height=480)

g <- g + geom_line() +
        xlab("Year") +
        ylab("Emissions of PM2.5 (in tons)") +
        ggtitle("Emissions of PM2.5 by year and type") +
        theme(legend.title = element_blank())
print(g)

dev.off()
