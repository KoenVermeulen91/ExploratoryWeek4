### PLOT 4 ###

# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# Reading & joining data
NEI <- readRDS("Data/summarySCC_PM25.rds")
SCC <- readRDS("Data/Source_Classification_Code.rds")
NEI[NEI == ""] <- NA
SCC[SCC == ""] <- NA
NEI_SCC <- left_join(NEI, SCC, by = "SCC")

# Subsetting coal related sources & aggregating by year
CoalComb <- grepl("coal", NEI_SCC$Short.Name, ignore.case = T)
NEI_SCC_Coal <- NEI_SCC[CoalComb, ]

SumByYear <- aggregate(NEI_SCC_Coal$Emissions ~ NEI_SCC_Coal$year, FUN = sum)

# Constructing plot
png("Plot4.png",width=480,height=480)

barplot(height = SumByYear$`NEI_SCC_Coal$Emissions`,
        names.arg = SumByYear$`NEI_SCC_Coal$year`,
        main = "Coal-related Emissions of PM2.5 across the US",
        ylab = "Emissions of PM2.5 (in tons)", 
        xlab = "Year")

dev.off()
