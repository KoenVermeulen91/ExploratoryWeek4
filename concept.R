### EDA CASE STUDY: AIR POLLUTION ###

# Ask the question: Are the levels on average lower in 2012 than in 1999?

getwd()

# Setting Working Directory
setwd("~/Downloads/Data_Science_Specialization/Tests/Tests_R/ExploratoryWeek4")

# Opening relevant libraries
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Loading data from data folder
NEI <- readRDS("Data/summarySCC_PM25.rds")
SCC <- readRDS("Data/Source_Classification_Code.rds")
NEI[NEI == ""] <- NA
SCC[SCC == ""] <- NA

# First check data
names(NEI)
head(NEI)
glimpse(NEI)
table(NEI$year)
plot(table(NEI$fips))
table(NEI$SCC)
table(NEI$Pollutant)
plot(NEI$Emissions)

names(SCC)
head(SCC)
glimpse(SCC)
table(SCC$Option.Group)
table(SCC$Data.Category)

# Check NAs
NA_NEI <- as.data.frame(sapply(NEI, function(x) sum(is.na(x)))) # no NAs
sum(is.na(NEI$fips))
sum(is.na(NEI$SCC))
sum(is.na(NEI$Pollutant))
sum(is.na(NEI$Emissions))
sum(is.na(NEI$type))
sum(is.na(NEI$year))

NA_SCC <- as.data.frame(sapply(SCC, function(x) sum(is.na(x)))) # some NAs

# Joining datasets
NEI_SCC <- left_join(NEI, SCC, by = "SCC")

# Check NAs
NA_NEI_SCC <- as.data.frame(sapply(NEI_SCC, function(x) sum(is.na(x))))












pmsub0 <- subset(P25_1999, State.Code == 36 & County.Code == 63 & Site.Num == 2008) 
pmsub1 <- subset(P25_2012, State.Code == 36 & County.Code == 63 & Site.Num == 2008) 

dates0 <- pmsub0$Datum
mean0 <- pmsub0$Arithmetic.Mean
dates0


dates0 <- pmsub0$Datum
mean0 <- pmsub0$Arithmetic.Mean

## setwd("~/CourseraModules/04_ExploratoryAnalysis/CaseStudy/pm25_data")

## Has fine particle pollution in the U.S. decreased from 1999 to
## 2012?

## Read in data from 1999

pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
dim(pm0)
head(pm0)
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
print(cnames)
cnames <- strsplit(cnames, "|", fixed = TRUE)
print(cnames)
names(pm0) <- make.names(cnames[[1]])
head(pm0)
x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0))  ## Are missing values important here?

## Read in data from 2012

pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "", nrow = 1304290)
names(pm1) <- make.names(cnames[[1]])
head(pm1)
dim(pm1)
x1 <- pm1$Sample.Value
class(x1)

## Five number summaries for both periods
summary(x1)
summary(x0)
mean(is.na(x1))  ## Are missing values important here?

## Make a boxplot of both 1999 and 2012
boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

## Check negative values in 'x1'
summary(x1)
negative <- x1 < 0
sum(negative, na.rm = T)
mean(negative, na.rm = T)
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, "month")  ## Check what's going on in months 1--6


## Plot a subset for one monitor at both times

## Find a monitor for New York State that exists in both datasets
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
str(site0)
str(site1)
both <- intersect(site0, site1)
print(both)

## Find how many observations available at each monitor
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

## Choose county 63 and side ID 2008
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm1sub)
dim(pm0sub)

## Plot data for 2012
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
str(dates1)
plot(dates1, x1sub)

## Plot data for 1999
dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

## Plot data for both years in same panel
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20)  ## Whoa! Different ranges
abline(h = median(x1sub, na.rm = T))

## Find global range
rng <- range(x0sub, x1sub, na.rm = T)
rng
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))

## Show state-wide means and make a plot showing trend
head(pm0)
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)

## Make separate data frames for states / years
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)

## Connect lines
par(mfrow = c(1, 1))
with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5)))
" For the first column of points, call with with 2 arguments. The first is mrg, and the second is the
| call to plot with 3 arguments. The first of these is rep(1,52). This tells the plot routine that the x
| coordinates for all 52 points are 1. The second argument is the second column of mrg or mrg[,2] which
| holds the 1999 data. The third argument is the range of x values we want, namely xlim set to
| c(.5,2.5). This works since we'll be plotting 2 columns of points, one at x=1 and the other at x=2."
with(mrg, points(rep(2, 52), mrg[, 3]))
" We see a column of points at x=1 which represent the 1999 state means. For the second column of
| points, again call with with 2 arguments. As before, the first is mrg. The second, however, is a call
| to the function points with 2 arguments. We need to do this since we're adding points to an already
| existing plot. The first argument to points is the set of x values, rep(2,52). The second argument is
| the set of y values, mrg[,3]. Of course, this is the third column of mrg. (We don't need to specify
| the range of x values again.)"
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])
" We see a shorter column of points at x=2. Now let's connect the dots. Use the R function segments with
| 4 arguments. The first 2 are the x and y coordinates of the 1999 points and the last 2 are the x and y
| coordinates of the 2012 points. As in the previous calls specify the x coordinates with calls to rep
| and the y coordinates with references to the appropriate columns of mrg."


