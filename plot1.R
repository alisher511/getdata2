# Check if the files are in your current working directory - check by calling dir()
# The data can be obtained at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# This first line will likely take a few seconds. Please be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#
#                               ***   Question 1   ***
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

head(NEI, 3)
dim(NEI)
table(NEI$year)

y99 <- subset(NEI, year == 1999)
y02 <- subset(NEI, year == 2002)
y05 <- subset(NEI, year == 2005)
y08 <- subset(NEI, year == 2008)

sum99 <- sum(y99$Emissions)/1000000
sum02 <- sum(y02$Emissions)/1000000
sum05 <- sum(y05$Emissions)/1000000
sum08 <- sum(y08$Emissions)/1000000

plot(x = c(1999,2002,2005,2008), y= c(sum99,sum02,sum05,sum08), type = "l", col = "green", lwd=2, main = "Total PM 2.5 Emissions in the U.S.", xlab = "year", ylab = "PM 2.5 (in mln tons)")
grid()

dev.copy(png, 'plot1.png', width = 600, height = 600)
dev.off()