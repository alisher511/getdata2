# Check if the files are in your current working directory - check by calling dir()
# The data can be obtained at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# This first line will likely take a few seconds. Please be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#                       ***   Question 2   ***
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

Baltimore <- subset(NEI, fips == "24510")
head(Baltimore, 3)

balt99 <- subset(Baltimore, year == 1999)
balt02 <- subset(Baltimore, year == 2002)
balt05 <- subset(Baltimore, year == 2005)
balt08 <- subset(Baltimore, year == 2008)

v <- c(sum(balt99$Emissions), sum(balt02$Emissions), sum(balt05$Emissions), sum(balt08$Emissions))

plot(x = c(1999,2002,2005,2008), y=v, type = "l", col = "khaki", lwd=2, main = "Baltimore City PM2.5 Emissions", xlab = "year", ylab = "PM 2.5 (in tons)")
grid()

dev.copy(png, 'plot2.png', width = 600, height = 600)
dev.off()