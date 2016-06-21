# Check if the files are in your current working directory - check by calling dir()
# The data can be obtained at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# This first line will likely take a few seconds. Please be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#
#                               ***   Question 3   ***
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

require(ggplot2)
require(reshape)

Baltimore <- subset(NEI, fips == "24510")
Baltimore$year <- as.factor(Baltimore$year) 

btmore <- melt(Baltimore, type = c("type", "year"))
head(btmore)

g <- ggplot(btmore, aes(x=year, y=value, color = type))  +facet_wrap( ~ type, ncol =2)
g <- g + labs(title="Total Baltimore City PM2.5 Emissions by Type", y="PM 2.5 (in tons)") 
g <- g + stat_summary(fun.y="sum", geom="line", lwd = 2, aes(group=1))
g

dev.copy(png, 'plot3.png', width = 600, height = 600)
dev.off()