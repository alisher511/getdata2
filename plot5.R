# Check if the files are in your current working directory - check by calling dir()
# The data can be obtained at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# This first line will likely take a few seconds. Please be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#
#                               ***   Question 5   ***
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
require(ggplot2)
Baltimore <- subset(NEI, fips == "24510")
Baltimore$year <- as.factor(Baltimore$year) 

contains_veh <- grep('veh', SCC$Short.Name, ignore.case = TRUE, value = TRUE)
head(contains_veh,2)
length(contains_veh)

df2 <- SCC[match(contains_veh, SCC$Short.Name),]
dim(df2)

veh <- Baltimore[match(df2$SCC, Baltimore$SCC), ]
veh <- na.omit(veh)
head(veh)
dim(veh)

g <- ggplot(veh, aes(x=year, y=Emissions))
g <- g + labs(title="Total Baltimore City PM2.5 Emissions from Motor Vehicle Sources", y="PM 2.5 (in tons)") 
g <- g + stat_summary(fun.y="sum", geom="line", color = "purple", lwd = 1.5, aes(group=1))
g

dev.copy(png, 'plot5.png', width = 600, height = 600)
dev.off()