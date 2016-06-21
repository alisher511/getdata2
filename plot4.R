# Check if the files are in your current working directory - check by calling dir()
# The data can be obtained at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# This first line will likely take a few seconds. Please be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#
#                               ***   Question 4   ***
# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?
require(ggplot2)
View(SCC)

contains_coal <- grep('coal', SCC$Short.Name, ignore.case = TRUE, value = TRUE)
contains_coal_comb <- grep('comb', contains_coal, ignore.case = TRUE, value = TRUE)
head(contains_coal_comb,2)
length(contains_coal_comb)

df <- SCC[match(contains_coal_comb, SCC$Short.Name),]
dim(df)

ccomb <- NEI[NEI$SCC == df$SCC, ]
head(ccomb)

g <- ggplot(ccomb, aes(x=year, y=Emissions))
g <- g + labs(title="PM2.5 Emissions from Coal Combustion in the U.S.", y="PM 2.5 (in tons)") 
g <- g + stat_summary(fun.y="sum", geom="line",color = "brown", lwd = 1.5, aes(group=1))
g

dev.copy(png, 'plot4.png', width = 600, height = 600)
dev.off()