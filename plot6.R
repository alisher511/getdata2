# Check if the files are in your current working directory - check by calling dir()
# The data can be obtained at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# This first line will likely take a few seconds. Please be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#
#                               ***   Question 6   ***
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time 
# in motor vehicle emissions?
require(ggplot2)

contains_veh <- grep('veh', SCC$Short.Name, ignore.case = TRUE, value = TRUE)

df2 <- SCC[match(contains_veh, SCC$Short.Name),]
veh <- Baltimore[match(df2$SCC, Baltimore$SCC), ]
veh <- na.omit(veh)

LA <- subset(NEI, fips == "06037")

vehLA <- LA[match(df2$SCC, LA$SCC), ]
vehLA <- na.omit(vehLA)
head(vehLA)
dim(vehLA)

z <- rbind(veh, vehLA)

#http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
cities <- list('24510'="Baltimore", '06037'="Los Angeles")
city_labeller <- function(variable,value){
        return(cities[value])
}

g <- ggplot(z, aes(x=year, y=Emissions)) + facet_wrap(~fips, labeller = city_labeller)
g <- g + stat_summary(fun.y="sum", geom="line", color = "gold", lwd = 1.5, aes(group=1))
g <- g + geom_point(pch=17, color = "red")
g <- g + labs(title="Total PM2.5 Emissions from Motor Vehicle Sources", y="PM 2.5 (in tons)") 
g

dev.copy(png, 'plot6.png', width = 600, height = 600)
dev.off()