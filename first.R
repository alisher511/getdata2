setwd("C:/Users/Ali/Desktop/MOOCS/DATA_SCIENCE/expdata/course_project")

#Check if the files are in your current working directory - check by calling dir()
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

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

plot(x = c(1999,2002,2005,2008), y= c(sum99,sum02,sum05,sum08), type = "l", col = "green", lwd=2, main = "Total PM 2.5 Emissions", xlab = "year", ylab = "PM 2.5 (in mln tons)")
grid()

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#   Use the base plotting system to make a plot answering this question.

Baltimore <- subset(NEI, fips == "24510")
head(Baltimore, 3)

balt99 <- subset(Baltimore, year == 1999)
balt02 <- subset(Baltimore, year == 2002)
balt05 <- subset(Baltimore, year == 2005)
balt08 <- subset(Baltimore, year == 2008)

v <- c(sum(balt99$Emissions), sum(balt02$Emissions), sum(balt05$Emissions), sum(balt08$Emissions))

plot(x = c(1999,2002,2005,2008), y=v, type = "l", col = "khaki", lwd=2, main = "Baltimore City PM2.5 Emissions", xlab = "year", ylab = "PM 2.5 (in tons)")
grid()

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

require(ggplot2)

# baltNR <- subset(Baltimore, type == "NON-ROAD")
# baltNP <- subset(Baltimore, type == "NONPOINT")
# baltOR <- subset(Baltimore, type == "ON-ROAD")
# baltPT <- subset(Baltimore, type == "POINT")

require(reshape)
Baltimore$year <- as.factor(Baltimore$year) 

btmore <- melt(Baltimore, type = c("type", "year"))
head(btmore)

# typesum <- cast(btmore, type~variable, sum)
# typesum
# 
# yearsum <- cast(btmore, year~variable, sum)
# yearsum

# a <- cast(btmore, type~year~variable, sum )
# a
# a <- as.data.frame(a)
# rowSums(a)
# colnames(a) <- c("1999", "2002", "2005", "2008")
# 
# 
# sum(baltPT$Emissions) ## CHECK

g <- ggplot(btmore, aes(x=year, y=value, color = type))  +facet_wrap( ~ type, ncol =2)
g <- g + labs(title="Total Baltimore City PM2.5 Emissions by Type", y="PM 2.5 (in tons)") 
g <- g + stat_summary(fun.y="sum", geom="line", lwd = 2, aes(group=1))
g

#4 Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
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

#5 How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

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



#6 Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time 
# in motor vehicle emissions?

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
