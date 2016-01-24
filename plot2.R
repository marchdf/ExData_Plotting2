plot2 <- function(df) {

    ## Load libraries
    library(dplyr)

    ## Load the dataset
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI$fips <- as.factor(NEI$fips)
    NEI$SCC <- as.factor(NEI$SCC)
    NEI$Pollutant <- as.factor(NEI$Pollutant)
    NEI$type <- as.factor(NEI$type)
    NEI$year <- as.factor(NEI$year)
    SCC <- readRDS("Source_Classification_Code.rds")
    
    ## Data for the plot: total emissions by year for baltimore (fips = 24510)
    subdf <- NEI[NEI$fips == "24510", ]
    groups <- group_by(subdf,year)
    sumdf <- summarise(groups,sum(Emissions))
    names(sumdf) <- c("year","Emissions")

    ## Make the plot
    png('plot2.png')
    barplot(sumdf$Emissions,names.arg=as.character(sumdf$year),main="Total emissions in Baltimore City, MD",xlab="Year",ylab="Total emissions (tons)",col=cmap[1])
    
    ## Save the plot
    dev.off()
}
