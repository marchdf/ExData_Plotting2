plot2 <- function(df) {

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
    list_sum <- tapply(subdf$Emissions, subdf$year, FUN=sum)

    ## Make the plot
    png('plot2.png')
    plot(list_sum,main="Total emissions in Baltimore City, MD",xlab="Year",ylab="Total emissions",col="red",xaxt="n")
    axis(1,at=1:length(list_sum),labels=names(list_sum))
    
    ## Save the plot
    dev.off()
}
