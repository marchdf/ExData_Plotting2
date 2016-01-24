plot1 <- function(df) {

    ## Load the dataset
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI$fips <- as.factor(NEI$fips)
    NEI$SCC <- as.factor(NEI$SCC)
    NEI$Pollutant <- as.factor(NEI$Pollutant)
    NEI$type <- as.factor(NEI$type)
    NEI$year <- as.factor(NEI$year)
    SCC <- readRDS("Source_Classification_Code.rds")

    ## Data for the plot: total emissions by year
    list_sum <- tapply(NEI$Emissions, NEI$year, FUN=sum)

    ## Make the plot
    png('plot1.png')
    plot(list_sum,main="Total emissions",xlab="Year",ylab="Total emissions",col="red",xaxt="n")
    axis(1,at=1:length(list_sum),labels=names(list_sum))
    
    ## Save the plot
    dev.off()
}
