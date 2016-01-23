plot2 <- function(df) {

    ## Load the dataset using our custom load function
    NEI <- load_dataset()
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
