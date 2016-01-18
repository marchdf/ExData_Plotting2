plot1 <- function(df) {

    ## ## Load the dataset using our custom load function
    ## df <- load_dataset()

    ## Name of the plot
    png('plot1.png')

    ## ## Data for the plot: total emissions by year
    ## df <- tapply(NEI$Emissions, NEI$year, FUN=sum)

    ## Make the histogram
    plot(y = df, x = names(df),main="Total emissions",xlab="Year",ylab="Total emissions",col="red")

    ## Save the plot
    dev.off()
}
