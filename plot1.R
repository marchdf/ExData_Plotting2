plot1 <- function(df) {

    ## Load the dataset using our custom load function
    NEI <- load_dataset()
    SCC <- readRDS("Source_Classification_Code.rds")
    
    ## Name of the plot
    png('plot1.png')
   
    ## Data for the plot: total emissions by year
    list_sum <- tapply(NEI$Emissions, NEI$year, FUN=sum)

    ## Make the plot
    plot(list_sum,main="Total emissions",xlab="Year",ylab="Total emissions",col="red",xaxt="n")
    axis(1,at=1:length(list_sum),labels=names(list_sum))
    
    ## Save the plot
    dev.off()
}
