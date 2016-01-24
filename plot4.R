plot4 <- function(df) {

    ## Load libraries
    library(dplyr)
    library(ggplot2)

    ## Some default colors
    cmap_med <- c('#F15A60','#7AC36A','#5A9BD4','#FAA75B','#9E67AB','#CE7058','#D77FB4','#737373')
    cmap <- c('#EE2E2F','#008C48','#185AA9','#F47D23','#662C91','#A21D21','#B43894','#010202')
    
    ## Load the dataset
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI$fips <- as.factor(NEI$fips)
    NEI$SCC <- as.factor(NEI$SCC)
    NEI$Pollutant <- as.factor(NEI$Pollutant)
    NEI$type <- as.factor(NEI$type)
    NEI$year <- as.factor(NEI$year)
    SCC <- readRDS("Source_Classification_Code.rds")
   
    ## We want coal combustion related sources in the US
    ## First get the index matching combustion and coal
    idx <- grepl("(?=.*Comb)(?=.*Coal)",SCC$Short.Name,perl=TRUE)

    ## Next get the subset of SCC corresponding to this index
    subscc <- SCC[idx,]
    
    ## Lets combine the two but only keep the rows of NEI that match
    ## the SCC value in our new subset of SCC (use semi_join for this)
    subdf <- semi_join(x=NEI,y=subscc,by="SCC")
    groups <- group_by(subdf,year)
    sumdf <- summarise(groups,sum(Emissions))
    names(sumdf) <- c("year","Emissions")

    ## Make the plot
    png('plot4.png')
    ggplot(sumdf, aes(x=year, y=Emissions, group=1)) +
        geom_line(size=1.5, color = cmap[1]) +
        geom_point(size=4, color = cmap[1])+
        labs(x = "Year", y = "Emissions (tons)", title = "Emissions from coal combustion")

    ## ## Boxplot if we want (but there is too much spread for good info)
    ## ggplot(subdf, aes(x=year, y=Emissions)) +
    ##     geom_boxplot()+
    ##     labs(x = "Year", y = "Emissions (tons)", title = "Emissions from coal combustion")

    ## Save the plot
    dev.off()
}
