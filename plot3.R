plot3 <- function(df) {

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
    
    ## Data for the plot: total emissions by year for baltimore (fips = 24510)
    subdf <- NEI[NEI$fips == "24510", ]
    groups <- group_by(subdf,type,year)
    sumdf <- summarise(groups,sum(Emissions))
    names(sumdf) <- c("type","year","Emissions")

    ## Make the plot
    png('plot3.png')
    ggplot(sumdf, aes(x=year,y=Emissions)) +
        geom_bar(stat='identity') +
        facet_wrap(~ type, nrow=1) + 
        labs(x = "Year", y = "Emissions (tons)", title = "Emissions by type in Baltimore City, MD")
    dev.off()
    
    ## ## this is a line plot (everything on the same plot)
    png('plot3bis.png')
    ggplot(sumdf, aes(x=year, y=Emissions, group = type, color=type, fill = type)) +
        geom_line(size=1.5) +
        geom_point(aes(shape=type), size=4)+
        scale_shape_manual(values=c(21,22,23,24)) +
        scale_colour_manual(values = cmap) +
        scale_fill_manual(values = cmap) +
        labs(x = "Year", y = "Emissions (tons)", title = "Emissions by type in Baltimore City, MD")
    dev.off()
}
