plot6 <- function(df) {

    ## Load libraries
    library(dplyr)
    library(tidyr)
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

    ## We want motor vehicle related sources in the Baltimore and LA
    ## We are going to assume it is basically just type=onroad
    subdf <- NEI[((NEI$fips == "24510") | (NEI$fips == "06037")) & (NEI$type == "ON-ROAD"), ]

    ## Join it with the SCC and get the vehicle type
    subdf <- subdf %>% 
        left_join(x=subdf,y=SCC,by="SCC") %>% 
        separate(EI.Sector,c("a","vehicle.type"),extra="merge",sep=" - On-Road ") %>%
        separate(vehicle.type,c("vehicle.type"),extra="drop",sep=" Vehicles") %>%
        mutate(fips=replace(as.character(fips), as.character(fips)=="24510", "Baltimore")) %>%
        mutate(fips=replace(as.character(fips), as.character(fips)=="06037", "Los Angeles")) %>%
        mutate(fips=as.factor(fips))
    
    ## Group the data by year and vehicle type
    groups <- group_by(subdf,fips,vehicle.type,year)
    sumdf <- summarise(groups,sum(Emissions))
    names(sumdf) <- c("location","vehicle.type","year","Emissions")

    ## Make the plot
    png('plot6.png')
    ggplot(sumdf, aes(x=year, y=Emissions, group = location, color=location, fill = location)) +
        geom_line(size=1.5) +
        geom_point(aes(shape=location), size=4)+
        scale_shape_manual(values=c(21,22,23,24)) +
        scale_colour_manual(values = cmap) +
        scale_fill_manual(values = cmap) +
        facet_grid(. ~ vehicle.type) +
        labs(x = "Year", y = "Emissions (tons)", title = "Emissions by vehicle type (Baltimore vs Los Angeles)") 
    

    ## Save the plot
    dev.off()
}
