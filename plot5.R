plot5 <- function(df) {

    ## Load libraries
    library(dplyr)
    library(tidyr)
    library(ggplot2)

    ## Some default colors
    cmap_med <- c('#F15A60','#7AC36A','#5A9BD4','#FAA75B','#9E67AB','#CE7058','#D77FB4','#737373')
    cmap <- c('#EE2E2F','#008C48','#185AA9','#F47D23','#662C91','#A21D21','#B43894','#010202')
    
    ## Load the dataset using our custom load function
    NEI <- load_dataset()
    SCC <- readRDS("Source_Classification_Code.rds")

    ## We want motor vehicle related sources in the Baltimore
    ## We are going to assume it is basically just type=onroad
    subdf <- NEI[(NEI$fips == "24510") & (NEI$type == "ON-ROAD"), ]

    ## Join it with the SCC and get the vehicle type
    subdf <- subdf %>% 
        left_join(x=subdf,y=SCC,by="SCC") %>% 
        separate(EI.Sector,c("a","vehicle.type"),extra="merge",sep=" - On-Road ") %>%
        separate(vehicle.type,c("vehicle.type"),extra="drop",sep=" Vehicles")
    
    ## Group the data by year and vehicle type
    groups <- group_by(subdf,vehicle.type,year)
    sumdf <- summarise(groups,sum(Emissions))
    names(sumdf) <- c("vehicle.type","year","Emissions")

    ## Make the plot
    png('plot5.png')
    ggplot(sumdf, aes(x=year, y=Emissions, group = vehicle.type, color=vehicle.type, fill = vehicle.type)) +
        geom_line(size=1.5) +
        geom_point(aes(shape=vehicle.type), size=4)+
        scale_shape_manual(values=c(21,22,23,24)) +
        scale_colour_manual(values = cmap) +
        scale_fill_manual(values = cmap) +
        labs(x = "Year", y = "Emissions (tons)", title = "Emissions by vehicle type in Baltimore City, MD")

    ## Save the plot
    dev.off()
}
