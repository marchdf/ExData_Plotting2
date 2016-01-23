load_dataset <- function() {

    ## Load the data
    NEI <- readRDS("summarySCC_PM25.rds")
    
    ## Format it
    NEI$fips <- as.factor(NEI$fips)
    NEI$SCC <- as.factor(NEI$SCC)
    NEI$Pollutant <- as.factor(NEI$Pollutant)
    NEI$type <- as.factor(NEI$type)
    NEI$year <- as.factor(NEI$year)

    return(NEI)
}

