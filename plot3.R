project2 <- function() {
  library("dplyr")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #Q3:
  library(ggplot2)
  data_part3 <- filter(NEI, fips == "24510")
  result <- data_part3 %>%
    group_by(year, type, add = TRUE) %>%
    mutate(sum = sum(Emissions)) %>%
    select(type, year, sum)
  result <- distinct(result)
  result$year <- as.factor(result$year)
  result$type <- as.factor(result$type)
  qplot(year, sum, data = result, facets = .~type)
  dev.copy(png, file = "plot3.png")
  dev.off()
}