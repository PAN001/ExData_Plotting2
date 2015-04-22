project2 <- function() {
  library("dplyr")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # Q2:
  data_part2 <- filter(NEI, fips == "24510")
  result_part2 <- data_part2%>%
    group_by(year, add = TRUE) %>%
    mutate(sum = sum(Emissions)) %>%
    select(year, sum)
  result_part2 <- distinct(result_part2)  
  barplot(result_part2$sum)
  dev.copy(png, file = "plot2.png")
  dev.off()
}