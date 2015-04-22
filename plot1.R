project2 <- function() {
  library("dplyr")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # Q1:
  op <- par(mar = rep(0, 4))   
  plot.new()
  par(op)
  data_part1 <- filter(NEI, year == 1999 || year == 2002 || year == 2005 || year == 2008)
  result_part1 <- data_part1 %>%
    group_by(year, add = TRUE) %>%
    mutate(sum = sum(Emissions)) %>%
    select(year, sum)
  result_part1 <- distinct(result_part1)  
  barplot(result_part1$sum)
  dev.copy(png, file = "plot1.png")
  dev.off()
}