project2 <- function() {
  library("dplyr")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #Q4:
  check <- grepl("Coal|Vehicle|Motor", SCC$Short.Name)
  targeted_scc <- SCC[check, "SCC"]
  data_part4 <- NEI[NEI$SCC%in%targeted_scc, c("year", "Emissions")]
  result_part4 <- data_part4 %>%
    group_by(year, add = TRUE) %>%
    mutate(sum = sum(Emissions))
  result_part4 <- distinct(result_part4)
  qplot(year, sum, data = result_part4)
  dev.copy(png, file = "plot4.png")
  dev.off()
}