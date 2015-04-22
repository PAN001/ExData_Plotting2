project2 <- function() {
  library("dplyr")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #Q5:
  check_for_motor <- grepl("Motor", SCC$Short.Name)
  targeted_scc <- SCC[check, "SCC"]
  is_targeted <- NEI$SCC%in%argeted_scc & NEI$fips == "24510"
  data_part5 <- NEI[is_targeted, c("year", "Emissions")]
  result_part5 <- data_part5 %>%
    group_by(year, add = TRUE) %>%
    mutate(sum = sum(Emissions))
  result_part5 <- distinct(result_part5)
  qplot(year, sum, data = result_part5)
  dev.copy(png, file = "plot5.png")
  dev.off()
}