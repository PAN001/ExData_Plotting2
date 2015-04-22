project2 <- function() {
  library("dplyr")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #Q6:
  check_for_motor <- grepl("Motor", SCC$Short.Name)
  targeted_scc <- SCC[check, "SCC"]
  balt <- NEI$SCC%in%argeted_scc & NEI$fips == "24510"
  la <- NEI$SCC%in%argeted_scc & NEI$fips == "06037"
  is_targeted <- balt | la
  data_part6 <- NEI[is_targeted, c("fips", "year", "Emissions")]
  result_part6 <- data_part6 %>%
    group_by(fips, year, add = TRUE) %>%
    mutate(sum = sum(Emissions)) %>%
    select(fips, year, sum)
  result_part6 <- distinct(result_part6)
  qplot(year, sum, data = result_part6, color = fips)
  dev.copy(png, file = "plot6.png")
  dev.off()
}