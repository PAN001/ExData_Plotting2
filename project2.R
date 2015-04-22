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
  
  #Q3:
  # 多变量分类！
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

    
  
  
  