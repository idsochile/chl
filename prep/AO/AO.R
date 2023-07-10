library(readr)



op_pesca <- read_csv("comunas/layers/ao_scores.csv")

write.csv(op_pesca, "comunas/layers/ao_scores.csv", row.names = F)

names (op_pesca)[3] = "year"
names (op_pesca)[4] = "status"
op_pesca<- op_pesca[!is.na(op_pesca$status),]

regions_list <- read_csv("comunas/spatial/regions_list.csv")

op_pesca<- merge(op_pesca, regions_list)
op_pesca<- op_pesca %>%
  select(rgn_id, year, status)

op_pesca$status<- op_pesca$status*100

trend_years <- (scen_year - 4):(scen_year)

trend <-
  CalculateTrend(status_data = op_pesca, trend_years = trend_years)

trend<- as.data.frame(trend)

op_pesca<- op_pesca %>%
  mutate(dimension = "status") %>%
  filter(year == 2021 ) %>%
  select(region_id, score = status , dimension)

op_pesca$score<- op_pesca$score*100

trend<- trend %>%
  mutate(dimension = "trend")

scores<- rbind(op_pesca, trend)

regions_list<- as.data.frame(regions_list)
regions_list<- edit(regions_list)

scores<- scores %>%
  mutate(goal = "AO")

scores<- merge(scores, regions_list)

scores<- scores %>%
  select(rgn_id, goal, dimension, score)

write.table(scores, "clipboard", sep="\t", row.names=F)
