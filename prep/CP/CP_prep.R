library(dplyr)
library(tidyr)
library(reshape2)
library(readxl)

#### Extensión ####

data <- read_excel("prep/CP/data_prep_CP.xlsx")
data<- data[,c(1:8)]

data1<- melt(data, id.vars = c("rgn_id", "rgn_name", "habitat" ))

names (data1)[4] = "year"
names (data1)[5] = "area_km2"
data1$year<- as.character(data1$year)
data1$year<- as.numeric(data1$year)
data1<- select(data1, -"rgn_name")

write.csv(data1, "comunas/layers/cp_habitat_extent_chl2023.csv", row.names = F, na = "")


#### Tendencia ####

# "Macrocystis" "Bosques y matorrales" "Marismas y humedales" "Playas y dunas"
hab_trend<- data.frame()

pres<- data1 %>%  filter(habitat == "Marismas y humedales")
pres <- na.omit(pres)
list<- data.frame(table(pres$rgn_id))

trend_hab <- data.frame()
for (i in c(list$Var1)) {
  t<- pres %>% filter(rgn_id == i)
  mdl = lm(area_km2 ~ year, data = t)
  trend =  coef(mdl)[['year']] * sd(pres$year) / sd(pres$area_km2)
  sector_trend = pmax(-1, pmin(1, trend * 5))
  d<- data.frame(halpern.trend = sector_trend, coef.Beta0 = coef(mdl)[['(Intercept)']],  coef.year = coef(mdl)[['year']])
  trend = data.frame(rgn_id = i, trend = d$halpern.trend)
  trend_hab = rbind(trend_hab, trend)
}

trend_hab<- trend_hab %>% mutate(habitat = "Marismas y humedales")%>%
  select(rgn_id, habitat, trend)

hab_trend<- rbind(hab_trend, trend_hab)

############
hab_trend$rgn_id<- as.numeric(hab_trend$rgn_id)


write.csv(hab_trend, "comunas/layers/cp_habitat_trend_chl2023.csv", row.names = F, na = "")


#### health ####
data <- read_excel("prep/CP/health.xlsx")

data1<- melt(data, id.vars = c("rgn_id", "rgn_name", "habitat" ))

names (data1)[4] = "year"
names (data1)[5] = "health"
data1$year<- as.character(data1$year)
data1$year<- as.numeric(data1$year)
data1<- select(data1, -"rgn_name")

write.csv(data1, "comunas/layers/cp_habitat_health_chl2023.csv", row.names = F, na = "")












