library(readxl)
library(dplyr)
library(reshape2)
library(readr)

#Algas
#Pastos
#Marismas y Humedales
#Bosques y Mat
#Playas y Dunas
#bentónico

hab <- read_excel("prep/HAB/hab.xlsx", sheet = "bentónico")

hab<- hab[,c(1,4:8)]

hab<- hab %>%
  melt(id.vars = c("rgn_id")) %>%
  mutate(habitat = "Bentonico") %>%
  select(rgn_id, habitat, year = "variable","value")

hab$year<- as.character(hab$year)
hab$year<- as.numeric(hab$year)

#hab1<- hab
#hab2<- hab
#hab3<- hab
#hab4<- hab
#hab5<- hab
#hab6<- hab

hab<- rbind(hab1, hab2, hab3, hab4, hab5, hab6)
hab <- hab[!is.na(hab$value),]


write.csv(hab, "comunas/layers/hab_extension_chl2023.csv", row.names = F, na = "")

##superficie total de las comunas
area <- read_excel("prep/HAB/hab.xlsx", sheet = "Playas y Dunas") %>%
  select(rgn_id, area_km2)

write.csv(area, "comunas/layers/hab_area_chl2023.csv", row.names = F, na = "")

##Functions
hab<- merge(hab, area)

##Punto de ref
p_ref<- hab %>%
  dplyr::mutate(por = value/ area_km2)  %>%
  dplyr::group_by(rgn_id, habitat) %>%
  dplyr::summarise(ref = max(por,  na.rm = TRUE)) %>%
  dplyr::select(rgn_id, habitat, ref)

## Numero de habitats
com_hab <- hab[!is.na(hab$value),]
com<- filter(com_hab, rgn_id == 1)
com_h1<-data.frame( rgn_id= 1,
                    n_h = nrow(table(com$habitat)))
for (i in c(2:103)) {
  com<- filter(com_hab, rgn_id == i)
  com_h<-data.frame(rgn_id= i,
                    n_h = nrow(table(com$habitat)))
  com_h1<- rbind(com_h1, com_h)
}
com_h1 <- com_h1[!is.na(com_h1$n_h),]


##Scores
hab<- merge(com_hab, p_ref)
hab<-merge(hab, area)

scores_hab<- hab %>%
  dplyr::mutate(Cc= value/area_km2)  %>%
  dplyr::mutate(C= Cc/ref) %>%
  dplyr::group_by(rgn_id, year) %>%
  dplyr::summarise(c_sum = sum(C,  na.rm = TRUE)) %>%
  dplyr::full_join(com_h1, by= c("rgn_id"))%>%
  dplyr::mutate(status= (c_sum/n_h) *100)


##Status
status_hab <- scores_hab %>%
  filter(year ==scenario_years) %>%
  mutate(dimension = 'status',
         score     = round(status, 4)) %>%
  mutate(goal = 'HAB')%>%
  select(region_id = "rgn_id", goal, dimension, score)


##Tendencia
trend_years <- (scen_year - 4):(scen_year)

r.trend <-
  CalculateTrend(status_data = scores_hab, trend_years = trend_years)


scores<- r.trend %>%
  mutate(goal = 'HAB') %>%
  rbind(status_hab)




boolean<- hab %>%
  filter(year == 2021) %>%
  select(rgn_id, habitat) %>%
  mutate(boolean = 1)


write.csv(boolean,"comunas/layers/element_wts_hab_pres_abs_chl2023.csv", row.names = F, na = "")














