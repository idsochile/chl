library(readxl)
library(dplyr)
library(reshape2)


hab <- read_delim("prep/HAB/hab_status_trend_chl_2021.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

hab<- hab %>% select(-c("rgn_name")) %>%
  melt(id.vars = c("rgn_id", "habitat")) %>%
  select(rgn_id, habitat, year = "variable","value")

hab$year<- as.character(hab$year)
hab$year<- as.numeric(hab$year)

##superficie total de las comunas
sup<-hab %>% filter(year ==scenario_years)%>%
  group_by(rgn_id)%>%
  dplyr::summarise(total_km2= sum(value)) %>%
    dplyr::select(rgn_id, total_km2)


##Functions
hab<- merge(hab, sup)


##Punto de ref
p_ref<- hab %>%
  dplyr::mutate(p_ref = value/ total_km2)  %>%
  dplyr::group_by(habitat) %>%
  dplyr::summarise(ref = max(p_ref,  na.rm = TRUE)) %>%
  dplyr::select(habitat, ref)

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
hab<-merge(hab, sup)

scores_hab<- hab %>%
  dplyr::mutate(Cc= value/total_km2)  %>%
  dplyr::mutate(C= Cc/ref) %>%
  dplyr::group_by(rgn_id, year) %>%
  dplyr::summarise(c_sum = sum(C,  na.rm = TRUE)) %>%
  dplyr::full_join(com_h1, by= c("rgn_id"))%>%
  dplyr::mutate(status= (c_sum/n_h) *100)


##Status
scores_hab <- scores_hab %>%
  filter(year ==scenario_years) %>%
  mutate(dimension = 'status',
         score     = round(status, 4)) %>%
  mutate(goal = 'HAB')%>%
  select(region_id = "rgn_id", goal, dimension, score)


##Tendencia
#Debido a que el cambio de cobertura de los habitats no estan disponibles al momento de la realización de este indice
#Utilizaremos la tedencia 0, suponiendo que ese es el presente estudio es el habitat inicial

trend_data<- data.frame(region_id = c(1:36),
                        goal = c(rep("HAB", 36)),
                        dimension = c(rep("trend", 36)),
                        score = c(rep(0, 36)))



scores<- rbind(scores_hab, trend_data)





















