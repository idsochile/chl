library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)


data <- read_excel("prep/CW/Marine_nutrient_pollution.xlsx")


data[,3]<- rescale_max(data[,3])
data[,4] <- rescale_max(data[,4])
data[,5]<- rescale_max(data[,5])
data[,6] <- rescale_max(data[,6])
data[,7]<- rescale_max(data[,7])

data<- data[,-2]


#Ordenar la tabla en formato largo
pres<-melt(data, id.vars = c("rgn_id"))


pres<- rename(pres, year = "variable")
pres$year<- as.character(pres$year)
pres$year<- as.numeric(pres$year)

#PO
pres<- rename(pres, pressure_score = "value")
pres_po<- pres %>%
  filter(year == 2021) %>%
  select(rgn_id, pressure_score)

pres_po <- pres_po[!is.na(pres_po$pressure_score),]

write.csv(pres_po, "comunas/layers/cw_connutrientesmar_chl2023.csv", row.names = F)

#Tendencia
pres <- pres[!is.na(pres$pressure_score),]
pres_trend <- data.frame()
for (i in unique(pres$rgn_id)) {
  t<- pres %>% filter(rgn_id == i)
  mdl = lm(pressure_score ~ year, data = t)
  trend =  coef(mdl)[['year']] * sd(pres$year) / sd(pres$pressure_score)
  sector_trend = pmax(-1, pmin(1, trend * 5))
  d<- data.frame(halpern.trend = sector_trend, coef.Beta0 = coef(mdl)[['(Intercept)']],  coef.year = coef(mdl)[['year']])
  trend = data.frame(rgn_id = i, trend = d$halpern.trend)
  pres_trend = rbind(pres_trend, trend)
}

pres_trend<- cbind(pres_trend, "year" = c(rep(2021, nrow(pres_trend))))
pres_trend<- select(pres_trend, rgn_id, year, trend)


write.csv(pres_trend, "comunas/layers/cw_connutrientesmar_trend_chl2021.csv", row.names = F)


##########


prs = c('po_pathogen' = 'a',
        'po_nutrients_3nm' = 'u',
        'po_chemical' = 'l',
        'po_trash'     = 'd',
        'po_pathogens_fan' = 'f')

trends<-c('cw_nutrient_trend'  = 'fert_trend',
          'cw_coastalpopn_trend' = 'popn_trend',
          'cw_pathogen_trend'    = 'path_trend')



p = SelectLayersData(layers, layers=names(prs))
pres_data1<-  p %>% filter(layer %in% c('po_pathogen', 'po_pathogens_fan')) %>%
  group_by(id_num) %>%
  dplyr::summarise(value= mean(val_num)) %>%
  select(region_id = id_num, value )


pres_data<- p %>% filter(layer %in% c('po_nutrients_3nm' , 'po_chemical',
                                      'po_trash' )) %>%
  select(region_id = id_num, value = val_num) %>%
  rbind(pres_data1)

t = SelectLayersData(layers, layers=names(trends))
trend_data<- t %>% select(region_id = id_num, value = val_num)



