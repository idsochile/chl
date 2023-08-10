library(dplyr)
library(readr)
library(scales)
library(readxl)

########### ##########

citaciones <- read_excel("C:/Users/vapiz/Dropbox/Ohi/OHI+ CHILE/Metas/presiones/Citaciones_a_tribunales_2014_2023.xlsx",
                             sheet = "Sheet1")


c1<- citaciones %>%
  filter(Año >=2017,
         Año <= 2023,
         !(`TipoLugar` %in%  c("Rio", "Humedal", "Lago", "Laguna", "Estero", "-"))) %>%
  group_by(Nm_Comuna) %>%
  summarise(n = n())%>%
  rename( "rgn_name" = Nm_Comuna) %>%
  filter(n < 2000)

c1["rgn_name"][c1["rgn_name"] == "Aysén"] <- "Aysen"
c1["rgn_name"][c1["rgn_name"] == "Cabo de Hornos(Navarino)"] <- "Cabo de Hornos"
c1["rgn_name"][c1["rgn_name"] == "Chaitén"] <- "Chaiten"
c1["rgn_name"][c1["rgn_name"] == "Chañaral"] <- "Chanaral"
c1["rgn_name"][c1["rgn_name"] == "Cochamó"] <- "Cochamo"
c1["rgn_name"][c1["rgn_name"] == "Concón"] <- "Concon"
c1["rgn_name"][c1["rgn_name"] == "Constitución"] <- "Constitucion"
c1["rgn_name"][c1["rgn_name"] == "Hualaihué"] <- "Hualaihue"
c1["rgn_name"][c1["rgn_name"] == "La Unión"] <- "La Union"
c1["rgn_name"][c1["rgn_name"] == "Licantén"] <- "Licanten"
c1["rgn_name"][c1["rgn_name"] == "Maullín"] <- "Maullin"
c1["rgn_name"][c1["rgn_name"] == "Puchuncaví"] <- "Puchuncavi"
c1["rgn_name"][c1["rgn_name"] == "Puqueldón"] <- "Puqueldon"
c1["rgn_name"][c1["rgn_name"] == "Queilén"] <- "Queilen"
c1["rgn_name"][c1["rgn_name"] == "Quellón"] <- "Quellon"
c1["rgn_name"][c1["rgn_name"] == "Río Verde"] <- "Rio Verde"
c1["rgn_name"][c1["rgn_name"] == "Toltén"] <- "Tolten"
c1["rgn_name"][c1["rgn_name"] == "Tomé"] <- "Tome"
c1["rgn_name"][c1["rgn_name"] == "Valparaíso"] <- "Valparaiso"
c1["rgn_name"][c1["rgn_name"] == "Vichuquén"] <- "Vichuquen"
c1["rgn_name"][c1["rgn_name"] == "Viña del Mar"] <- "Vina del Mar"
c1["rgn_name"][c1["rgn_name"] == "El Quisco"] <- "El quisco"
c1["rgn_name"][c1["rgn_name"] == "La Higuera"] <- "La higuera"
c1["rgn_name"][c1["rgn_name"] == "El Tabo"] <- "El tabo"
c1["rgn_name"][c1["rgn_name"] == "La Serena"] <- "La serena"
c1["rgn_name"][c1["rgn_name"] == "La Ligua"] <- "La ligua"
c1["rgn_name"][c1["rgn_name"] == "La Union"] <- "La union"
c1["rgn_name"][c1["rgn_name"] == "Los Vilos"] <- "Los vilos"
c1["rgn_name"][c1["rgn_name"] == "San Pedro de la Paz"] <- "San pedro de la paz"
c1["rgn_name"][c1["rgn_name"] == "San Antonio"] <- "San antonio"
c1["rgn_name"][c1["rgn_name"] == "Santo Domingo"] <- "Santo domingo"
c1["rgn_name"][c1["rgn_name"] == "Savedra"] <- "Saavedra"
c1["rgn_name"][c1["rgn_name"] == "Vina del Mar"] <- "Vina del mar"





names <- read_csv("C:/github/chl/comunas/spatial/regions_list.csv")


c1<- merge(c1, names, all.x = T)

c1 <- c1[!is.na(c1$rgn_id),]

toneladasxcomuna <- read_excel("C:/R/OHI/data/toneladasxcomuna.xlsx")

c2<- merge(c1, toneladasxcomuna, all.x = T)
c2<- edit(c2)

c2$pressure_score<- c2$n/c2$total

data<- c2


########### ##########

data <- read_csv("C:/R/OHI/data/insost_pesca.csv")

data<- select(data, rgn_id, pressure_score)


data$pressure_score<- rescale_max(data$pressure_score)

names (data)[2] = "pressure_score"
data <- data[!is.na(data$rgn_id),]
data$pressure_score<- data$pressure_score / 100
#data<- merge(data, names)

#data<- select(data, rgn_id, pressure_score)


write.csv(data, "comunas/layers/des_habitat_costero_chl2023.csv", row.names = F)



##### #####
chl<- st_read("C:/R/OHI/shape/New Folder/Export_Output.shp") %>%
  as.data.frame() %>%
  select(rgn_name = COMUNA, area_km2)


data<- población_ohi_chile %>%
  select(rgn_name, rgn_id, pob = "2021") %>%
  left_join(chl, by = "rgn_name") %>%
  mutate(pressure_score = pob/area_km2) %>%
  select(rgn_id, pressure_score)


### sociales ####

list<- read_excel("prep/_pressures/Libro1.xlsx")


for (i in list$list) {
  ruta <- paste("comunas/layers/", i, sep = "")
  data<- read_csv(ruta)
  names (data)[2] = "pressure_score"
  data$pressure_score <- 1- data$pressure_score

  nombre<- paste("comunas/layers/pres", i, sep = "_")

  write.csv(data, nombre , row.names = F)
}




