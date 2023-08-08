library(dplyr)
library(httr)
library(rfishbase)
library(readxl)
library(readr)

###bases de datos ####

marine_spp <- read_csv("prep/_resilience/marine_spp.csv")
table(marine_spp$SPP)
marine_spp["SPP"][marine_spp["SPP"] == "SARDINA ESPA\xd1OLA"] <- "SARDINA ESPAÑOLA"
marine_spp["SPP"][marine_spp["SPP"] == "ATUN ALETA AMARILLA / KAHI AVE AVE"] <- "ATUN ALETA AMARILLA"
marine_spp["SPP"][marine_spp["SPP"] == "ATUN OJOS GRANDES / KAHI MATA TATA"] <- "ATUN OJOS GRANDES"
marine_spp["SPP"][marine_spp["SPP"] == "CABRILLA ESPA\xd1OLA"] <- "CABRILLA ESPAÑOLA"
marine_spp["SPP"][marine_spp["SPP"] == "TIBURON DE GALAPAGOS / MANGO"] <- "TIBURON DE GALAPAGOS"
marine_spp["SPP"][marine_spp["SPP"] == "CABRILLA ESPA\xd1OLA "] <- "CABRILLA ESPAÑOLA"
marine_spp["SPP"][marine_spp["SPP"] == "CABRILLA ESPA\xd1OLA "] <- "CABRILLA ESPAÑOLA"

nom <- read_excel("prep/_resilience/nom_sp.xlsx")

marine_spp<- merge(marine_spp, nom)
marine_spp <- marine_spp[!is.na(marine_spp$n_cientifico),]

marine_spp<- select(marine_spp, rgn_id, sp =n_cientifico)

sp_grid <- read_excel("prep/SPP/sp_grid.xlsx")

sp_grid<- sp_grid %>%
  select(rgn_id, sp)

sp_grid<- sp_grid[-(1:28),]


data<- rbind(marine_spp, sp_grid)

data<- data[!duplicated(data), ]

sp_grid <- read_excel("prep/SPP/sp_grid.xlsx")

prot<- read_excel("prep/_resilience/prot.xlsx",
                  sheet = "regiones") %>%
  select(rgn_id, region)


data<- merge(data, prot) %>%
  select(region, sp)

data<- data[!duplicated(data), ]

data1<- data %>%
  group_by(region) %>%
  summarise(n_sp = n())

regions_list <- read_csv("comunas/spatial/regions_list.csv")

data3<- regions_list %>%
  select(-rgn_name) %>%
  left_join(prot, by= "rgn_id") %>%
  group_by(region) %>%
  summarise(area = sum(area_km2)) %>%
  left_join(data1, by= "region") %>%
  mutate(riq = (n_sp/area)*10)

data3$i_riq<- rescale(data3$riq)

data4<- merge(data3, prot) %>%
  select(rgn_id, resilience_score = riq)

write.csv(data4, "comunas/layers/species_diversity_chl2023.csv", row.names = F, na = "")







