library(readr)
library(readxl)
library(dplyr)
library(reshape2)

datac <- read_csv("prep/FIS/DESEMBARQUESPESQUEROS_SERNAPESCA_2000-2022_DATACENTER_2.csv")
sp <- read_csv("prep/FIS/species.csv")
sp$pmm<- as.character(sp$pmm)

datac$total<- rowSums(datac[,8:19])
datac$ESPECIE<-toupper(datac$ESPECIE)

datap<- read_csv("prep/FIS/DESEMBARQUE TOTAL POR PUERTO 2000-2021_DATACENTER_2.csv")

datap$ESPECIE<-toupper(datap$ESPECIE)

#duplicados por comuna
datac_1<-datac %>%
  left_join(sp, by = c("ESPECIE")) %>%
  filter(AÑO %in% c(2021), pmm == "TRUE",
         COMUNA != "No IDENTIFICADO",
         COMUNA != "ISLA DE PASCUA",
         COMUNA != "JUAN FERNANDEZ") %>%
  group_by(COMUNA, ESPECIE, AÑO) %>%
  summarise(sum = sum(total))

datap_1<- datap %>%
  left_join(sp, by = c("ESPECIE")) %>%
  #filter("TIPO AGENTE" %in% c("Industrial", "INDUSTRIAL")) %>%
  filter(`TIPO AGENTE` %in% c("Artesanal", "ARTESANAL"),
         COMUNA != c("DESCONOCIDO"),
         COMUNA != c("EXTRANJERO"),
         COMUNA != c("LANCHAS TRANSPORTADORES"),
         COMUNA != c("METROPOLITANA"),
         COMUNA != c("AGUAS INTERNACIONALES")) %>%
  filter(AÑO %in% c(2021), pmm == "TRUE") %>%
  group_by(COMUNA, ESPECIE, AÑO) %>%
  summarise(sum_2 = sum(TONELADAS))

data_art<- merge(datap_1, datac_1, all = T)

data_art2<- melt(data_art, id.vars = c("COMUNA","ESPECIE", "AÑO"))

data_art2<- select(data_art2, -c("variable"))
data_art2<- data_art2[!is.na(data_art2$value),]

data_art3<- data_art2[!duplicated(data_art2), ]


#### Considerar las 10 especies con mayor porcentaje por macrozona ####
##Pesca artesanal ####
dataz<- datap %>%
  select(c("COMUNA", "MACROZONA"))

dataz1<- datac %>%
  select(c("COMUNA", "MACROZONA"))

dataz<- rbind(dataz1, dataz)

dataz<- dataz[!duplicated(dataz), ]
dataz <- dataz[!is.na(dataz$MACROZONA),]

data_art4<-data_art3 %>%
  left_join(dataz, by = c("COMUNA")) %>%
  group_by(MACROZONA) %>%
  summarise(sum = sum(value, na.rm = T))

data_art5<- data_art3 %>%
  left_join(dataz, by = c("COMUNA")) %>%
  group_by(MACROZONA, ESPECIE) %>%
  summarise(sum_sp = sum(value, na.rm = T)) %>%
  left_join(data_art4, by = c("MACROZONA")) %>%
  mutate(per = c((sum_sp/sum)*100))

data_art6<- data.frame()
for (i in dataz$MACROZONA) {
  d<- data_art5 %>% filter(MACROZONA %in% i ) %>%
    arrange( desc(per) )%>%
    mutate(ac = cumsum (per))%>%
    filter(ac <= 95)%>%
    as.data.frame()
  data_art6<- rbind(data_art6,d)
}

spart<-select(data_art6, MACROZONA, ESPECIE)
spart<- spart[!duplicated(spart), ]

spart<- cbind(spart, y = 2021)
spart1<- cbind(spart1, y = 2017)

spart<-rbind(spart, spart1)

write.csv(spart, "prep/FIS/spart.csv", row.names = F)

##Pesca industrial ####
datap_1<-datap %>%
  left_join(sp, by = c("ESPECIE")) %>%
  filter(`TIPO AGENTE` %in% c("Industrial", "INDUSTRIAL"),
         COMUNA != c("DESCONOCIDO"),
         COMUNA != c("EXTRANJERO"),
         COMUNA != c("LANCHAS TRANSPORTADORES"),
         COMUNA != c("METROPOLITANA"),
         COMUNA != c("AGUAS INTERNACIONALES"))%>%
  #filter(`TIPO AGENTE` %in% c("Artesanal", "ARTESANAL")) %>%
  filter(AÑO %in% c(2021), pmm %in% "TRUE")%>%
  group_by(MACROZONA) %>%
  summarise(sum = sum(TONELADAS, na.rm = T))

datap_2<- datap %>%
    left_join(sp, by = c("ESPECIE")) %>%
    filter(`TIPO AGENTE` %in% c("Industrial", "INDUSTRIAL"),
                                COMUNA != c("DESCONOCIDO"),
                                COMUNA != c("EXTRANJERO"),
                                COMUNA != c("LANCHAS TRANSPORTADORES"),
                                COMUNA != c("METROPOLITANA")) %>%
    #filter(`TIPO AGENTE` %in% c("Artesanal", "ARTESANAL")) %>%
    filter(AÑO %in% c(2021), pmm == "TRUE") %>%
    group_by(MACROZONA, ESPECIE) %>%
    summarise(sum_2 = sum(TONELADAS))%>%
    left_join(datap_1, by = c("MACROZONA"))%>%
    mutate(per = c((sum_2/sum)*100))

data_puer<- data.frame()
for (i in dataz$MACROZONA) {
  d<- datap_2 %>% filter(MACROZONA %in% i) %>%
    arrange( desc(per) )%>%
    mutate(ac = cumsum (per))%>%
    filter(ac <= 99.9)%>%
    as.data.frame()
  data_puer<- rbind(data_puer,d)
}

spind<-select(data_puer, MACROZONA, ESPECIE)
spind<- spind[!duplicated(spind), ]

spind<- cbind(spind, y = 2021)
spind1<- cbind(spind1, y = 2017)

spind<- rbind(spind, spind1)

write.csv(spind, "prep/FIS/spind.csv", row.names = F)

species<- rbind(spart, spind)
species<- select(species, MACROZONA, ESPECIE)
species<- species[!duplicated(species), ]


res_species<- merge(species, master_data, all.x = T)
write.csv(res_species, "res_species.csv", row.names = F, na = "")

####################
master_data <- read_excel("prep/FIS/master_data.xlsx")

#### ARTESANAL  #####
#duplicados por comuna
datac_1<-datac %>%
  left_join(sp, by = c("ESPECIE")) %>%
  filter(AÑO %in% c(2012:2021), pmm == "TRUE",
         COMUNA != "No IDENTIFICADO",
         COMUNA != "ISLA DE PASCUA",
         COMUNA != "JUAN FERNANDEZ") %>%
  group_by(COMUNA, ESPECIE, AÑO) %>%
  summarise(sum = sum(total))

datap_1<- datap %>%
  left_join(sp, by = c("ESPECIE")) %>%
  #filter("TIPO AGENTE" %in% c("Industrial", "INDUSTRIAL")) %>%
  filter(`TIPO AGENTE` %in% c("Artesanal", "ARTESANAL"),
         COMUNA != c("DESCONOCIDO"),
         COMUNA != c("EXTRANJERO"),
         COMUNA != c("LANCHAS TRANSPORTADORES"),
         COMUNA != c("METROPOLITANA"),
         COMUNA != c("AGUAS INTERNACIONALES")) %>%
  filter(AÑO %in% c(2012:2021), pmm == "TRUE") %>%
  group_by(COMUNA, ESPECIE, AÑO) %>%
  summarise(sum_2 = sum(TONELADAS))

data_art<- merge(datap_1,datac_1, all = T)

data_art2<- melt(data_art, id.vars = c("COMUNA","ESPECIE", "AÑO"))

data_art2<- select(data_art2, -c("variable"))
data_art2<- data_art2[!is.na(data_art2$value),]

data_art3<- data_art2[!duplicated(data_art2), ]

#INDUSTRIAL
datap_1<-datap %>%
  left_join(sp, by = c("ESPECIE")) %>%
  filter(`TIPO AGENTE` %in% c("Industrial", "INDUSTRIAL"),
         COMUNA != c("DESCONOCIDO"),
         COMUNA != c("EXTRANJERO"),
         COMUNA != c("LANCHAS TRANSPORTADORES"),
         COMUNA != c("METROPOLITANA"),
         COMUNA != c("AGUAS INTERNACIONALES"))%>%
  #filter(`TIPO AGENTE` %in% c("Artesanal", "ARTESANAL")) %>%
  filter(AÑO %in% c(2012:2021), pmm == "TRUE")%>%
  group_by(COMUNA, ESPECIE, AÑO)%>%
  summarise(value2 = sum(TONELADAS))

##MERGE
data1<- data1 %>% select(COMUNA, MACROZONA, id, stock_id = "Taxon", res, ct, yr = "AÑO")

data1["COMUNA"][data1["COMUNA"] == "CHAÑARAL"] <- "CHANARAL"#asignamos palena con chait?n
data1["COMUNA"][data1["COMUNA"] == "MELINKA"] <- "GUAITECAS"
data1["COMUNA"][data1["COMUNA"] == "ILOCA"] <- "LICANTEN"
data1["COMUNA"][data1["COMUNA"] == "PALENA"] <- "CHAITEN"
data1["COMUNA"][data1["COMUNA"] == "SAVEDRA"] <- "SAAVEDRA"
data1["COMUNA"][data1["COMUNA"] == "VIÑA DEL MAR"] <- "VINA DEL MAR"

data<- data1[!is.na(data1$res),]
write.csv(data, "prep/FIS/data_bmsy_na.csv", row.names = F, na = "")

##Despues de pasar por MasterScript

my_1 <- read_csv("my_bbmsy_relaxed_1.csv")

my_2 <- read_csv("my_bbmsy_relaxed_9.csv")

my_bbmsy<- rbind(my_bbmsy, my_2)


write.csv(my_bbmsy, "comunas/layers/fis_b_bmsy_chl2021.csv",
          row.names = F,
          na = "")

#######
#### fis_meancatch ####

comunas <- read_csv("comunas/spatial/regions_list.csv")

data1<- data_art3

data1<- merge(data1, master_data)
data1<- merge(data1, dataz)

landings<- data1 %>% select(COMUNA, MACROZONA, Spp = "ESPECIE", catch= "value", year="AÑO")

landings1 <- merge(landings, rgn, all.x = T)
landings1 <- landings1[!is.na(landings1$rgn_id),]

landings2<- landings1 %>% select(rgn_id, Spp, year, catch, MACROZONA)


write.csv(landings2, "comunas/layers/fis_meancatch_chl2021.csv",
          row.names = F,
          na = "")


##################
produccion <- read_excel("prep/NP/2021_040103_materia_prima_y_produccion_por_puerto.xlsx", sheet = "ARICA")


ZONAS<- filter(master_data, MACROZONA == "AUSTRAL")
COMUNA<- filter(dataz, MACROZONA == "AUSTRAL")


p_total<- data.frame()
for (i in COMUNAS5$COMUNA) {

produccion <- read_excel("prep/NP/2018materia_prima_y_produccion_por_puerto.xlsx", sheet = i)

p<- produccion %>%
  filter(`CHILE, MATERIA PRIMA Y PRODUCCIÓN AÑO 2018` %in% c(ZONAS$ESPECIE),
         ...2 == "M") %>%
  select(ESPECIE = `CHILE, MATERIA PRIMA Y PRODUCCIÓN AÑO 2018`,
         L9 = ...10, L10 = ...11 )%>%
  mutate(COMUNA = i)

p_total<- rbind(p_total, p)

}

p_total1<- p_total
p_total2<-p_total
p_total3<- p_total
p_total4<- p_total
p_total5<- p_total

p_total2018<- rbind(p_total1, p_total2, p_total3, p_total4, p_total5)
p_total2018<- cbind(p_total2018, year = 2018)

p_total<- rbind(p_total2018, p_total2019, p_total2020, p_total2021)


write.csv(p_total, "p_total.csv", row.names = F)


#############
####Descontar
library(reshape2)

p_total <- read_csv("prep/FIS/p_total.csv")
rgn <- read_csv("comunas/spatial/regions_list.csv")


p_total["L9"][p_total["L9"] == "-"] <- "0"
p_total["L10"][p_total["L10"] == "-"] <- "0"
p_total$L9<- as.numeric(p_total$L9)
p_total$L10<- as.numeric(p_total$L10)


p_total["COMUNA"][p_total["COMUNA"] == "PTO. NATALES"] <- "NATALES"
p_total["COMUNA"][p_total["COMUNA"] == "SAN VICENTE"] <- "TALCAHUANO"

p_total$catch2<- p_total$L9+p_total$L10

rgn$COMUNA<- toupper(rgn$rgn_name)
p_total2<- merge(p_total, rgn, all.x = T)
master_data<- select(master_data, ESPECIE, Taxon)
master_data<-master_data[!duplicated(master_data), ]
p_total2<- merge(p_total2, master_data, all.x = T)

p_total3<- select(p_total2, rgn_id, sciname="Taxon", year, catch2)

P_total4<- merge(fis, p_total3)

P_total4$catch3<- P_total4$catch - P_total4$catch2



fis_b_bmsy_chl2023 <- merge(fis_b_bmsy_chl2023, master_data)


fis_b_bmsy_chl2023<- select(fis_b_bmsy_chl2023, Spp = "ESPECIE", b_bmsy,
                            year, MACROZONA)

dataz1<- merge(fis_b_bmsy_chl2023,dataz)


dataz1["COMUNA"][dataz1["COMUNA"] == "CHAÑARAL"] <- "CHANARAL"#asignamos palena con chait?n
dataz1["COMUNA"][dataz1["COMUNA"] == "MELINKA"] <- "GUAITECAS"
dataz1["COMUNA"][dataz1["COMUNA"] == "ILOCA"] <- "LICANTEN"
dataz1["COMUNA"][dataz1["COMUNA"] == "PALENA"] <- "CHAITEN"
dataz1["COMUNA"][dataz1["COMUNA"] == "SAVEDRA"] <- "SAAVEDRA"
dataz1["COMUNA"][dataz1["COMUNA"] == "VIÑA DEL MAR"] <- "VINA DEL MAR"

dataz1<- merge(dataz1, rgn, all.x = T)

dataz1<-select(dataz1, rgn_id, Spp , b_bmsy,
               year, MACROZONA)

write.csv(fis_b_bmsy_chl2023, "comunas/layers/fis_b_bmsy_chl2023.csv",
          row.names = F,
          na = "")


#######################
##fp_wildcaught_weight

mar <- read_csv("comunas/layers/mar_harvest_tonnes_chl2023.csv")
fis <- read_csv("comunas/layers/fis_meancatch_chl2023.csv")

fp<- fis %>%
  group_by(rgn_id, year) %>%
  dplyr::summarise(fis = sum(catch))

fp2<- mar %>%
  group_by(rgn_id, year) %>%
  dplyr::summarise(mar = c(sum(tonnes))/1000)

fp3<- fp %>%
  left_join(fp2, by= c("rgn_id", "year")) %>%
  filter(year %in% c(2017:2021))

fp3$total <- rowSums(fp3[, c(3,4)], na.rm = T)

fp4<- fp3 %>%
  mutate(w_fis = fis/total) %>%
  select(rgn_id, year, w_fis)


write.csv(fp4, "comunas/layers/fp_wildcaught_weight.csv",
          row.names = F)

write.csv(fp_wildcaught_weight_chl2023, "comunas/layers/fp_wildcaught_weight_chl2023.csv",
          row.names = F)
