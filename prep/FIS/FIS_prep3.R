library(readr)
library(readxl)
library(dplyr)
library(reshape2)

datac <- read_csv("prep/FIS/DESEMBARQUESPESQUEROS_SERNAPESCA_2000-2022_DATACENTER_2.csv")
#sp <- read_csv("prep/FIS/species.csv")
sp <- read_excel("prep/FIS/sp.xlsx")
sp<- select(sp, ESPECIE, pmm)
#sp$pmm<- as.character(sp$pmm)

datac$total<- rowSums(datac[,8:19], na.rm = T)
datac$ESPECIE<-toupper(datac$ESPECIE)

datap<- read_csv("prep/FIS/DESEMBARQUE TOTAL POR PUERTO 2000-2021_DATACENTER_2.csv")

datap$ESPECIE<-toupper(datap$ESPECIE)

####################
master_data <- read_excel("prep/FIS/master_data.xlsx")

dataz<- datap %>%
  select(c("COMUNA", "MACROZONA"))

dataz1<- datac %>%
  select(c("COMUNA", "MACROZONA"))

dataz<- rbind(dataz1, dataz)

dataz<- dataz[!duplicated(dataz), ]
dataz <- dataz[!is.na(dataz$MACROZONA),]

#### ARTESANAL  #####
#duplicados por comuna
datac_1<-datac %>%
  left_join(sp, by = c("ESPECIE")) %>%
  filter(AÑO %in% c(2012:2021),# pmm == "TRUE",
         COMUNA != "No IDENTIFICADO",
         COMUNA != "ISLA DE PASCUA",
         COMUNA != "JUAN FERNANDEZ") %>%
  group_by(COMUNA, ESPECIE, AÑO) %>%
  dplyr::summarise(sum = sum(total))

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
  dplyr::summarise(sum_2 = sum(TONELADAS))

data_art<- merge(datap_1,datac_1, all = T)

data_art2<- melt(data_art, id.vars = c("COMUNA","ESPECIE", "AÑO"))

data_art2<- select(data_art2, -c("variable"))
data_art2<- data_art2[!is.na(data_art2$value),]

data_art3<- data_art2[!duplicated(data_art2), ]


#######
#### fis_meancatch ####

comunas <- read_csv("comunas/spatial/regions_list.csv")
comunas<- mutate(comunas, "COMUNA" = toupper(comunas$rgn_name))
comunas<- select(comunas, rgn_id, COMUNA)
data1<-datac

data1<- data_art3
data1<- as.data.frame(data1)
data1["COMUNA"][data1["COMUNA"] == "CHAÑARAL"] <- "CHANARAL"#asignamos palena con chait?n
data1["COMUNA"][data1["COMUNA"] == "MELINKA"] <- "GUAITECAS"
data1["COMUNA"][data1["COMUNA"] == "ILOCA"] <- "LICANTEN"
data1["COMUNA"][data1["COMUNA"] == "PALENA"] <- "CHAITEN"
data1["COMUNA"][data1["COMUNA"] == "SAVEDRA"] <- "SAAVEDRA"
data1["COMUNA"][data1["COMUNA"] == "VIÑA DEL MAR"] <- "VINA DEL MAR"

data1<- merge(data1, comunas, all.x = T)

landings<- data1 %>% select(rgn_id, spp = "ESPECIE", catch= "value", year="AÑO")
landings <- landings[!is.na(landings$rgn_id),]

write.csv(landings, "comunas/layers/fis_meancatch_chl2023.csv",
          row.names = F,
          na = "")

####### tosti  #########

p <- read_csv("prep/FIS/DESEMBARQUE TOTAL POR PUERTO 2000-2021_DATACENTER_2.csv")

c <- read_csv("prep/FIS/DESEMBARQUESPESQUEROS_SERNAPESCA_2000-2022_DATACENTER_2.csv")

c1<- c %>%
  select(ESPECIE, COMUNA, AÑO)

c1<-mutate(c1, sp = toupper(ESPECIE))

c2<- c1 %>%
  select(sp, COMUNA, AÑO)


p1 <- p %>%
  select(ESPECIE, COMUNA, AÑO)

p1<-mutate(p1, sp = toupper(ESPECIE))


p2 <- p1 %>%
  select(sp, COMUNA, AÑO)


all<- rbind(c2, p2)

all<- all[!duplicated(all), ]


alls<- all %>%
  group_by(COMUNA, AÑO) %>%
  dplyr::summarise(n = n())


alls["COMUNA"][alls["COMUNA"] == "CHAÑARAL"] <- "CHANARAL"#asignamos palena con chait?n
alls["COMUNA"][alls["COMUNA"] == "MELINKA"] <- "GUAITECAS"
alls["COMUNA"][alls["COMUNA"] == "ILOCA"] <- "LICANTEN"
alls["COMUNA"][alls["COMUNA"] == "PALENA"] <- "CHAITEN"
alls["COMUNA"][alls["COMUNA"] == "SAVEDRA"] <- "SAAVEDRA"
alls["COMUNA"][alls["COMUNA"] == "VIÑA DEL MAR"] <- "VINA DEL MAR"


comunas <- read_csv("comunas/spatial/regions_list.csv")
comunas<- mutate(comunas, "COMUNA" = toupper(comunas$rgn_name))
comunas<- select(comunas, rgn_id, COMUNA)


alls2<- merge(datac, comunas, all.x = T)
alls2<- as.data.frame(alls2)


g<- data1 %>%
  filter(AÑO == 2021) %>%
  group_by(rgn_id) %>%
  summarise(total = sum(total)) %>%
  select(rgn_id, total)


write.table(g, "clipboard", sep="\t", row.names=F)
