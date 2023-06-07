library(readr)
library(dplyr)
library(reshape2)

###
##Paso 1.
###Encontrar las especies que representan a menos el 60% de la extracción de pesca en Chile
data1<- read.csv("prep/FIS/DESEMBARQUE TOTAL POR PUERTO 2000-2021.txt", sep = ";",
                 dec = ",", header = F)
data2<- read.csv("prep/FIS/DESEMBARQUE ARTESANAL POR CALETA 2000-2021.txt", sep = ";",
                 dec = ",", header = F)

caleta<- read_excel("prep/FIS/master_data.xlsx", sheet = "caletas")
puerto<- read_excel("prep/FIS/master_data.xlsx", sheet = "puertos")

data1<- rename(data1,
               AÑO = V1,
               REGION = V2,
               Puerto = V3,
               MES = V4,
               ESPECIE = V5,
               TONELADAS = V6,
               TIPO = V7)

data2<- rename(data2,
               AÑO = V1,
               REGION = V2,
               TIPO = V3,
               Caleta = V4,
               ESPECIE = V5)

data2$total<- rowSums(data2[,6:17],na.rm = T)


data1<- data1 %>%
  filter(TIPO == c("Industrial", "INDUSTRIAL")) %>%
  group_by(AÑO, ESPECIE) %>%
  summarise(sum = sum(TONELADAS)) %>%
  filter(AÑO == 2021) %>%
  mutate(per1 = (sum/c(sum(sum)))*100) %>%
  as.data.frame()%>%
  select( ESPECIE, sum1 = sum, per1)


data2<- data2 %>% group_by(AÑO, ESPECIE) %>%
  summarise(sum = sum(total)) %>%
  filter(AÑO == 2021) %>%
  mutate(per2 = (sum/c(sum(sum)))*100)%>%
  as.data.frame() %>%
  select(ESPECIE, sum2 = sum, per2)



data<- merge(data1, data2, all.x = T)
data$sum1<- as.numeric(data$sum1)
data$total<- rowSums(data[,c(2,4)], na.rm = T)


#nrow(data[duplicated(data[,c(2,4)],), ])

data<- data %>%
  mutate(per3 = c((total/sum(total))*100))%>%
  filter(per3 >= 1)
#97.28924% de la pesca en Chile durante el 2021

##Paso 2.
###Tabla de producción por año

data1<- read.csv("prep/FIS/DESEMBARQUE TOTAL POR PUERTO 2000-2021.txt", sep = ";",
                 dec = ",", header = F)


data2<- read.csv("prep/FIS/DESEMBARQUE ARTESANAL POR CALETA 2000-2021.txt", sep = ";",
                 dec = ",", header = F)


data1<- rename(data1,
               AÑO = V1,
               REGION = V2,
               PUERTO = V3,
               MES = V4,
               ESPECIE = V5,
               TONELADAS = V6,
               TIPO = V7)

data2<- rename(data2,
               AÑO = V1,
               REGION = V2,
               TIPO = V3,
               CALETA = V4,
               ESPECIE = V5)

data2$total<- rowSums(data2[,6:17])


data1<- data1 %>%
  filter(TIPO == c("Industrial", "INDUSTRIAL")) %>%
  group_by(AÑO, ESPECIE) %>%
  summarise(sum_c = sum(TONELADAS))


data2<- data2 %>% group_by(AÑO, ESPECIE) %>%
  summarise(sum_p = sum(total))

data<- merge(data1, data2, all.x = T)
data$ct<- rowSums(data[,3:4], na.rm = T)

res<- read_excel("prep/FIS/master_data.xlsx", sheet = "resiliencias")

proper <- function(x)
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

data$ESPECIE<- proper(data$ESPECIE)

##
data_n<-merge(data, res)
data_new<- select(data_n, id, stock_id = Taxon, res, ct, yr = AÑO)

write.csv(data_new, "prep/FIS/data_bmsy.csv", row.names = F)



### Considerar las 10 especies con mayor porcentaje por macrozona
##Por caleta
datac<- read_delim("prep/FIS/DESEMBARQUESPESQUEROS_SERNAPESCA_2000-2022_DATACENTER.csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE,
                   locale = locale(decimal_mark = ","))
sp <- read_csv("prep/FIS/species.csv")
sp$pmm<- as.character(sp$pmm)

datac$total<- rowSums(datac[,9:20])
datac$ESPECIE<-toupper(datac$ESPECIE)

datap<- read_delim("prep/FIS/DESEMBARQUE TOTAL POR PUERTO 2000-2021_DATACENTER.csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

datap$ESPECIE<-toupper(datap$ESPECIE)


datac_1<-datac %>%
    left_join(sp, by = c("ESPECIE")) %>%
    filter(AÑO %in% c(2017:2021), pmm == "TRUE") %>%
    group_by(MACROZONA, ESPECIE) %>%
    summarise(sum = sum(total))


datap_1<- datap %>%
  left_join(sp, by = c("ESPECIE")) %>%
  #filter("TIPO AGENTE" %in% c("Industrial", "INDUSTRIAL")) %>%
  filter(`TIPO AGENTE` %in% c("Artesanal", "ARTESANAL")) %>%
  filter(AÑO %in% c(2017:2021), pmm == "TRUE") %>%
  group_by(MACROZONA, ESPECIE) %>%
  summarise(sum_2 = sum(TONELADAS))

data_art<- datap_1%>%
  left_join(datac_1, by = c("MACROZONA","ESPECIE")) %>%


data_3<- data.frame()
for (i in dataz$MACROZONA) {
  d<- datac_2 %>% filter(MACROZONA == i) %>%
  arrange( desc(per_c) )%>%
  mutate(ac = cumsum (per_c))%>%
  filter(ac <= 95)%>%
  as.data.frame()
  datac_3<- rbind(datac_3,d)
}

datac_4<- select(datac_3, ESPECIE)
datac_4<-data.frame(datac_4[!duplicated(datac_4), ])

##Por puerto























