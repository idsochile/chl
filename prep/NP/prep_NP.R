library(dplyr)
library(reshape2)
library(readxl)
library(readr)


###### np_harvest_tonnes ######

desembarque <- read_csv("prep/NP/desembarque.csv")

desembarque<- desembarque %>%
  filter(spp != "CHANCHITO") %>%
  group_by(rgn_id, year) %>%
  dplyr::summarise(tonnes = sum(catch)) %>%
  mutate(producto = "Algas") %>%
  as.data.frame()

produccion <- read_excel("prep/NP/produccion.xlsx", sheet = "Hoja3" )

produccion<- produccion[, c(1,2, 4:8)]
produccion = rename(produccion, c("2017...8" = 2017))

produccion<- melt(produccion, id.vars = c("Comuna", "producto"))
names(produccion) <- c("comuna", "producto", "year", "tonnes")

comunas <- read_csv("comunas/spatial/regions_list.csv")
comunas<- select(comunas, comuna = "rgn_name", rgn_id)

p2<- merge(produccion, comunas, all.x = T)
p2 <- p2[!is.na(p2$rgn_id),]
p2<- select(p2, -c("comuna"))
p2<- select(p2, c("rgn_id", "year", "tonnes", "producto"))

p2<- rbind(desembarque, p2)

p3<- p2 %>%
  group_by(rgn_id, producto, year) %>%
  dplyr::summarise(tonnes = sum(tonnes, na.rm = T)) %>%
  filter(year %in% c(2017:2021)) %>%
  as.data.frame()



write.csv(p3, "np_harvest_tonnes_chl2023.csv",
          row.names = F,
          na = "")

###### np_harvest_tonnes_weigth ######
p3 <- read_csv("comunas/layers/np_harvest_tonnes_chl2023.csv")
p3<- as.data.frame(p3)

precios <- read_excel("prep/NP/Precios.xlsx")
precios<- melt(precios, id.vars = "producto")
names(precios)<- c("producto", "year", "precio")
precios$year<- as.character(precios$year)
precios$year<- as.numeric(precios$year)


p4<- p3 %>%
  left_join(precios, by = c("producto", "year"))%>%
  mutate(g = c(tonnes* precio))%>%
  group_by(rgn_id, year) %>%
  dplyr::summarise(prom_t = sum(g)) %>%
  as.data.frame()

p5<- p3 %>%
  left_join(precios, by = c("producto", "year"))%>%
  mutate(g = c(tonnes* precio))%>%
  #group_by(rgn_id, producto, year) %>%
  #dplyr::summarise(prom_p = mean(g)) %>%
  as.data.frame()%>%
  left_join(p4, by = c("rgn_id", "year")) %>%
  mutate(v_rel = g /prom_t) %>%
  select(rgn_id, year, producto, weight ="v_rel")

##
write.csv(p5, "comunas/layers/np_harvest_tonnes_weigth_chl2023.csv",
          row.names = F,
          na = "")



########## b/bmsy ############
library(openxlsx)
ruta_archivo<- "C:/Users/vapiz/Dropbox/Ohi/OHI+ CHILE/Metas/Productos Naturales/Datos/2021_040103_materia_prima_y_produccion_por_puerto.xlsx"

nombres <- function(ruta_archivo) {
  # Cargar el archivo de Excel
  libro_excel <- loadWorkbook(ruta_archivo)

  # Obtener los nombres de las hojas
  nombres_hojas <- sheets(wb = libro_excel)

  return(nombres_hojas)
}

nombres_hojas <- nombres(ruta_archivo)

p<- data.frame()

for (i in nombres_hojas) {
  p1 <- read_excel(ruta_archivo, sheet = i)
  names (p1)[1] = "ESPECIE"
  names (p1)[10] = "L9"
  names (p1)[11] = "L10"
  names (p1)[2] = "TIPO"

  p2<- p1 %>%
    filter(L9 > 0,
           TIPO == "M") %>%
    select(ESPECIE, TIPO, value ="L9") %>%
    filter(!ESPECIE %in% c("TOTAL CRUSTACEOS", "TOTAL GENERAL", "TOTAL MOLUSCOS", "TOTAL OTRAS ESPECIE", "TOTAL PECES"))%>%
    mutate(Comuna = i,
           Linea = "L9")

  p3<- p1 %>%
    filter(L10 > 0,
           TIPO == "M") %>%
    select(ESPECIE, TIPO, value = "L10")%>%
    mutate(Comuna = i,
           Linea = "L10")

  p<- rbind(p,p2, p3)

}

p2018$year<- 2018
p2019$year<- 2019
p2020$year<- 2020
p2021$year<- 2021

p<- rbind(p2018, p2019, p2020, p2021)

p <- filter(p,
            !Comuna %in% c("I", "II", "III", "IV", "V", "Reg IV", "RegI", "RegII", "RegIII", "RegV", "RegVIII", "RegXV", "VIII", "X", "XI", "XIV", "XV"))


p$comuna<- toupper(p$Comuna)

comunas <- read_csv("comunas/spatial/regions_list.csv")
comunas<- select(comunas, rgn_name, rgn_id)
comunas$comuna<- toupper(comunas$rgn_name)

p<- merge(p, comunas)

p<- select(p, rgn_id, year, spp = "ESPECIE", value)

b <- read_csv("comunas/layers/fis_b_bmsy_chl2023.csv")

alpha <- 0.5
beta <- 0.25
lowerBuffer <- 0.95
upperBuffer <- 1.05

b$score = ifelse(
  b$b_bmsy < lowerBuffer,
  b$b_bmsy,
  ifelse (b$b_bmsy >= lowerBuffer &
            b$b_bmsy <= upperBuffer, 1, NA)
)
b$score = ifelse(!is.na(b$score),
                 b$score,
                 ifelse(
                   1 - alpha * (b$b_bmsy - upperBuffer) > beta,
                   1 - alpha * (b$b_bmsy - upperBuffer),
                   beta
                 ))



p1<- p %>%
  left_join(b, by= c("rgn_id", "spp", "year"))


p2 <- p1 %>%
  dplyr::group_by(rgn_id, year) %>%
  dplyr::mutate(mean_score = mean(score, na.rm = TRUE)) %>%
  dplyr::ungroup()

p2 <- p1 %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(mean_score_global = mean(score, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(mean_score = ifelse(!is.na(score), score, mean_score_global))

p3 <- p2 %>%
  mutate(mean_score = ifelse(!is.na(score), score, mean_score_global))

p3$value<- as.numeric(p3$value)



status_data <- p3 %>%
  dplyr::group_by(year, rgn_id) %>%
  dplyr::mutate(SumCatch = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(wprop = value / SumCatch)

status_data_final <- status_data %>%
  dplyr::group_by(rgn_id, year) %>%
  dplyr::summarize(status = prod(na.omit(mean_score ^ wprop)))   %>%
  dplyr::ungroup()

pa<- status_data_final %>%
  mutate(producto = "Aceite")

ph<- status_data_final %>%
  mutate(producto = "Harina")

np_fofm_scores<- rbind(pa, ph)

write.csv(np_fofm_scores, "comunas/layers/np_fofm_scores_chl2023.csv",
          row.names = F,
          na = "")



######### np_seaweed_sust #########

sust <- read_excel("prep/NP/produccion.xlsx", sheet = "Hoja2")

sust <- sust[!is.na(sust$Año),]

coef_sust<- sust %>%
  select(Especie, Macrozona, Año, Total) %>%
  group_by(Macrozona, Año) %>%
  dplyr::summarise(coef = mean(Total, na.rm = T)) %>%
  mutate(producto = "Algas")

coef_sust$MACROZONA<- toupper(coef_sust$Macrozona)

dataz["COMUNA"][dataz["COMUNA"] == "CHAÑARAL"] <- "CHANARAL"#asignamos palena con chait?n
dataz["COMUNA"][dataz["COMUNA"] == "MELINKA"] <- "GUAITECAS"
dataz["COMUNA"][dataz["COMUNA"] == "ILOCA"] <- "LICANTEN"
dataz["COMUNA"][dataz["COMUNA"] == "PALENA"] <- "CHAITEN"
dataz["COMUNA"][dataz["COMUNA"] == "SAVEDRA"] <- "SAAVEDRA"
dataz["COMUNA"][dataz["COMUNA"] == "VIÑA DEL MAR"] <- "VINA DEL MAR"

data<- merge(coef_sust, dataz)

comunas <- read_csv("comunas/spatial/regions_list.csv")
comunas<- select(comunas, rgn_name, rgn_id)
comunas$COMUNA<- toupper(comunas$rgn_name)

p<- merge(data, comunas, all.x = T)

p <- p[!is.na(p$rgn_id),]

p<- select(p, rgn_id, producto,year = Año, score = coef)


write.csv(p, "comunas/layers/np_seaweed_sust_chl2023.csv",
          row.names = F,
          na = "")
###### np_harvest_tonnes_releative #####

np2<- np %>%
  group_by(rgn_id, producto) %>%
  mutate(max = max(tonnes)) %>%
  ungroup() %>%
  mutate(tonnes_rel = tonnes/ max) %>%
  select(-c("tonnes", "max"))


write.csv(np2, "comunas/layers/np_harvest_tonnes_releative_chl2023.csv",
          row.names = F,
          na = "")


















