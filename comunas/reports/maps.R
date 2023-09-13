library(sf)
library(cartography)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(paletteer)
library(colorBlindness)
library(patchwork)


chl<- st_read("comunas/spatial/rgn_cl.shp")
chl<- select(chl, region_id = "regin_d", region_name = "regn_nm", area_km2 = "are_km2")

factor<- as.factor(chl$region_name)

Puntajes <- read_excel("comunas/reports/Puntajes.xlsx")

s<- Puntajes %>%
  filter(dimension == "status")

chl<- merge(chl, s, all.x = T)

ggplot()+
  geom_sf(data = chl, aes(fill= score))+
  scale_fill_gradient(low = "#E5FFFF", high = "#003FFF") +
  theme_light()

regions_list<- select(regions_list, region_id = "rgn_id",rgn_name )

s<- merge(regions_list, s, all.x = T)
s$rgn_name <- factor(s$rgn_name, levels = factor)

s <- s %>%
  arrange(desc(rgn_name))

write.table(s, "clipboard", sep="\t", row.names=F)


ggplot(data=s, aes(x = score,  y =rgn_name))+
  geom_bar(stat="identity", fill = "#003FFF",
           color = "#E5FFFF")+
  scale_y_discrete(limits = rev(levels(s$rgn_name)))+
  theme_light()


######
sf_use_s2(FALSE)
chl<- st_read("C:/R/OHI/shape/comunasohi2.shp")
chl <- chl[!is.na(chl$rgn_id),]
chl<- select(chl, region_id=rgn_id)

scores <- read_csv("comunas/scores.csv")
data <-  read_excel("prep/_resilience/prot.xlsx",
                    sheet = "regiones") %>%
  select(region_id= rgn_id, region)

data1<- data.frame(region = unique(data$region))
data1<- edit(data1)

data<- merge(data, data1)

chl<-merge(chl, data)

for (i in unique(scores$goal)) {
i="CS"
s<- scores %>%
  filter(goal == i,
         dimension == "score")

s1<- merge(chl, s) %>%
  filter(posicion == 1)
color_palette <- paletteer_c("grDevices::Spectral", 30)

s1 <- ggplot()+
  geom_sf(data = s1, aes(fill= score))+
  scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
  theme_light()+
  theme(legend.position = "none")

s2<- merge(chl, s) %>%
  filter(posicion == 2)

s2 <- ggplot()+
  geom_sf(data = s2, aes(fill= score))+
  scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
  theme_light()+
  theme(legend.position = "none")

s3<- merge(chl, s) %>%
  filter(posicion == 3)

s3 <- ggplot()+
  geom_sf(data = s3, aes(fill= score))+
  scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
  labs(fill = "Puntaje")+
  theme_light()

nombre<- paste(i, ".png", sep = "_score")

png(nombre, height = 2500, width = 2000, res = 400)
s1+s2+s3
dev.off()
}

for (i in unique(scores$goal)) {
  i="SPP"
  s<- scores %>%
    filter(goal == i,
           dimension == "pressures")

  s1<- merge(chl, s) %>%
    filter(posicion == 1)
  color_palette <-     paletteer_c("ggthemes::Red", 30)

  s1 <- ggplot()+
    geom_sf(data = s1, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s2<- merge(chl, s) %>%
    filter(posicion == 2)

  s2 <- ggplot()+
    geom_sf(data = s2, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s3<- merge(chl, s) %>%
    filter(posicion == 3)

  s3 <- ggplot()+
    geom_sf(data = s3, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    labs(fill = "Presión")+
    theme_light()

  nombre<- paste(i, ".png", sep = "_pressure")

  png(nombre, height = 2500, width = 2000, res = 400)
  s1+s2+s3
  dev.off()
}


for (i in unique(scores$goal)) {
  i="TR"
  s<- scores %>%
    filter(goal == i,
           dimension == "resilience")

  s1<- merge(chl, s) %>%
    filter(posicion == 1)
  color_palette <-     paletteer_c("ggthemes::Green", 30)

  s1 <- ggplot()+
    geom_sf(data = s1, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s2<- merge(chl, s) %>%
    filter(posicion == 2)

  s2 <- ggplot()+
    geom_sf(data = s2, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s3<- merge(chl, s) %>%
    filter(posicion == 3)

  s3 <- ggplot()+
    geom_sf(data = s3, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    labs(fill = "Resiliencia")+
    theme_light()

  nombre<- paste(i, ".png", sep = "_resilience")

  png(nombre, height = 2500, width = 2000, res = 400)
  s1+s2+s3
  dev.off()
}

for (i in unique(scores$goal)) {
  i="BD"
  s<- scores %>%
    filter(goal == i,
           dimension == "trend")

  s1<- merge(chl, s)
  s1 <- s1%>%
    filter(posicion == 1)
  color_palette <- paletteer_c("grDevices::Spectral", 30)

  s1 <- ggplot()+
    geom_sf(data = s1, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(-1, 1))+
    theme_light()+
    theme(legend.position = "none")

  s2<- merge(chl, s) %>%
    filter(posicion == 2)

  s2 <- ggplot()+
    geom_sf(data = s2, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(-1, 1))+
    theme_light()+
    theme(legend.position = "none")

  s3<- merge(chl, s) %>%
    filter(posicion == 3)

  s3 <- ggplot()+
    geom_sf(data = s3, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(-1, 1))+
    labs(fill = "Tendencia")+
    theme_light()

  nombre<- paste(i, ".png", sep = "_trend")

  png(nombre, height = 2500, width = 2000, res = 400)
  s1+s2+s3
  dev.off()
}

for (i in unique(scores$goal)) {
  i="TR"
  s<- scores %>%
    filter(goal == i,
           dimension == "status")

  s1<- merge(chl, s) %>%
    filter(posicion == 1)
  color_palette <- paletteer_c("grDevices::Spectral", 30)

  s1 <- ggplot()+
    geom_sf(data = s1, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s2<- merge(chl, s) %>%
    filter(posicion == 2)

  s2 <- ggplot()+
    geom_sf(data = s2, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s3<- merge(chl, s) %>%
    filter(posicion == 3)

  s3 <- ggplot()+
    geom_sf(data = s3, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    labs(fill = "Estado actual")+
    theme_light()

  nombre<- paste(i, ".png", sep = "_status")

  png(nombre, height = 2500, width = 2000, res = 400)
  s1+s2+s3
  dev.off()
}

for (i in unique(scores$goal)) {
  i="TR"
  s<- scores %>%
    filter(goal == i,
           dimension == "future")

  s1<- merge(chl, s) %>%
    filter(posicion == 1)
  color_palette <- paletteer_c("grDevices::Spectral", 30)

  s1 <- ggplot()+
    geom_sf(data = s1, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s2<- merge(chl, s) %>%
    filter(posicion == 2)

  s2 <- ggplot()+
    geom_sf(data = s2, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    theme_light()+
    theme(legend.position = "none")

  s3<- merge(chl, s) %>%
    filter(posicion == 3)

  s3 <- ggplot()+
    geom_sf(data = s3, aes(fill= score))+
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
    labs(fill = "Estado futuro")+
    theme_light()

  nombre<- paste(i, ".png", sep = "_future")

  png(nombre, height = 2500, width = 2000, res = 400)
  s1+s2+s3
  dev.off()
}

