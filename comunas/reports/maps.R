library(sf)
library(cartography)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(paletteer)
library(colorBlindness)
library(patchwork)


sf_use_s2(FALSE)
chl<- st_read("C:/R/OHI/shape/comunas.shp")
chl2<- st_read("C:/R/OHI/shape/com5.shp") %>%
  as.data.frame() %>%
  select(objectid, rgn_id = region_id)
chl<- merge(chl, chl2)
chile<- st_read("C:/R/OHI/shape/chil_cont.shp")
chl<- select(chl, region_id=rgn_id)

factor<- as.factor(chl$region_name)

scores <- read_csv("comunas/scores.csv")

chl<- merge(chl, scores, all.x = T)

## Mel's color palette ----
reds <-  grDevices::colorRampPalette(
  c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090"),
  space="Lab")(65)
blues <-  grDevices::colorRampPalette(
  c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"))(35)
myPalette <-   c(reds, blues)


### map 1 ####
d1 <-  read_excel("prep/_resilience/prot.xlsx",
                  sheet = "regiones") %>%
  select(region_id= rgn_id, region, posicion)


chl<- merge(chl, d1)

#### filter ####
ch<- chl %>%
  filter(goal == "Index",
         dimension == "score",
         posicion == 3)


s1<- ggplot()+
  geom_sf(data= chile)+
  geom_sf(data = ch, aes(fill= score))+
  scale_fill_gradientn(colors = myPalette, limits = c(0, 100))+
  scale_x_continuous(breaks = c(-73,-70,-68), limits = c(-73.2,-68))+
  ylim(c(-30, -18))+
  theme_light()+
  theme(legend.position = "none")

ch<- chl %>%
  filter(goal == "Index",
         dimension == "score",
         posicion == 2)

s2 <- ggplot()+
  geom_sf(data = chile)+
  geom_sf(data = ch, aes(fill= score))+
  scale_fill_gradientn(colors = myPalette, limits = c(0, 100))+
  scale_x_continuous(breaks = c(-74,-70,-68), limits = c(-74,-68))+
  ylim(c(-42, -30))+
  theme_light()+
  theme(legend.position = "none")


ch<- chl %>%
  filter(goal == "Index",
         dimension == "score",
         posicion == 1)

s3 <- ggplot()+
  geom_sf(data = chile)+
  geom_sf(data = ch, aes(fill= score))+
  scale_fill_gradientn(colors = myPalette, limits = c(0, 100))+
  scale_x_continuous(breaks = c(-76,-70,-66))+
  ylim(c(-56,-42))+
  theme_light()+
  theme(legend.position = c(0.67, 0.84), legend.justification = c(0, 0.5))+
  labs(fill='Puntaje')


p <- s1 + s2 + s3

png("C:/github/chl/comunas/reports/maps/map1.png", height = 1800, width = 2500, res = 300)
p
dev.off()

### resto ####






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
d1 <-  read_excel("prep/_resilience/prot.xlsx",
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

