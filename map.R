library(sf)
library(cartography)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(paletteer)
library(colorBlindness)
library(patchwork)
library(rworldxtra)
library(rgeos)

i<- "LIV"

sf_use_s2(FALSE)
chl<- st_read("C:/R/OHI/shape/comunas.shp")
chl2<- st_read("C:/R/OHI/shape/com5.shp") %>%
  as.data.frame() %>%
  select(objectid, rgn_id = region_id)
chl<- merge(chl, chl2)
chile<- st_read("C:/R/OHI/shape/chil_cont.shp")
chl<- select(chl, region_id=rgn_id)


scores <- read_csv("comunas/scores.csv")
data <-  read_excel("prep/_resilience/prot.xlsx",
                    sheet = "regiones") %>%
  select(region_id= rgn_id, rgn_name)

scores["dimension"][scores["dimension"] == "future"] <- "Estado futuro"
scores["dimension"][scores["dimension"] == "pressures"] <- "Presiones"
scores["dimension"][scores["dimension"] == "resilience"] <- "Resiliencias"
scores["dimension"][scores["dimension"] == "status"] <- "Estado actual"

regions_list <- read_csv("comunas/spatial/regions_list.csv") %>%
  select(region_id = rgn_id, rgn_name)

factor<- as.factor(data$rgn_name)

chl<-merge(chl, data)

#### MAP 1####

s<- scores %>%
  left_join(data, by = "region_id") %>%
  filter(goal == i,
         dimension  %in% c("Estado futuro", "Estado actual"))

s$rgn_name<- factor(s$rgn_name, levels = factor)

color<-  c("#FF5500", "#6551CC")

s1<- ggplot()+
  geom_bar(data = s, aes(group = dimension, x = score,  y = rgn_name, fill = dimension),
           orientation = "y", stat = "identity", position='dodge')+
  scale_y_discrete(limits = levels(s$rgn_name))+
  scale_fill_manual(values = color) +
  theme_light()+
  theme(axis.text = element_text(size=5))+
  labs(x = "Puntaje", y = NULL,
       fill = "Dimensión")+
  theme(legend.position = "none")

s<- filter(scores, goal == i,
           dimension == "score")

chl<- merge(chl, s)
color_palette <- paletteer_c("grDevices::Spectral", 30)

s2<- ggplot()+
  geom_sf(data = chile)+
  geom_sf(data = chl, aes(fill= score))+
  scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
  scale_x_continuous(breaks = c(-76,-70,-66))+
  theme_light()+
  theme(legend.position = "none")


#### MAP 2 ####

s<- scores %>%
  left_join(data, by = "region_id") %>%
  filter(goal == i,
         dimension  %in% c("Presiones", "Resiliencias"))

s$rgn_name<- factor(s$rgn_name, levels = factor)

color<-  c( "#A5300F", "#60BD68")

s3<- ggplot()+
  geom_point(data=s, aes(x = score,  y = rgn_name, color = dimension))+
  geom_line(data = s, aes(group = dimension, x = score,  y = rgn_name, color = dimension),
            orientation = "y")+
  scale_y_discrete(limits = levels(s$rgn_name))+
  theme_light()+
  scale_color_manual(values = color) +
  theme(axis.text = element_text(size=5),
        axis.text.y = element_blank())+
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Puntaje", y = NULL,
       fill = "Dimensión")

s<- scores %>%
  left_join(data, by = "region_id") %>%
  filter(goal == i,
         dimension  %in% c("trend"))
s$rgn_name<- factor(s$rgn_name, levels = factor)

s$color <- ifelse(s$score > 0,
                  "#BE1316",
                  "#1C5A99")
color<-  c("#BE1316", "#1C5A99")

s4<- ggplot()+
  geom_bar(data=s, aes(x = score,  y = rgn_name, fill = color), stat = "identity")+
  scale_fill_manual(values = color) +
  scale_y_discrete(limits = levels(s$rgn_name))+
  theme_light()+
  theme(axis.text = element_text(size=5),
        axis.text.y = element_blank())+
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "Tendencia", y = NULL)+
  theme(legend.position = "none")



png("map.png", height = 2500, width = 3500, res = 400)
s2+s1+s4+s3+ plot_layout(nrow = 1, byrow = FALSE)
dev.off()
png("map2.png", height = 2500, width = 3000, res = 400)
s2+s1+s4+s3+ plot_layout(nrow = 1, byrow = FALSE)
dev.off()

###score ####

scores <- scores %>%
  filter(goal == i)

write.table(scores, "clipboard", sep="\t", row.names=F, na = "")

s<- filter(scores, goal == i,
           dimension == "score") %>%
  left_join(regions_list, by = "region_id")
View(s)


#### Map 3 ####
s<- filter(scores, goal == i,
           dimension == "score") %>%
  as.data.frame()

chl<- merge(chl, s)
color_palette <- paletteer_c("grDevices::Spectral", 30)

s1<- ggplot()+
  geom_sf(data = chile)+
  geom_sf(data = chl, aes(fill = score))+
  scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks = c(-76,-70,-66))+
  labs(fill = "Puntaje")

png("map.png", height = 2000, width = 1000, res = 400)
s1
dev.off()


