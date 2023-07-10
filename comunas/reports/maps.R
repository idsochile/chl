library(sf)
library(cartography)
library(ggplot2)
library(dplyr)
library(readxl)
library(paletteer)
library(colorBlindness)

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
  ylim(19, 56)+
  theme_light()

regions_list<- select(regions_list, region_id = "rgn_id",rgn_name )

s<- merge(regions_list, s, all.x = T)
s$rgn_name <- factor(s$rgn_name, levels = factor)

s <- s %>%
  arrange(desc(rgn_name))

ggplot(data=s, aes(x = score,  y =rgn_name))+
  geom_bar(stat="identity", fill = "#3288FF",
           color = "#E5FFFF")+
  scale_y_discrete(limits = rev(levels(s$rgn_name)))+
  theme_light()





