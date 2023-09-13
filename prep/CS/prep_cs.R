library(readxl)
library(dplyr)
library(reshape2)

algas<- read_excel("prep/CS/cs_rgn_chl_2023 (1).xlsx", sheet = "Kelp Forest")

algas<- algas[,c(1,4:8)]

algas<- algas %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Kelp forest")) %>%
  select(rgn_id ,year = "variable", habitat, m2 = "value")




pastos<- read_excel("prep/CS/cs_rgn_chl_2023 (1).xlsx", sheet = "Seagrass")

pastos<- pastos[,c(1,4:8)]

pastos<- pastos %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Seagrass")) %>%
  select(rgn_id ,year = "variable", habitat, m2 = "value")


marismas<- read_excel("prep/CS/cs_rgn_chl_2023 (1).xlsx", sheet = "Tidal flats")

marismas<- marismas[,c(1,4:8)]

marismas<- marismas %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Tidal flats")) %>%
  select(rgn_id ,year = "variable", habitat, m2 = "value")

cs<- rbind(algas, pastos, marismas)

cs$m2<- cs$m2*1000000

cs<- cs %>%
  group_by(rgn_id, habitat) %>%
  mutate(p = sum(m2)) %>%
  filter(m2 > 0) %>%
  select(- p)


write.csv(cs, "comunas/layers/cs_habitat_extent_chl2023.csv",
          row.names = F,
          na= "")

####

p_ref<- cs %>%
  group_by(rgn_id,habitat) %>%
  summarise(p_ref = max(m2))

write.csv(p_ref, "comunas/layers/cs_habitat_pref_chl2023.csv",
          row.names = F,
          na= "")


#### area ####

area<- read_excel("prep/CS/cs_rgn_chl_2023 (1).xlsx", sheet = "Kelp Forest")

area <- area[,c(1,3)]

area$m2<- area$AREA_KM2 *1000000

area <- area[,c(1,3)]

write.csv(area, "comunas/layers/cs_area_chl2023.csv",
          row.names = F,
          na= "")


########
###Formula


wt<- mean(coef$w)

cs_b<- area %>%
  mutate(B= area_km2 * wt)



cs_scores<- p_ref %>%
  dplyr::left_join(area, by = c("rgn_id")) %>%
  dplyr::mutate(h = p_ref / m2) %>%
  select(rgn_id, habitat, h) %>%
  left_join(coef, by = "habitat") %>%
  left_join(cs, by = c("rgn_id", "habitat")) %>%
  dplyr::mutate(A = c(h*w*m2),
                   B = c(w*m2)) %>%
  dplyr::group_by(rgn_id, year)%>%
  dplyr::summarise(A = sum(A),
                   B = sum(B))%>%
  dplyr::mutate(status = (A/B)*100)

write.table(cs_scores, "clipboard", sep="\t", row.names=F)


#######MAP############

status<- cs_scores %>%
  filter(year == 2021) %>%
  select(region_id = rgn_id, status) %>%
  ungroup()


chl<- merge(chl, status)

png("np.png", height = 2500, width = 2000, res = 400)
ggplot()+
  geom_sf(data = chl, aes(fill= status))+
  scale_fill_gradientn(colors = color_palette, limits = c(0, 100))+
  theme_light()+
  theme(legend.position = "none")
dev.off()













