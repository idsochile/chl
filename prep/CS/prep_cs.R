library(readxl)
library(dplyr)
library(reshape2)

algas<- read_excel("prep/CS/cs_rgn_chl_2023.xlsx", sheet = "Kelp Forest")

algas<- algas %>% select(-c("rgn_name")) %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Kelp forest")) %>%
  select(rgn_id ,year = "variable", habitat, km2 = "value")


pastos<- read_excel("prep/CS/cs_rgn_chl_2023.xlsx", sheet = "Seagrass")

pastos<- pastos %>% select(-c("rgn_name")) %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Seagrass")) %>%
  select(rgn_id ,year = "variable", habitat, km2 = "value")


marismas<- read_excel("prep/CS/cs_rgn_chl_2023.xlsx", sheet = "Tidal flats")

marismas<- marismas %>% select(-c("rgn_name")) %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Tidal flats")) %>%
  select(rgn_id ,year = "variable", habitat, km2 = "value")

cs<- rbind(algas, pastos, marismas)


write.csv(cs, "comunas/layers/cs_habitat_extent_chl2021.csv",
          row.names = F,
          na= "")


PR<- read_excel("prep/CS/cs_rgn_chl_2023.xlsx", sheet = "Punto referencia")

p_ref<- PR %>% select(rgn_id , "Kelp forest"=  "Kelp Forest" ,
                      "Seagrass" ,
                   "Tidal flats")%>%
  melt( id.vars = "rgn_id") %>%
  select(rgn_id, "habitat" = "variable", "p_ref" = "value")

write.csv(p_ref, "comunas/layers/cs_habitat_pref_chl2021.csv",
          row.names = F,
          na= "")


########
###Formula

area = SelectLayersData(layers, layers='rgn_area') %>%
  dplyr::select( rgn_id = "id_num",  area_km2 ="val_num")

cs <- cs[!is.na(cs$km2),]

wt<- mean(coef$w)

cs_b<- area %>%
  mutate(B= area_km2 * wt)



cs_scores<- cs %>%
  dplyr::filter(km2 > 0) %>%
  dplyr::left_join(p_ref, by = c("rgn_id", "habitat")) %>%
  dplyr::mutate(h = km2 / p_ref) %>%
  group_by(rgn_id, year, habitat) %>%
  left_join(coef, by = "habitat") %>%
  dplyr::summarise(A = c(h*w*km2)) %>%
  dplyr::group_by(rgn_id, year)%>%
  dplyr::summarise(A = sum(A))%>%
  left_join(cs_b, by = "rgn_id") %>%
  dplyr::mutate(status = (A/B)*100)























