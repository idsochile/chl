library(readxl)
library(dplyr)
library(reshape2)

algas<- read_excel("prep/CS/cs_rgn_chl_PR.xlsx", sheet = "Algas pardas")

algas<- algas %>% select(-c("rgn_name")) %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Kelp forest")) %>%
  select(rgn_id ,year = "variable", habitat, km2 = "value")


pastos<- read_excel("prep/CS/cs_rgn_chl_PR.xlsx", sheet = "pastos marinos")

pastos<- pastos %>% select(-c("rgn_name")) %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Seagrass")) %>%
  select(rgn_id ,year = "variable", habitat, km2 = "value")


marismas<- read_excel("prep/CS/cs_rgn_chl_PR.xlsx", sheet = "marismas")

marismas<- marismas %>% select(-c("rgn_name")) %>%
  melt( id.vars = "rgn_id") %>%
  mutate(habitat = c("Tidal Flats")) %>%
  select(rgn_id ,year = "variable", habitat, km2 = "value")

cs<- rbind(algas, pastos, marismas)


write.csv(cs, "comunas/layers/cs_habitat_extent_chl2021.csv",
          row.names = F,
          na= "")


PR<- read_excel("prep/CS/cs_rgn_chl_PR.xlsx", sheet = "Puntos de referencia")

p_ref<- PR %>% select(rgn_id , "Kelp forest" = "algas", "Seagrass" = "pastos",
                   "Tidal Flats"= "marismas")%>%
  melt( id.vars = "rgn_id") %>%
  rename(p_ref = "value", habitat = "variable")

write.csv(PR, "comunas/layers/cs_habitat_pref_chl2021.csv",
          row.names = F,
          na= "")



