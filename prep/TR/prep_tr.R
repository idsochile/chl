library(readxl)
library(dplyr)
library(reshape2)

PE<- read_excel("prep/TR/prop_empleos.xlsx", sheet = "PETC")
PE<- PE %>% select(-c("rgn_name")) %>%
  melt(id.vars = c("rgn_id")) %>%
  select(rgn_id, year = "variable", ep = "value")

PE$year<- as.character(PE$year)
PE$year<- as.numeric(PE$year)

write.csv(PE, "comunas/layers/tr_jobs_pct_tourism_chl2021.csv",
          row.names = F,
          na= "")


SS<- read_excel("prep/TR/factor_sustentabilidad.xlsx", sheet = "SELLO S")
SS<- SS %>% select(-c("rgn_name")) %>%
  melt(id.vars = c("rgn_id")) %>%
  select(rgn_id, year = "variable", s_score = "value")

SS$year<- as.character(SS$year)
SS$year<- as.numeric(SS$year)

write.csv(SS, "comunas/layers/tr_sustainability_chl2021.csv",
          row.names = F,
          na= "")


TC<- read_excel("prep/TR/factor_tc.xlsx", sheet = "TendFTC")
TC<- TC %>% select(-c("rgn_name", "2022")) %>%
  melt(id.vars = c("rgn_id")) %>%
  select(rgn_id, year = "variable", factor = "value")

TC$year<- as.character(TC$year)
TC$year<- as.numeric(TC$year)

write.csv(TC, "comunas/layers/tr_factor_chl2021.csv",
          row.names = F,
          na= "")










