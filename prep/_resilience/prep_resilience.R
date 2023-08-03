library(dplyr)
library(readr)

####consumo de ER
resiliencia <- read_excel("prep/_resilience/ERNC_pat2021.xlsx")

resiliencia<- inv %>% mutate("resilience_score" = c(Valor/max(Valor) ))
## Punto de referencia : 49.73351 en Ancud
resiliencia<- select(resiliencia, c("rgn_id", "resilience_score"))

write.csv(resiliencia, "comunas/layers/cc_consumption_ernc_pat2021.csv", row.names = F)


####P anomalia de t°
resiliencia <- read_excel("prep/_resilience/research_invest_pat2021.xlsx")

resiliencia<- resiliencia %>% mutate("resilience_score" = c(MONTO/max(MONTO) ))
## Punto de referencia : 11460357 en Ancud
resiliencia<- select(resiliencia, c("rgn_id", "resilience_score"))

write.csv(resiliencia, "comunas/layers/wgi_all_pat2021.csv", row.names = F)

### Estado de las pesquerias ####
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

bmsy<- b %>%
  filter(year == 2021) %>%
  group_by(rgn_id) %>%
  summarise(score = mean(score))

write.csv(bmsy, "comunas/layers/s_fisheries_chl2023.csv", row.names = F)


