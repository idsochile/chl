library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)

#Cargar la base de datos de los salarios
le_j <- read_excel("prep/LIV/le_jobs.xlsx")
le_j<- select(le_j, -c(Comuna))

#Ordenar la tabla en formato largo
le_j2<-melt(le_j, id.vars = c("rgn_id", "Sector"))

le_j2<- le_j2 %>%
  dplyr::group_by(rgn_id, Sector) %>%
  dplyr::mutate(sum = sum(value)) %>%
  as.data.frame()

le_j2$value <- ifelse(le_j2$sum == 0,
                       NA,
                       le_j2$value)

le_j2<- select(le_j2, rgn_id , Sector, year = "variable", jobs = "value")

le_j2 <- le_j2[!is.na(le_j2$jobs),]


#guardar la base de datos
write.csv(le_j2, "C:/github/chl/comunas/layers/le_jobs_sector_chl2023.csv", row.names = FALSE, na = "")

#Cargar la base de datos de los salarios
le_w <- read_excel("prep/LIV/le_wage_sector_year.xlsx")
le_w<- merge(le_w, le)
le_w<- select(le_w, -c(Comuna))

#Ordenar la tabla en formato largo
le_w2<-melt(le_w, id.vars = c("rgn_id", "Sector"))

le_w2<- le_w2 %>%
  dplyr::group_by(rgn_id, Sector) %>%
  dplyr::mutate(sum = sum(value)) %>%
  as.data.frame()

le_w2$value <- ifelse(le_w2$sum == 0,
                      NA,
                      le_w2$value)

le_w2<- select(le_w2, rgn_id , Sector, year = "variable", wage_usd  = "value")

le_w2 <- le_w2[!is.na(le_w2$wage_usd),]


#guardar la base de datos
write.csv(le_w2, "C:/github/chl/comunas/layers/le_wage_sector_chl2023.csv", row.names = FALSE, na = "")




