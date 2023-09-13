library(dplyr)
library(readr)
library(scales)

#################

data <-  read_excel("prep/_resilience/prot.xlsx",
                    sheet = "regiones")

data<- select(data, rgn_id, resilience_score)

names (data)[2] = "resilience_score"
data$resilience_score<- rescale_max(data$resilience_score)


data <- data[!is.na(data$rgn_id),]
data$resilience_score<- data$resilience_score / 100


write.csv(data, "comunas/layers/area_ramsar_chl2023.csv", row.names = F)




