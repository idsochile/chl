library(readxl)
library(dplyr)
library(readxl)

###### ########

planilla_final <- read_excel("prep/_pressures/planilla_final.xlsx", sheet = "OP")

planilla_final$Quimica<- as.numeric(planilla_final$Quimica)
planilla_final$Porpatogenos<- as.numeric(planilla_final$Porpatogenos)
planilla_final$PorFAN<- as.numeric(planilla_final$PorFAN)
planilla_final$Pornutrientes<- as.numeric(planilla_final$Pornutrientes)
planilla_final$Porbasura<- as.numeric(planilla_final$Porbasura)
planilla_final$EspeciesInvasoras<- as.numeric(planilla_final$EspeciesInvasoras)
planilla_final$Marino<- as.numeric(planilla_final$Marino)
planilla_final$Costero<- as.numeric(planilla_final$Costero)
planilla_final$Traficomarino<- as.numeric(planilla_final$Traficomarino)
planilla_final$Densidadcentrosdecultivo<- as.numeric(planilla_final$Densidadcentrosdecultivo)
planilla_final$Pescailegal<- as.numeric(planilla_final$Pescailegal)
planilla_final$Pescainsostenible<- as.numeric(planilla_final$Pescainsostenible)
planilla_final$Anomaliasdetemperaturadelmar<- as.numeric(planilla_final$Anomaliasdetemperaturadelmar)
planilla_final$Acidificacióndelmar<- as.numeric(planilla_final$Acidificacióndelmar)
planilla_final$Cotadeinundación <- as.numeric(planilla_final$Cotadeinundación)
planilla_final$Debilidaddegobernanza<- as.numeric(planilla_final$Debilidaddegobernanza)



resumen<- planilla_final %>%
  group_by(Meta) %>%
  summarise(Quimica = round(mean(Quimica, na.rm = T)),
            Porpatogenos = round(mean(Porpatogenos, na.rm = T)),
            PorFAN = round(mean(PorFAN, na.rm = T)),
            Pornutrientes = round(mean(Pornutrientes, na.rm = T)),
            Porbasura = round(mean(Porbasura, na.rm = T)),
            EspeciesInvasoras = round(mean(EspeciesInvasoras, na.rm = T)),
            Marino = round(mean(Marino, na.rm = T)),
            Costero = round(mean(Costero, na.rm = T)),
            Traficomarino = round(mean(Traficomarino, na.rm = T)),
            Densidadcentrosdecultivo = round(mean(Densidadcentrosdecultivo, na.rm = T)),
            Pescailegal = round(mean(Pescailegal, na.rm = T)),
            Pescainsostenible = round(mean(Pescainsostenible, na.rm = T)),
            Anomaliasdetemperaturadelmar = round(mean(Anomaliasdetemperaturadelmar, na.rm = T)),
            Acidificacióndelmar = round(mean(Acidificacióndelmar, na.rm = T)),
            Cotadeinundación = round(mean(Cotadeinundación, na.rm = T)),
            Debilidaddegobernanza = round(mean(Debilidaddegobernanza, na.rm = T)))


write.table(matrix, "clipboard", sep="\t", row.names=F)


planilla_ideal <- read_excel("prep/_pressures/planilla_ideal.xlsx")
matrix <- read_excel("prep/_pressures/matrix.xlsx")

matrix<- select(matrix, -c(element_name))

colnames(matrix)<- colnames(planilla_ideal)

matrix2<- rbind(matrix, planilla_ideal)




planilla_final<- matrix2 %>%
  group_by(goal, element) %>%
  summarise(po_chemical= trunc(mean(po_chemical, na.rm = T)),
            po_pathogen= trunc(mean(po_pathogen, na.rm = T)),
            po_pathogens_fan= trunc(mean(po_pathogens_fan, na.rm = T)),
            po_nutrients_3nm= trunc(mean(po_nutrients_3nm, na.rm = T)),
            po_trash= trunc(mean(po_trash, na.rm = T)),
            sp_exhaust= trunc(mean(sp_exhaust, na.rm = T)),
            hd_marine= trunc(mean(hd_marine, na.rm = T)),
            hd_coastal= trunc(mean(hd_coastal, na.rm = T)),
            hd_traffic= trunc(mean(hd_traffic, na.rm = T)),
            hd_density= trunc(mean(hd_density, na.rm = T)),
            fp_ilegal= trunc(mean(fp_ilegal, na.rm = T)),
            fp_unstt= trunc(mean(fp_unstt, na.rm = T)),
            cs_sst= trunc(mean(cs_sst, na.rm = T)),
            cc_acid= trunc(mean(cc_acid, na.rm = T)),
            cc_slr= trunc(mean(cc_slr, na.rm = T)),
            ss_wgi= trunc(mean(ss_wgi, na.rm = T))
  )


write.table(planilla_final, "clipboard", sep="\t", row.names=F)


########### ##########

citaciones <- read_excel("C:/Users/vapiz/Dropbox/Ohi/OHI+ CHILE/Metas/presiones/Citaciones_a_tribunales_2014_2023.xlsx",
                             sheet = "Sheet1")


c1<- citaciones %>%
  filter(Año >=2017,
         Año <= 2023,
         !(`TipoLugar` %in%  c("Rio", "Humedal", "Lago", "Laguna", "Estero", "-"))) %>%
  group_by(Nm_Comuna) %>%
  summarise(n = n())
















