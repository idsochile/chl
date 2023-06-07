

getwd()

rm(list=ls())

library(ggplot2)
library(dplyr)


#SE FILTRAN LOS VALORES POR VELOCIDAD > 2 Y LATITUD < -40



filedirene20 <- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/1. ENERO 2020")#crea un directorio donde tengas los csv's, puede ser mensual o como prefiera
file_namesene20 <- dir(filedirene20)
enero2020 <- do.call(rbind, lapply(file_namesene20[1:30], read.csv, header = TRUE, sep=";", dec="."))
filterEnero20 <- filter(enero2020, Latitude< -40, Speed > 2)



setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/2. FEBRERO 2020")
filedirfeb20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/2. FEBRERO 2020")
file_namesfeb20 <- dir(filedirfeb20)
feb2020 <- do.call(rbind, lapply(file_namesfeb20[2:26], read.csv, header = TRUE, sep=";",dec="."))
filterFebrero2020 <- filter(feb2020, Latitude< -40, Speed > 2)



setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/3. MARZO 2020")
filedirmar20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/3. MARZO 2020")
file_namesmar20 <- dir(filedirmar20)
marzo2020 <- do.call(rbind, lapply(file_namesmar20[2:31], read.csv, header = TRUE, sep=";",dec="."))
filterMarzo2020 <- filter(marzo2020, Latitude< -40, Speed > 2)




setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/4. ABRIL 2020")
filedirabril20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/4. ABRIL 2020")
file_namesabr20 <- dir(filedirabril20)
abr2020 <- do.call(rbind, lapply(file_namesabr20[1:29], read.csv, header = TRUE, sep=";",dec="."))
filterAbril2020 <- filter(abr2020, Latitude< -40, Speed > 2)



setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/5. MAYO 2020")
filedirmayo20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/5. MAYO 2020")
file_namesmay20 <- dir(filedirmayo20)
may2020 <- do.call(rbind, lapply(file_namesmay20[1:31], read.csv, header = TRUE, sep=";",dec="."))
filterMayo2020 <- filter(may2020, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/6. JUNIO 2020")
filedirjunio20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/6. JUNIO 2020")
file_namesjunio20 <- dir(filedirjunio20)
junio20 <- do.call(rbind, lapply(file_namesjunio20[1:30], read.csv, header = TRUE, sep=";",dec="."))
filterJunio20 <- filter(junio20, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/7. JULIO 2020")
filedirjulio20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/7. JULIO 2020")
file_namesjulio20 <- dir(filedirjulio20)
julio20 <- do.call(rbind, lapply(file_namesjulio20[2:31], read.csv, header = TRUE, sep=";",dec="."))
filterJulio20 <- filter(julio20, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/8. AGOSTO 2020")
filediragosto20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/8. AGOSTO 2020")
file_namesagosto20 <- dir(filediragosto20)
agosto20 <- do.call(rbind, lapply(file_namesagosto20[1:31], read.csv, header = TRUE, sep=";",dec="."))
filterAgosto20 <- filter(agosto20, Latitude< -40, Speed >2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/9. SEPTIEMBRE 2020")
filedirseptiembre20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/9. SEPTIEMBRE 2020")
file_namesseptiembre20 <- dir(filedirseptiembre20)
septiembre20 <- do.call(rbind, lapply(file_namesseptiembre20[1:30], read.csv, header = TRUE, sep=";",dec="."))
filterSeptiembre20 <- filter(septiembre20, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/10. OCTUBRE 2020")
filediroctubre20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/10. OCTUBRE 2020")
file_namesoctubre20 <- dir(filediroctubre20)
octubre20 <- do.call(rbind, lapply(file_namesoctubre20[1:31], read.csv, header = TRUE, sep=";",dec="."))
filterOctubre20 <- filter(octubre20, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/11. NOVIEMBRE 2020")
filedirnoviembre20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/11. NOVIEMBRE 2020")
file_namesnoviembre20 <- dir(filedirnoviembre20)
noviembre20 <- do.call(rbind, lapply(file_namesnoviembre20[1:30], read.csv, header = TRUE, sep=";",dec="."))
filterNoviembre20 <- filter(noviembre20, Latitude< -40, Speed> 2)



setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/12. DICIEMBRE 2020")
filedirdiciembre20<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2020/12. DICIEMBRE 2020")
file_namesdiciembre20 <- dir(filedirdiciembre20)
diciembre20 <- do.call(rbind, lapply(file_namesdiciembre20[2:30], read.csv, header = TRUE, sep=";",dec="."))
filterDiciembre20 <- filter(diciembre20, Latitude< -40 , Speed > 2)




TM_20<-rbind(filterEnero20,filterFebrero2020,filterMarzo2020,filterAbril2020,filterMayo2020,filterJunio20,filterJulio20
             ,filterAgosto20,filterSeptiembre20,filterOctubre20,filterNoviembre20,filterDiciembre20)


write.csv(TM_20,"TM_20.csv")



setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/1. ENERO 2021")
filedirene21 <- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/1. ENERO 2021")#crea un directorio donde tengas los csv's, puede ser mensual o como prefiera
file_namesene21 <- dir(filedirene21)
enero2021 <- do.call(rbind, lapply(file_namesene21[1:31], read.csv, header = TRUE, sep=";", dec="."))
filterEnero21 <- filter(enero2021, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/2. FEBRERO 2021")
filedirfeb21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/2. FEBRERO 2021")
file_namesfeb21 <- dir(filedirfeb21)
feb2021 <- do.call(rbind, lapply(file_namesfeb21[1:28], read.csv, header = TRUE, sep=";",dec="."))
filterFebrero2021 <- filter(feb2021, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/3. MARZO 2021")
filedirmar21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/3. MARZO 2021")
file_namesmar21 <- dir(filedirmar21)
marzo2021 <- do.call(rbind, lapply(file_namesmar21[1:29], read.csv, header = TRUE, sep=";",dec="."))
filterMarzo2021 <- filter(marzo2021, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/4. ABRIL 2021")
filedirabril21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/4. ABRIL 2021")
file_namesabr21 <- dir(filedirabril21)
abr2021 <- do.call(rbind, lapply(file_namesabr21[1:29], read.csv, header = TRUE, sep=";",dec="."))
filterAbril2021 <- filter(abr2021, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/5. MAYO 2021")
filedirmayo21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/5. MAYO 2021")
file_namesmay21 <- dir(filedirmayo21)
may2021 <- do.call(rbind, lapply(file_namesmay21[2:31], read.csv, header = TRUE, sep=";",dec="."))
filterMayo2021 <- filter(may2021, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/6. JUNIO 2021")
filedirjunio21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/6. JUNIO 2021")
file_namesjunio21 <- dir(filedirjunio21)
junio21 <- do.call(rbind, lapply(file_namesjunio21[1:30], read.csv, header = TRUE, sep=";",dec="."))
filterJunio21 <- filter(junio21, Latitude< -40, Speed >2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/7. JULIO 2021")
filedirjulio21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/7. JULIO 2021")
file_namesjulio21 <- dir(filedirjulio21)
julio21 <- do.call(rbind, lapply(file_namesjulio21[3:31], read.csv, header = TRUE, sep=";",dec="."))
filterJulio21 <- filter(julio21, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/8. AGOSTO 2021")
filediragosto21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/8. AGOSTO 2021")
file_namesagosto21 <- dir(filediragosto21)
agosto21 <- do.call(rbind, lapply(file_namesagosto21[1:31], read.csv, header = TRUE, sep=";",dec="."))
filterAgosto21 <- filter(agosto21, Latitude< -40, Speed >2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/9. SEPTIEMBRE 2021")
filedirseptiembre21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/9. SEPTIEMBRE 2021")
file_namesseptiembre21 <- dir(filedirseptiembre21)
septiembre21 <- do.call(rbind, lapply(file_namesseptiembre21[1:29], read.csv, header = TRUE, sep=";",dec="."))
filterSeptiembre21 <- filter(septiembre21, Latitude< -40, Speed >2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/10. OCTUBRE 2021")
filediroctubre21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/10. OCTUBRE 2021")
file_namesoctubre21 <- dir(filediroctubre21)
octubre21 <- do.call(rbind, lapply(file_namesoctubre21[1:31], read.csv, header = TRUE, sep=";",dec="."))
filterOctubre21 <- filter(octubre21, Latitude< -40, Speed >2)



setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/11. NOVIEMBRE 2021")
filedirnoviembre21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/11. NOVIEMBRE 2021")
file_namesnoviembre21 <- dir(filedirnoviembre21)
noviembre21 <- do.call(rbind, lapply(file_namesnoviembre21[2:30], read.csv, header = TRUE, sep=";",dec="."))
filterNoviembre21 <- filter(noviembre21, Latitude< -40, Speed > 2)


setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/12. DICIEMBRE 2021")
filedirdiciembre21<- setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO/A?O 2021/12. DICIEMBRE 2021")
file_namesdiciembre21 <- dir(filedirdiciembre21)
diciembre21 <- do.call(rbind, lapply(file_namesdiciembre21[1:31], read.csv, header = TRUE, sep=";",dec="."))
filterDiciembre21 <- filter(diciembre21, Latitude< -40,Speed > 2)



TM_21<-rbind(filterEnero21,filterFebrero2021,filterMarzo2021,filterAbril2021,filterMayo2021,filterJunio21,filterJulio21,
             filterAgosto21,filterSeptiembre21,filterOctubre21,filterNoviembre21,filterDiciembre21)


write.csv(TM_21, "TM_21.csv")

getwd()

setwd("C:/Users/jonag/Dropbox/OHI (1)/PRESIONES/TRAFICO MARITIMO")

TM<- rbind(TM_20,TM_21)

write.csv(TM, "TM.csv")


TM<-read.csv("TM.csv")

library(KernSmooth)
library(raster)
library(dplyr)
library(ggplot2)



input <- TM
output <- "TM_2020_21.asc"


# get the coordinates
records <- TM
coordinates <- records[,6:5]


# compute the 2D binned kernel density estimate
est <- bkde2D(coordinates,
              bandwidth=c(0.05,0.05), #suaviza la distribuci?n
              gridsize=c(501L,501L), #2500 metros aproximadamente
              range.x=list(c(-80,-64),c(-56,-40)))


summary(est)

est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

# create raster
est.raster = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster) <- CRS("+init=epsg:4326")
xmin(est.raster) <- -80
xmax(est.raster) <- -64
ymin(est.raster) <- -56
ymax(est.raster) <- -40
# visually inspect the raster output
plot(est.raster)


summary(est.raster)


# write an ascii file
writeRaster(est.raster,output,"ascii", overwrite=T)








######ESTADISTICOS TRAFICO MARITIMO POR COMUNA

TM_RGN_ID<-read.csv("trafico_marino_rgn_id.csv",  h=T)


ED_TM_RGN<-TM_RGN_ID %>%
  group_by(RGN_ID_1) %>%
  summarise(mean=mean(TM18S), median=median(TM18S))%>%
  mutate(std_value = (mean - 0)/ max(mean)-0 )


write.csv(ED_TM_RGN,"PresionTraficoMarino.csv")

