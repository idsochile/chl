library(sf)
library(KernSmooth)
library(raster)
library(dplyr)
library(ggplot2)
library(rgdal)
library(readr)
library(terra)
library(ggplot2)

#### Trafico maritimo ####
data<- read_csv("C:/R/OHI/data/TraficoAcuicultura_2021.csv")

i<- 3
coordenadas<- data.frame(xmin=c(-78, -76, -76),
                         ymin=c(-57, -42, -30),
                         xmax=c(-63, -64, -64),
                         ymax=c(-42, -30, -18))

## separar por sectores y filtrar coordenadas
d1<- filter(data,
            Speed > 2)

d1<-filter(d1,
           Longitude  > coordenadas[i,1],
           Latitude  > coordenadas[i,2],
           Longitude < coordenadas[i,3],
           Latitude  < coordenadas[i,4])


# get the coordinates
coordinates <- d1[,3:2]
coordinates<- as.data.frame(coordinates)

# compute the 2D binned kernel density estimate
est <- bkde2D(coordinates,
              bandwidth=c(0.05,0.05), #suaviza la distribucion
              gridsize=c(501L,501L), #2500 metros aproximadamente
              range.x=list(c(coordenadas[i,1],coordenadas[i,2]),
                           c(coordenadas[i,3],coordenadas[i,4])))

##range.x -> la diferencia en grados de minimas y maximas debe siempre ser la misma

### Le doy las coordenadas el ancho de banda el tamaño de la grilla y el rango va a cambiar
summary(est)

est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

# create raster
est.raster = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster) <- CRS("+init=epsg:4326")
#xmin(est.raster) <- coordenadas[1,1]
#xmax(est.raster) <- coordenadas[1,3]
#ymin(est.raster) <- coordenadas[1,2]
#ymax(est.raster) <-coordenadas[1,4]
# visually inspect the raster output
plot(est.raster)

data <- read_delim("C:/R/OHI/data/TraficoArtesanal_2021.csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

d1<- filter(data,
            Speed > 2)

d1<-filter(d1,
           Longitude  > coordenadas[i,1],
           Latitude  > coordenadas[i,2],
           Longitude < coordenadas[i,3],
           Latitude  < coordenadas[i,4])


# get the coordinates
coordinates <- d1[,3:2]
coordinates<- as.data.frame(coordinates)

# compute the 2D binned kernel density estimate
est <- bkde2D(coordinates,
              bandwidth=c(0.05,0.05), #suaviza la distribucion
              gridsize=c(501L,501L), #2500 metros aproximadamente
              range.x=list(c(coordenadas[i,1],coordenadas[i,2]),
                           c(coordenadas[i,3],coordenadas[i,4])))

##range.x -> la diferencia en grados de minimas y maximas debe siempre ser la misma

### Le doy las coordenadas el ancho de banda el tamaño de la grilla y el rango va a cambiar
summary(est)

est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

# create raster
est.raster2 = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster2) <- CRS("+init=epsg:4326")
#xmin(est.raster) <- coordenadas[1,1]
#xmax(est.raster) <- coordenadas[1,3]
#ymin(est.raster) <- coordenadas[1,2]
#ymax(est.raster) <-coordenadas[1,4]
# visually inspect the raster output
plot(est.raster2)


data <- read_csv("C:/R/OHI/data/TraficoIndustriales_2021.csv")

d1<- filter(data,
            Speed > 2)

d1<-filter(d1,
           Longitude  > coordenadas[i,1],
           Latitude  > coordenadas[i,2],
           Longitude < coordenadas[i,3],
           Latitude  < coordenadas[i,4])


# get the coordinates
coordinates <- d1[,3:2]
coordinates<- as.data.frame(coordinates)

# compute the 2D binned kernel density estimate
est <- bkde2D(coordinates,
              bandwidth=c(0.05,0.05), #suaviza la distribucion
              gridsize=c(501L,501L), #2500 metros aproximadamente
              range.x=list(c(coordenadas[i,1],coordenadas[i,2]),
                           c(coordenadas[i,3],coordenadas[i,4])))

##range.x -> la diferencia en grados de minimas y maximas debe siempre ser la misma

### Le doy las coordenadas el ancho de banda el tamaño de la grilla y el rango va a cambiar
summary(est)

est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

# create raster
est.raster3 = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster3) <- CRS("+init=epsg:4326")
#xmin(est.raster3) <- coordenadas[i,1]
#xmax(est.raster3) <- coordenadas[i,3]
#ymin(est.raster3) <- coordenadas[i,2]
#ymax(est.raster3) <-coordenadas[i,4]
# visually inspect the raster output
plot(est.raster3)


data <- read_csv("C:/R/OHI/data/TraficoTransportadoras_2021.csv")

d1<- filter(data,
            Speed > 2)

d1<-filter(d1,
           Longitude  > coordenadas[i,1],
           Latitude  > coordenadas[i,2],
           Longitude < coordenadas[i,3],
           Latitude  < coordenadas[i,4])


# get the coordinates
coordinates <- d1[,3:2]
coordinates<- as.data.frame(coordinates)

# compute the 2D binned kernel density estimate
est <- bkde2D(coordinates,
              bandwidth=c(0.05,0.05), #suaviza la distribucion
              gridsize=c(501L,501L), #2500 metros aproximadamente
              range.x=list(c(coordenadas[i,1],coordenadas[i,2]),
                           c(coordenadas[i,3],coordenadas[i,4])))

##range.x -> la diferencia en grados de minimas y maximas debe siempre ser la misma

### Le doy las coordenadas el ancho de banda el tamaño de la grilla y el rango va a cambiar
summary(est)

est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

# create raster
est.raster4 = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster4) <- CRS("+init=epsg:4326")
#xmin(est.raster) <- coordenadas[1,1]
#xmax(est.raster) <- coordenadas[1,3]
#ymin(est.raster) <- coordenadas[1,2]
#ymax(est.raster) <-coordenadas[1,4]
# visually inspect the raster output
plot(est.raster4)


est.raster<- overlay(est.raster,  est.raster2 , est.raster3, est.raster4, fun = sum)
plot(est.raster)

raster3 <- est.raster

summary(est.raster)
poligono<- rasterToPolygons(est.raster, dissolve = TRUE)
puntos <- coordinates(poligono) %>%
  as.data.frame()
p<- as.data.frame(puntos)
puntos <- st_as_sf(puntos, coords= c("V1", "V2"),
                   crs="+init=epsg:4326")


chl<- st_read("comunas/spatial/rgn_cl.shp",
              crs ="+init=epsg:4326") %>%
  select(rgn_id = regin_d, rgn_name =regn_nm )

ggplot() + geom_sf(data= chl)
+ geom_sf(data = puntos)

Value<- p %>% mutate(ind = raster::extract(est.raster, puntos))

puntos<- st_as_sf(Value, coords= c("V1", "V2"),
                  crs = "+init=epsg:4326")
sf_use_s2(FALSE)
p2 <-st_intersection(chl, puntos, join = st_overlaps)


p3 <- p2 %>%
  group_by(rgn_id,  rgn_name) %>%
  summarise(mean=mean(ind), median=median(ind))%>%
  mutate(std_value = (mean - 0)/ max(mean)-0 ) %>%
  as.data.frame() %>%
  select(rgn_id,rgn_name, mean, median, std_value)

write.table(p3, "clipboard", sep="\t", row.names=F)


#### Densidad de centros ####

c1<- st_read("C:/Data/zipfolder/Concesiones_de_Acuicultura.shp") %>%
  select(COMUNA, TOPONIMIO, ESPECIES, T_GRUPOESP, COORDENADA)
c1 <- st_transform(c1, "+init=epsg:4326")

sp<- data.frame(unique(c1$ESPECIES))

sp<- sp[c(4,5, 6,9, 10,13,14, 15, 20, 21, 22, 24,25,26, 27, 28,29, 30, 31, 32, 33,
          34, 35, 36, 37, 38, 39, 40, 41, 42, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
          56, 59, 60, 61, 62, 66, 67, 69, 70, 73, 74),]

sp<- as.data.frame(sp)

c2<- c1 %>%
  filter(ESPECIES %in% sp$sp)

p1 <- st_centroid(c2)
coords <- st_coordinates(p1)

chl<- st_read("comunas/spatial/rgn_cl.shp") %>%
  select(rgn_id = regin_d, rgn_name =regn_nm)
chl <- st_transform(chl, "+init=epsg:4326")

ggplot()+
  geom_sf(data = chl) +
  geom_sf(data = p1, aes(fill= "black", color = "black"))

#write.csv(p1, "p1.csv", row.names = F, na = "")
coordenadas<- data.frame(xmin=c(-75, -75, -75),
                         ymin=c(-56, -46, -35),
                         xmax=c(-65, -64, -62),
                         ymax=c(-46, -35, -22))
coords<- as.data.frame(coords)

##filtrar coordenadas
i<-1
c3<-filter(coords,
           X  > coordenadas[i,1],
           Y  > coordenadas[i,2],
           X < coordenadas[i,3],
           Y  < coordenadas[i,4])

# get the coordinates
coordinates <- coords[,1:2]


# compute the 2D binned kernel density estimate
est <- bkde2D(coordinates,
              bandwidth=c(0.05,0.05), #suaviza la distribucion
              gridsize=c(501L,501L), #2500 metros aproximadamente
              #range.x=list(c(coordenadas[i,1],coordenadas[i,2]),
              #             c(coordenadas[i,3],coordenadas[i,4])))
              range.x = list(c(-80,-56),
                             c(-46,-22)))

##range.x -> la diferencia en grados de minimas y maximas debe siempre ser la misma

### Le doy las coordenadas el ancho de banda el tamaño de la grilla y el rango va a cambiar
summary(est)

est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

# create raster
est.raster = raster(list(x=est$x1,y=est$x2,z=est$fhat))
crs(est.raster) <-  "+init=epsg:4326"
#xmin(est.raster) <- coordenadas[i,1]
#xmax(est.raster) <- coordenadas[i,3]
#ymin(est.raster) <- coordenadas[i,2]
#ymax(est.raster) <- coordenadas[i,4]
# visually inspect the raster output
plot(est.raster)

writeRaster(est.raster, filename="dc1.tif", format="GTiff", overwrite=TRUE)

summary(est.raster)
poligono<- rasterToPolygons(est.raster, dissolve = TRUE)
puntos <- coordinates(poligono) %>%
  as.data.frame()
p<- as.data.frame(puntos)
puntos <- st_as_sf(puntos, coords= c("V1", "V2"),
                   crs="+init=epsg:4326")


ggplot()+  geom_sf(data = chl) +  geom_sf(data = puntos)


Value<- puntos %>% mutate(ind = raster::extract(est.raster, puntos))

puntos<- st_as_sf(Value, coords= c("V1", "V2"),
                  crs = st_crs(chl))
sf_use_s2(FALSE)
p2 <-st_intersection(puntos, chl)

p2<- p2 %>%
  as.data.frame() %>%
  select(ind, rgn_id, rgn_name)

p2<- as.data.frame(p2)

p3 <- p2 %>%
  group_by(rgn_id, rgn_name) %>%
  dplyr::summarise(mean=mean(ind), median=median(ind))%>%
  mutate(std_value = (mean - 0)/ max(mean)-0 ) %>%
  select(rgn_id,rgn_name, mean, median, std_value)

write.table(p3, "clipboard", sep="\t", row.names=F)



chlt<- st_read("c:/R/OHI/shape/comunasohi.shp")
chlt <- st_transform(chlt, "+init=epsg:4326")



ggplot()+ geom_sf(data = chl) + geom_sf(data = chlt)





