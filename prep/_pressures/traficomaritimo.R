library(sf)
library(KernSmooth)
library(raster)
library(dplyr)
library(ggplot2)


data<- read.csv("C:/R/TraficoArtesanal2019_Depurado.csv",
                                         sep= ";")

coordenadas<- data.frame(xmin=c(-78, -76, -76),
                         ymin=c(-57, -42, -30),
                         xmax=c(-63, -64, -64),
                         ymax=c(-42, -30, -18))

## separar por sectores y filtrar coordenadas
d1<- filter(data,
            Speed > 2)

d1<-filter(d1,
           Longitude  > coordenadas[1,1],
           Latitude  > coordenadas[1,2],
           Longitude < coordenadas[1,3],
           Latitude  < coordenadas[1,4])


# get the coordinates
coordinates <- d1[,3:2]

# compute the 2D binned kernel density estimate
est <- bkde2D(coordinates,
              bandwidth=c(0.05,0.05), #suaviza la distribuci?n
              gridsize=c(501L,501L), #2500 metros aproximadamente
              range.x=list(c(coordenadas[1,1],coordenadas[1,2]),
                           c(coordenadas[1,3],coordenadas[1,4])))

##range.x -> la diferencia en grados de minimas y maximas debe siempre ser la misma

### Le doy las coordenadas el ancho de banda el tamaño de la grilla y el rango va a cambiar
summary(est)

est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

# create raster
est.raster = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster) <- CRS("+init=epsg:4326")
xmin(est.raster) <- coordenadas[1,1]
xmax(est.raster) <- coordenadas[1,2]
ymin(est.raster) <- coordenadas[1,3]
ymax(est.raster) <-coordenadas[1,4]
# visually inspect the raster output
plot(est.raster)


summary(est.raster)
poligono<- rasterToPolygons(est.raster, dissolve = TRUE)
puntos <- coordinates(poligono) %>%
  as.data.frame()
p<- as.data.frame(puntos)
puntos <- st_as_sf(puntos, coords= c("V1", "V2"))


Value<- p %>% mutate(ind = raster::extract(est.raster, puntos))

chl<- st_read("comunas/spatial/rgn_cl.shp") %>%
  select(rgn_id = regin_d, rgn_name =regn_nm )
puntos<- st_as_sf(Value, coords= c("V1", "V2"),
                  crs = st_crs(chl))
sf_use_s2(FALSE)
p2 <-st_intersection(chl, puntos, join = st_overlaps)


p3 <- p2 %>%
  group_by(rgn_id,  rgn_name) %>%
  summarise(mean=mean(ind), median=median(ind))%>%
  mutate(std_value = (mean - 0)/ max(mean)-0 ) %>%
  as.data.frame() %>%
  select(rgn_id,rgn_name, mean, median, std_value)
write.table(p3, "clipboard", sep="\t", row.names=F)

