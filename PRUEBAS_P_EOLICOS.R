library(here)
source('libraries.R')

lista_parques<- list()
for (i in 1:4) {
  PARQUE<- here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/Datos_Produccion_Disponibilidad.xlsx') %>% read.xls(sheet = i)
  
  PARQUE$FHUTC<- PARQUE$FHUTC %>% dmy_hm()
  lista_parques[[i]]<- PARQUE
}

saveRDS(lista_parques, here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS'))

# SACAMOS INFO KMZ Y POTENCIAS --------------------------------------------
#SACAMOS INFO DE LOS PARQUES
INFO_PARQUES<- here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/Datos_Produccion_Disponibilidad.xlsx') %>% read.xls(sheet = 5)

#SACAMOS LONGITUD Y LATITUD

LonLat_Parques<- Extract_Lon_Lat_from_Kmz(here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/'))

cbind(INFO_PARQUES[,c(3:5)], LonLat_Parques) %>% as.data.frame() %>% write.csv(here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/INFO_PARQUES.csv'))
      




#TRATAR DATOS  ---------------------------------------------------------
DATA_PARQUES<- read.csv(here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/INFO_PARQUES.csv'))
DATA_PARQUES

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf()+
  geom_point(data= DATA_PARQUES, aes(x= Longitude, y= Latitude), color= "red")



TABLA_PARQUES<- readRDS(here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS')) %>% bind_rows()
colnames(TABLA_PARQUES)<- c('PARQUE', 'DATE', 'PRUDUCCION_MWH', 'DISPONIBILIDAD')
head(TABLA_PARQUES)


TABLA_PARQUES %>% group_split(PARQUE) %>% lapply(function(x) print(paste(x$PARQUE %>% unique(),": Rango",x$DATE %>% range)))




lista_parques<- TABLA_PARQUES %>%  group_split(PARQUE) 

for (i in 1:length(lista_parques)) {
  x<- lista_parques[[i]]
  x$PARQUE<- DATA_PARQUES[i,1]
  x$CAPACIDAD<- DATA_PARQUES[i,2]
  x$N_TURBINAS<- DATA_PARQUES[i,3]
  x$LON<- DATA_PARQUES[i,5]
  x$LAT<- DATA_PARQUES[i,6]
  lista_parques[[i]]<- x
}

TABLA_PARQUES<- lista_parques %>% bind_rows()
saveRDS(TABLA_PARQUES, here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS'))




# TANTANKA ----------------------------------------------------------------

"
A CONTINUACION TRATAMOS LA SALIDA DEL MODELO DE TANTAKA PARA VER LAS DIFERENCIAS EXISTENTES DEBIDOS 

"

"
A CONTINUACION TRATAMOS LA SALIDA DEL MODELO DE TANTAKA PARA VER LAS DIFERENCIAS EXISTENTES DEBIDOS 

"
library(here)
source('libraries.R')
library(tools)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)



CSV_FILES<- list.files('/home/meteobit/TATANKA_OUTPUT/', recursive = T, full.names = T) %>% .[str_detect(., '.csv')]

CSV_FILES_D01_L0<- CSV_FILES %>% .[str_detect(., 'D01')] %>% .[str_detect(., 'L0')]

DATA_SIM<-CSV_FILES_D01_L0[1] %>% read.csv(header = T)

COORD_SIM<- DATA_SIM[,c("LON", 'LAT')] %>% unique()

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states$ID <- toTitleCase(states$ID)
states <- cbind(states, st_coordinates(st_centroid(states)))

incr= 5
ggplot(data = world) +
  geom_sf()+
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 5)+
  geom_point(data=COORD_SIM , aes(x= LON, y= LAT), color= "red")+
  coord_sf(xlim = c(min(COORD_SIM$LON) - incr , max(COORD_SIM$LON) + incr), 
           ylim = c(min(COORD_SIM$LAT) - incr , max(COORD_SIM$LAT) + incr), expand = FALSE)
theme_light()



library(rasterVis)
library(elevatr)

LON_TNK<- -98.955699 	
LAT_TNK<- 45.95685


n=max(COORD_SIM$LAT) 
s=min(COORD_SIM$LAT)    
e=max(COORD_SIM$LON)    
w=min(COORD_SIM$LON) 

incr<- 0.1


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 2)  #Upper Left
lr <- round(c(s,e), digits = 2)  #Lower Right


lon_location<- seq(lr[1],ul[1],length.out = 100 ) 
lat_location<- seq(ul[2],lr[2], length.out = 100)
data_loc<- expand.grid(lat_location,lon_location)
colnames(data_loc)<- c("x", "y")

spdf <- SpatialPointsDataFrame(coords = data_loc, data = data_loc,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

if(!dir.exists(here::here('Mapas/Raster_TATANKA/'))){dir.create(here::here('Mapas/Raster_TATANKA/'), recursive = T)}

if(file.exists(here::here('Mapas/Raster_TATANKA/map_raster1.RDS'))){
  map_raster1<- readRDS(here::here('Mapas/Raster_Cerroblanco/map_raster1.RDS'))
}else{
  map_raster1<- get_elev_raster(spdf, z=12)
  saveRDS(map_raster1,here::here('Mapas/Raster_TATANKA/map_raster1.RDS'))
}

levelplot(map_raster1) + 
  layer(panel.points(COORD_SIM$LON ,
                     COORD_SIM$LAT, pch=21, cex=1, colour='white', fill= 'white'))+
  layer(panel.points(LON_TNK ,
                     LAT_TNK, pch=21, cex=1, colour='white', fill= 'red'))



n=max(LAT_TNK) 
s=min(LAT_TNK)    
e=max(LON_TNK)    
w=min(LON_TNK) 

incr<- 0.1


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 2)  #Upper Left
lr <- round(c(s,e), digits = 2)  #Lower Right


lon_location<- seq(lr[1],ul[1],length.out = 100 ) 
lat_location<- seq(ul[2],lr[2], length.out = 100)
data_loc<- expand.grid(lat_location,lon_location)
colnames(data_loc)<- c("x", "y")

spdf <- SpatialPointsDataFrame(coords = data_loc, data = data_loc,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

if(!dir.exists(here::here('Mapas/Raster_TATANKA/'))){dir.create(here::here('Mapas/Raster_TATANKA/'), recursive = T)}

if(file.exists(here::here('Mapas/Raster_TATANKA/map_raster1.RDS'))){
  map_raster1<- readRDS(here::here('Mapas/Raster_Cerroblanco/map_raster1.RDS'))
}else{
  map_raster1<- get_elev_raster(spdf, z=12)
  saveRDS(map_raster1,here::here('Mapas/Raster_TATANKA/map_raster1.RDS'))
}

levelplot(map_raster1) + 
  layer(panel.points(COORD_SIM$LON ,
                     COORD_SIM$LAT, pch=21, cex=1, colour='white', fill= 'white'))+
  layer(panel.points(LON_TNK ,
                     LAT_TNK, pch=21, cex=1, colour='white', fill= 'red'))
