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

library(here)
source('libraries.R')
library(tools)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

PATH_TO_CSVS<- '/media/oscar/14002CD4002CBF1C/tamaulipas/'

LON_TNK<- -98.196080
LAT_TNK<- 25.78788

CSV_FILES<- list.files(PATH_TO_CSVS, recursive = T, full.names = T) %>% .[str_detect(., '.csv')]

CSV_FILES_D01<- CSV_FILES %>% .[str_detect(., 'D01')]
CSV_FILES_D02<- CSV_FILES %>% .[str_detect(., 'D02')]
CSV_FILES_D03<- CSV_FILES %>% .[str_detect(., 'D03')]


DATA_SIM_D01<-CSV_FILES_D01[1] %>% read.csv(header = T)
DATA_SIM_D02<-CSV_FILES_D02[1] %>% read.csv(header = T)
DATA_SIM_D03<-CSV_FILES_D03[1] %>% read.csv(header = T)

COORD_SIM_D01<- DATA_SIM_D01[,c("LON", 'LAT')] %>% unique()
COORD_SIM_D02<- DATA_SIM_D02[,c("LON", 'LAT')] %>% unique()
COORD_SIM_D03<- DATA_SIM_D03[,c("LON", 'LAT')] %>% unique()

MAPAS_NATURAL_EARTH<- FALSE
if(MAPAS_NATURAL_EARTH){
  world <- ne_countries(scale = "medium", returnclass = "sf")
  states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
  states$ID <- toTitleCase(states$ID)
  states <- cbind(states, st_coordinates(st_centroid(states)))
  
  incr= 0.05
  ggplot(data = world) +
    geom_sf()+
    geom_sf(data = states, fill = NA) + 
    geom_text(data = states, aes(X, Y, label = ID), size = 5)+
    geom_point(data=COORD_SIM_D01 , aes(x= LON, y= LAT), color= "red")+
    geom_point(data=COORD_SIM_D02 , aes(x= LON, y= LAT), color= "green", size= 0.7)+
    geom_point(data=COORD_SIM_D03 , aes(x= LON, y= LAT), color= "orange", size = 0.4)+
    
    coord_sf(xlim = c(min(COORD_SIM_D01$LON) - incr , max(COORD_SIM_D01$LON) + incr), 
             ylim = c(min(COORD_SIM_D01$LAT) - incr , max(COORD_SIM_D01$LAT) + incr), expand = FALSE)
  theme_light()
  
}



n=max(COORD_SIM_D01$LAT) 
s=min(COORD_SIM_D01$LAT)    
e=max(COORD_SIM_D01$LON)    
w=min(COORD_SIM_D01$LON) 

incr<- 5


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}





ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right

library(OpenStreetMap)

download_maps(ul, lr, maptyp = "bing", res=15)

TAMAULIPAS_MAP<- readRDS('/home/oscar/MB//Mapas/32.162_-91.78//bing15.RDS')
autoplot(TAMAULIPAS_MAP)+ 
  geom_point(aes(x= LON_TNK, y= LAT_TNK), cex= 3, col = 'red')
  




library(rasterVis)
library(elevatr)




n=max(COORD_SIM_D01$LAT) 
s=min(COORD_SIM_D01$LAT)    
e=max(COORD_SIM_D01$LON)    
w=min(COORD_SIM_D01$LON) 

incr<- 0.0005


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

if(!dir.exists(here::here('Mapas/Raster_TAMAULIPAS/'))){dir.create(here::here('Mapas/Raster_TAMAULIPAS/'), recursive = T)}

if(file.exists(here::here('Mapas/Raster_TAMAULIPAS/map_raster1.RDS'))){
  map_raster1<- readRDS(here::here('Mapas/Raster_TAMAULIPAS/map_raster1.RDS'))
}else{
  map_raster1<- get_elev_raster(spdf, z=12)
  saveRDS(map_raster1,here::here('Mapas/Raster_TAMAULIPAS/map_raster1.RDS'))
}

levelplot(map_raster1, par.settings= viridisTheme()) + 
  layer(panel.points(COORD_SIM_D01$LON ,
                     COORD_SIM_D01$LAT, pch=21, cex=1, colour='white', fill= 'white'))+
  layer(panel.points(LON_TNK ,
                     LAT_TNK, pch=21, cex=1, colour='white', fill= 'red'))


rm(map_raster1)
.rs.restartR()

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
  map_raster1<- readRDS(here::here('Mapas/Raster_TATANKA/map_raster2.RDS'))
}else{
  map_raster1<- get_elev_raster(spdf, z=12)
  saveRDS(map_raster1,here::here('Mapas/Raster_TATANKA/map_raster2.RDS'))
}

levelplot(map_raster1) + 
  layer(panel.points(COORD_SIM_D01$LON ,
                     COORD_SIM_D01$LAT, pch=21, cex=2, colour='white', fill= 'white'))+
layer(panel.points(COORD_SIM_D02$LON ,
                   COORD_SIM_D02$LAT, pch=21, cex=1.5, colour='white', fill= 'green'))+
  layer(panel.points(COORD_SIM_D03$LON ,
                     COORD_SIM_D03$LAT, pch=21, cex=1, colour='white', fill= 'orange'))+
  layer(panel.points(LON_TNK ,
                     LAT_TNK, pch=21, cex=1, colour='white', fill= 'red'))





CSV_FILES<- list.files(PATH_TO_CSVS, recursive = T, full.names = T) %>% .[str_detect(., '.csv')]
DOMINIOS<- c('D01', 'D02', 'D03')
LISTA_CORR_LEVEL<- list()
LISTA_DATOS_LEVELS<- list()
for(LEVELS in 1:10){
  LISTA_DATOS<- list()
  LISTA_CORR_DOM<- list()
  for(DOM in 1:3){
    
    CSV_FILES_D01<- CSV_FILES %>% .[str_detect(.,DOMINIOS[DOM])]
    
    
    #CREAMOS LA LISTA COMPLETA
    LISTA_SIM<- list()
    for( i in 1:length(CSV_FILES_D01)){
      TABLA_DOM<- CSV_FILES_D01[i] %>% read.csv(header = T)
      TABLA_DOM$SIM_TIME<- as.numeric(with(TABLA_DOM, difftime(ymd_hms(DATE),ymd_hms(DATE)[1],units="hours") ))

      LISTA_SIM[[i]]<-TABLA_DOM
    }
    
    TABLA_SIM<- bind_rows(LISTA_SIM)
    TABLA_SIM_0<- TABLA_SIM %>% group_split(LEVEL) %>% .[[LEVELS]]
    
    ADD_DISTANCIA<- FALSE
    if(ADD_DISTANCIA){
      
      for(i in 1:nrow(TABLA_SIM_0)){
        TABLA_SIM_0[i, 'DIST']<- distm(TABLA_SIM_0[i, c("LON", "LAT")], c(LON_TNK, LAT_TNK))
      }
      DISTANCIAS_MENORES<- TABLA_SIM_0$DIST %>% unique() %>% .[order(., decreasing = FALSE)] %>% .[1:50]
      
      
      TABLA_SIM_10<- TABLA_SIM_0[TABLA_SIM_0$DIST%in%DISTANCIAS_MENORES, ]
    }else{
      TABLA_SIM_10<- TABLA_SIM_0
    }

    
    
    TABLA_CORR<- data.frame(matrix(ncol = 8))
    for(i in 1:length( TABLA_SIM_10 %>% group_split(LON,LAT))){
      
      TABLA_INDV<- TABLA_SIM_10 %>% group_split(LON,LAT)%>% .[[i]]
      TABLA_INDV$DATE<-ymd_hms(TABLA_INDV$DATE)
      TABLA_1<- TABLA_INDV[TABLA_INDV$SIM_TIME<24,] %>% .[!duplicated(prueba),] %>% .[complete.cases(.),]
      TABLA_2<- TABLA_INDV[TABLA_INDV$SIM_TIME>24,] %>% .[!duplicated(prueba),] %>% .[complete.cases(.),]
      
      
      INFO_PARQUES<- readRDS(here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS'))
      INFO_TATANKA<- INFO_PARQUES[str_detect(INFO_PARQUES$PARQUE, 'Tamaulipas'),]
      TABLA_1_JOIN<- left_join(TABLA_1, INFO_TATANKA , by= 'DATE') %>% .[complete.cases(.),]
      TABLA_2_JOIN<- left_join(TABLA_2, INFO_TATANKA , by= 'DATE') %>% .[complete.cases(.),]
      
      
      
      
      
      TABLA_2_JOIN$PRUDUCCION_DISP_NORM<- (TABLA_2_JOIN$PRUDUCCION_MWH*TABLA_2_JOIN$DISPONIBILIDAD/100 - mean(TABLA_2_JOIN$PRUDUCCION_MWH*TABLA_2_JOIN$DISPONIBILIDAD/100))/sd(TABLA_2_JOIN$PRUDUCCION_MWH*TABLA_2_JOIN$DISPONIBILIDAD/100)
      TABLA_2_JOIN$PRUDUCCION_MWH_NORM<- (TABLA_2_JOIN$PRUDUCCION_MWH - mean(TABLA_2_JOIN$PRUDUCCION_MWH))/sd(TABLA_2_JOIN$PRUDUCCION_MWH)
      
      TABLA_2_JOIN$WS_NORM<- (TABLA_2_JOIN$WS - mean(TABLA_2_JOIN$WS))/sd(TABLA_2_JOIN$WS)
      TABLA_2_JOIN$WS3_NORM<- (TABLA_2_JOIN$WS^3 - mean(TABLA_2_JOIN$WS^3))/sd(TABLA_2_JOIN$WS^3)
      
      
      TABLA_1_JOIN$PRUDUCCION_DISP_NORM<- (TABLA_1_JOIN$PRUDUCCION_MWH*TABLA_1_JOIN$DISPONIBILIDAD/100 - mean(TABLA_1_JOIN$PRUDUCCION_MWH*TABLA_1_JOIN$DISPONIBILIDAD/100))/sd(TABLA_1_JOIN$PRUDUCCION_MWH*TABLA_1_JOIN$DISPONIBILIDAD/100)
      TABLA_1_JOIN$PRUDUCCION_MWH_NORM<- (TABLA_1_JOIN$PRUDUCCION_MWH - mean(TABLA_1_JOIN$PRUDUCCION_MWH))/sd(TABLA_1_JOIN$PRUDUCCION_MWH)
      
      TABLA_1_JOIN$WS_NORM<- (TABLA_1_JOIN$WS - mean(TABLA_1_JOIN$WS))/sd(TABLA_1_JOIN$WS)
      TABLA_1_JOIN$WS3_NORM<- (TABLA_1_JOIN$WS^3 - mean(TABLA_1_JOIN$WS^3))/sd(TABLA_1_JOIN$WS^3)
      
      
      
      
      CORR_WS_1<- cor(TABLA_1_JOIN$WS_NORM, TABLA_1_JOIN$PRUDUCCION_DISP_NORM, use = 'complete.obs')
      CORR_WS3_1<- cor(TABLA_1_JOIN$WS3_NORM, TABLA_1_JOIN$PRUDUCCION_DISP_NORM)
      
      CORR_WS_2<- cor(TABLA_2_JOIN$WS_NORM, TABLA_2_JOIN$PRUDUCCION_DISP_NORM)
      CORR_WS3_2<- cor(TABLA_2_JOIN$WS3_NORM, TABLA_2_JOIN$PRUDUCCION_DISP_NORM)
      
      CORR_WS_WO_NORM_1<- cor(TABLA_1_JOIN$WS, TABLA_1_JOIN$PRUDUCCION_MWH)
      CORR_WS_WO_NORM_2<- cor(TABLA_2_JOIN$WS, TABLA_2_JOIN$PRUDUCCION_MWH)
      
      CORR_WS3_WO_NORM_1<- cor(TABLA_1_JOIN$WS^3, TABLA_1_JOIN$PRUDUCCION_MWH)
      CORR_WS3_WO_NORM_2<- cor(TABLA_2_JOIN$WS^3, TABLA_2_JOIN$PRUDUCCION_MWH)
      
      TABLA_CORR[i,]<- c(CORR_WS_1, CORR_WS3_1, CORR_WS_WO_NORM_1,CORR_WS3_WO_NORM_1, 
                         CORR_WS_2, CORR_WS3_2, CORR_WS_WO_NORM_2,CORR_WS3_WO_NORM_2)
      
      
    }
    
    print(ggplot()+
            geom_line(data = TABLA_1_JOIN, aes(x= DATE, y= PRUDUCCION_MWH_NORM))+
            geom_line(data = TABLA_1_JOIN, aes(x= DATE, y= WS_NORM), col='red')+
            geom_line(data = TABLA_1_JOIN, aes(x= DATE, y= WS3_NORM), col='orange')+
            theme_light())
    
    LISTA_CORR_DOM[[DOM]]<- TABLA_CORR
    
  }
  
  LISTA_CORR_LEVEL[[LEVELS]]<- LISTA_CORR_DOM
  LISTA_DATOS_LEVELS[[LEVELS]]<- LISTA_DATOS
  
}




dir.create(here::here('Data/Parques/PRUEBA_EOLICOS/TAMAULIPAS_DATA/'))
saveRDS(LISTA_DATOS_LEVELS,here::here('Data/Parques/PRUEBA_EOLICOS/TAMAULIPAS_DATA/LISTA_DATOS.RDS'))
saveRDS(LISTA_CORR_LEVEL,here::here('Data/Parques/PRUEBA_EOLICOS/TAMAULIPAS_DATA/LISTA_CORRELATION.RDS'))


x<- LISTA_CORR_LEVEL[[1]]

LISTA_2<- LISTA_CORR_LEVEL %>% lapply(function(x){
  TABLA<- x %>% sapply(function(y){
    colMeans(y)
  })
  colnames(TABLA)<- c('D01', 'D02', 'D03')
  rownames(TABLA)<- c('WS_NORMALIZADO', 'WS3_NORMALIZADO', 'WS_SIN_NORMALIZAR', 'WS3_SIN_NORMALIZAR',
                      '2WS_NORMALIZADO', '2WS3_NORMALIZADO', '2WS_SIN_NORMALIZAR', '2WS3_SIN_NORMALIZAR')
  return(TABLA)
  })
names(LISTA_2)<- paste0('L', seq(1,10,1))
for(x in 1:length(LISTA_2)){
  TABLA<- as.data.frame(unname(LISTA_2[[x]]))
  colnames(TABLA)<- c('D01','D02', 'D03')
  TABLA$LEVEL<- names(LISTA_2)[x]
  TABLA$METHOD<- rownames(LISTA_2[[x]])
  LISTA_2[[x]]<- TABLA
}
TABLA_CORR<- bind_rows(LISTA_2)

TABLA_CORR_WS<- TABLA_CORR[!str_detect(TABLA_CORR$METHOD, 'WS3'),]

TABLA_CORR_WS_MELT_MEAN<- melt(TABLA_CORR_WS, id.vars=c("METHOD", 'LEVEL'))

ggplot(TABLA_CORR_WS_MELT_MEAN) +
  geom_point(aes(x=LEVEL ,y= value, color= METHOD, pch = variable), cex=5)+
  scale_x_discrete(limits=paste0('L', seq(1,10,1)))+
  theme_light()





LISTA_MAX<- LISTA_CORR_LEVEL %>% lapply(function(x){
  TABLA<- x %>% sapply(function(y){
    apply(y,2,max)
  })
  colnames(TABLA)<- c('D01', 'D02', 'D03')
  rownames(TABLA)<- c('WS_NORMALIZADO', 'WS3_NORMALIZADO', 'WS_SIN_NORMALIZAR', 'WS3_SIN_NORMALIZAR',
                      '2WS_NORMALIZADO', '2WS3_NORMALIZADO', '2WS_SIN_NORMALIZAR', '2WS3_SIN_NORMALIZAR')
  return(TABLA)
})
names(LISTA_MAX)<- paste0('L', seq(1,10,1))
for(x in 1:length(LISTA_MAX)){
  TABLA<- as.data.frame(unname(LISTA_MAX[[x]]))
  colnames(TABLA)<- c('D01','D02', 'D03')
  TABLA$LEVEL<- names(LISTA_MAX)[x]
  TABLA$METHOD<- rownames(LISTA_MAX[[x]])
  LISTA_MAX[[x]]<- TABLA
}
TABLA_CORR<- bind_rows(LISTA_MAX)

TABLA_CORR_WS<- TABLA_CORR[!str_detect(TABLA_CORR$METHOD, 'WS3'),]

TABLA_CORR_WS_MELT_MAX<- melt(TABLA_CORR_WS, id.vars=c("METHOD", 'LEVEL'))


ggplot(TABLA_CORR_WS_MELT_MAX) +
  geom_point(aes(x=LEVEL ,y= value, color= METHOD, pch = variable), cex=5)+
  scale_x_discrete(limits=paste0('L', seq(1,10,1)))+
  theme_light()






















cut_in<- 3.5
nominal<- 12
cut_off<- 25
max_power<- 3000

x<- data.frame(vwind= c(3.5, 11.5), power= c(0, 1500))
fit<- lm(power ~ exp(vwind), data = x)


plot(x$vwind, x$power, 
     xlim=c(0,30),
     ylim= c(0,1600), xlab= "Wind Speed (m/s)",
     ylab= "Power (kW)", 
     main= "AW77/1500 Estimated power curve ")
lines(seq(0,11.5,length.out = 1000), predict(fit, data.frame(vwind=seq(0,11.5,length.out = 1000))))
lines(x= c(11.5, 25, 25.1), y=c(1500,1500, 0))
lines(seq(0,11.5,length.out = 1000), 
      ((seq(0,11.5,length.out = 1000)^3*0.5*1.2*4657)/1000)*0.35, col="red")

r<-(((seq(0,11.5,length.out = 1000)^3*0.5*1.2*4657)/1000)*0.35)-
  predict(fit, data.frame(vwind=seq(0,11.5,length.out = 1000)))
mean_c<- (seq(0,11.5,length.out = 1000)^3*0.5*1.2*4657/1000)*0.35 - r/2
lines(seq(0,11.5,length.out = 1000), 
      mean_c, col="blue")


CURVA_FINAL<- data.frame(Vwind= c(seq(0,11.5,length.out = 1000),
                                  seq(11.51,25,length.out = 100)),
                         power= c(mean_c, rep(1500, 100)),
                         power2= c(((seq(0,11.5,length.out = 1000)^3*0.5*1.2*4657)/1000)*0.35, rep(1500,100)),
                         power3= c(predict(fit, data.frame(vwind=seq(0,11.5,length.out = 1000))), rep(1500,100)))
CURVA_FINAL$power<- ifelse(CURVA_FINAL$Vwind<3.5, 0, CURVA_FINAL$power)
ggplot(CURVA_FINAL)+
  geom_line(aes(x= Vwind, y=power))+
  theme_light()