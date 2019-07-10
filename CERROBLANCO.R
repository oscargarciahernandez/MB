library(here)
source('libraries.R')



# HACER HISTÓRICO CERROBLANCO ---------------------------------------------
#PONGO HACER HISTORICO EN FALSE PORQUE YA ESTÁ HECHO.... 
#O POR LO MENOS EN MI ORDENATA

HACER_HISTORICO_CERRO_BLANCO<- FALSE

if(HACER_HISTORICO_CERRO_BLANCO){
  All_files_Spain<- list.files(here::here('Data/Espana/'),
                               recursive = T, 
                               full.names = T)
  
  d01_files<- All_files_Spain[!str_detect(All_files_Spain, "/d02/")]
  RDS_Spain<- d01_files[str_detect(d01_files, ".RDS")]
  
  nombres<- sapply(str_split(RDS_Spain, "/"), function(x) x[length(x)]) 
  nombres1<- str_remove(nombres, "Espana_")
  nombres2<- str_remove(nombres1,".RDS")
  
  path1<-here::here('Data/Parques/CERROBLANCO/')
  
  
  for (i in 1:length(RDS_Spain)) {
    
    list_RDS<- readRDS(RDS_Spain[i])
    
    
    #CERROBLANKO
    
    Longitud_Parque= -2.409666
    Latitud_Parque= 38.72292
    
    path_cb1<- paste0(path1,nombres2[i] )
    if(!dir.exists(path_cb1)){dir.create(path_cb1, recursive = T)}
    
    
    path_cb<- paste0(path_cb1, "/", nombres2[i], ".RDS")
    if(!file.exists(path_cb)){
      cb_list<- Cortar_datos(list_hoy = list_RDS,
                             Longitud_Parque = Longitud_Parque,
                             Latitud_Parque=Latitud_Parque)
      
      saveRDS(cb_list, file = path_cb)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_cb))}
  }
  
  
  ##LEEMOS LOS RDS Y LOS METEMOS A UNA LISTA
  RDS_cb<- path1 %>% list.files(full.names = T, recursive = T) 
  lista_WRF_cb<- list()
  for (i in 1:length(RDS_cb)) {
    lista_WRF_cb[[i]]<- RDS_cb[[i]] %>% readRDS
    
  }
  
  
  r<- lista_WRF_cb %>% lapply(function(x){
    x2<- x %>% bind_rows(.id = "Date")
    
    x2$Date<- ifelse(nchar(x2$Date)<12, paste(x2$Date,"00:00:00"), x2$Date) %>% ymd_hms
    return(x2)
  }) %>% bind_rows()
  
  Lista_sindupli<- r %>% group_split(lon,lat) %>% 
    lapply(function(r2){
      
      r3<- r2[r2$Date %>% duplicated(), ]
      r3p<- r2[!r2$Date%in%r3$Date,]
      
      r4<- bind_rows(r3, r3p) %>% .[order(.$Date),]
      r5<- r4[!r4$Date %>% duplicated(),] 
      return(r5)
      
    })
  
  path_cerro_historico<- here::here('Data/Parques/CERROBLANCO/Historico/')
  if(!dir.exists(path_cerro_historico)){dir.create(path_cerro_historico)}
  saveRDS(Lista_sindupli, paste0(path_cerro_historico,"HISTORICO_WRF.RDS"))
  
  
  
  #longitud latitud parque
  Lista_sindupli<- readRDS( paste0(path_cerro_historico,"HISTORICO_WRF.RDS"))
  Longitud_Parque= -2.409666
  Latitud_Parque= 38.72292
  
  Punto_mascercano<- Lista_sindupli %>% sapply(function(x){
    vector_lon_lat<- c(x$lon %>% unique(),
                       x$lat %>% unique())
    distm(vector_lon_lat,
          c(Longitud_Parque,
            Latitud_Parque))
  }) %>% which.min()
  
  
  WRF_vercano<- Lista_sindupli[[Punto_mascercano]]
  
  
  
  u10<- WRF_vercano$U10_MEAN
  v10<- WRF_vercano$V10_MEAN
  wind_abs <- sqrt(u10^2 + v10^2)
  wind_dir_rad <-  atan2(u10/wind_abs, v10/wind_abs) 
  wind_dir_deg1 <-  wind_dir_rad * 180/pi 
  wind_dir_deg2 <-  wind_dir_deg1+ 180 
  
  WRF_vercano$Dir<- cut(wind_dir_deg2, 
                        breaks = c(0,seq(11.5,360,22.5), 361),
                        labels = seq(0,360,22.5))%>% 
    as.character() %>% ifelse(.==360,0,. ) %>% as.numeric()
  
  WRF_vercano$WS<- wind_abs
  WRF_vercano$WSmax<- sqrt(WRF_vercano$U10_MAX^2 + WRF_vercano$V10_MAX^2)
  

  WRF_sum<- WRF_vercano[complete.cases(WRF_vercano), ]
  saveRDS(WRF_sum, paste0(path_cerro_historico,"TABLA_WIND_CERROBLANCO_AFINADA.RDS"))
  
}



# TRATAMOS LOS DATOS PROPORCIONADOS POR LOS PARQUES EOLICOS ---------------
#CARGAMOS INFO DE CERROBLANCO
INFO_PE<-  here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS') %>% readRDS()
INFO_CB<- INFO_PE %>% filter(PARQUE=='P.E. Cerroblanco')


#CARGAMOS NUESTRO HISTÓRICO WRF
HIST_WRF<-  here::here('Data/Parques/CERROBLANCO/Historico/TABLA_WIND_CERROBLANCO_AFINADA.RDS') %>% readRDS()
colnames(HIST_WRF)<- HIST_WRF %>% colnames() %>% toupper()
HIST_WRF[,c("LON_WRF", "LAT_WRF")]<-HIST_WRF[,c("LON", "LAT")] 
HIST_WRF[,c("LON","LAT")]<- NULL

# JUNTAMOS LOS DATOS 
TABLA_MERGE<- left_join(INFO_CB, HIST_WRF, by= "DATE")


#NOS QUEDAMOS SOLO CON LOS DATOS COMPLETOS
TABLA_MERGE <- TABLA_MERGE %>% .[complete.cases(.), ] 


#VEMOS COMO AUNQUE HEMOS ELIMINADO MUCHAS FILAS FALTANTES 
#EXISTEN GRANDES PERIODOS DE TIEMPO CON HUECOS
ggplot(data = TABLA_MERGE)+
  geom_point(aes(y= PRUDUCCION_MWH, x=DATE))+
  geom_point(aes(y= WS, x=DATE), col="red")+ theme_light()




# CREAMOS MAPAS CON ROSAS DE LOS VIENTOS 
library(OpenStreetMap)
library(ggplot2)
library(here)
library(magrittr)
library(dplyr)
library(RColorBrewer)

n=max(TABLA_MERGE$LAT)    
s=min(TABLA_MERGE$LAT)    
e=max(TABLA_MERGE$LON)    
w=min(TABLA_MERGE$LON)    


#Fijamos incremento para hacer más grande el mapa

incr<- 0.1


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 2)  #Upper Left
lr <- round(c(s,e), digits = 2)  #Lower Right


#download_maps(ul, lr, maptyp = "bing", res=80)



mapa<- readRDS(here::here('Mapas/38.82_-2.31/bing80.RDS'))

#REPRESENTAMOS MAPA A PELO, SIN EJES NI NADA ELEMENT_BLANK FTW
pmap<- autoplot(mapa)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                             axis.text.y=element_blank(),axis.ticks=element_blank(),
                             axis.title.x=element_blank(),
                             axis.title.y=element_blank(),legend.position="none",
                             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank(),plot.background=element_blank())


#MOSTRAMOS
pmap


#lonros
lonros<- TABLA_MERGE$LON %>% unique()
latros<- TABLA_MERGE$LAT %>% unique()
TABLA_MERGE$lon<- TABLA_MERGE$LON
TABLA_MERGE$lat<- TABLA_MERGE$LAT
p_ros<- WR_parameters2(data = TABLA_MERGE, 
                       anchura = 0.1, 
                       paleta = "YlGnBu",
                       opacidad = 0.5, 
                       lon_pos = lonros, 
                       lat_pos = latros, 
                       spd_name = "WS",
                       dir_name = "DIR",border_size = 0.08)

pmap+p_ros$subgrobs
ggsave(here::here('RMDS/imagenes/cerroblanco2.png'), dpi= 600)




# ELEVATE R ---------------------------------------------------------------

library(elevatr)
library(rasterVis)

#DESCARGAR DATOS DE ELEVACION USANDO elevatr
#PLOTEAR DATOS CON RasterVis

#LO HE PENSADO PARA QUE FUNCIONE SELECCIONANDO n,s,e y w 
# DEL MISMO MODO QUE HACIAMOS PARA DESCARGAR LOS TILES CON 
# OPENSTREETMAP

n=max(TABLA_MERGE$LAT)    
s=min(TABLA_MERGE$LAT)    
e=max(TABLA_MERGE$LON)    
w=min(TABLA_MERGE$LON) 

incr<- 0.1


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 2)  #Upper Left
lr <- round(c(s,e), digits = 2)  #Lower Right



path_raster_cerroblanco<- here::here('Mapas/Raster_Cerroblanco/')
if(!dir.exists(path_raster_cerroblanco)){dir.create(path_raster_cerroblanco)}

#PONGO ESE IF DENTRO DEL BUCLE PORQUE TARDA LA VIDA
# Y SEGURAMENTE DESCARGARÉ LOS RASTERS DE VARIAS VECES...VARIOS DIAS
for (incremento in seq(0,1,0.2)) {
  file_raster<- paste0(path_raster_cerroblanco,"RASTER_",incremento,".RDS")
  if(file.exists(file_raster)){
    print("Este raster ya existe en: ", file_raster)
  }else{
    
    lon_location<- seq(lr[1]- incremento,ul[1]+ incremento,length.out = 100 ) 
    lat_location<- seq(ul[2]- incremento,lr[2]+ incremento, length.out = 100)
    data_loc<- expand.grid(lat_location,lon_location)
    colnames(data_loc)<- c("x", "y")
    
    spdf <- SpatialPointsDataFrame(coords = data_loc, data = data_loc,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    
    map_raster<- get_elev_raster(spdf, z=14)
    saveRDS(map_raster, file_raster)
    
  }
  

}


