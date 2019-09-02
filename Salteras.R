library(here)
source(here::here('libraries.R'))



########## HACER HISTÓRICO ############
All_files_Spain<- list.files(here::here('Data/Espana/'),
                             recursive = T, 
                             full.names = T)

d01_files<- All_files_Spain[!str_detect(All_files_Spain, "/d02/")]
RDS_files<- d01_files[str_detect(d01_files, ".RDS")]
RDS_files1<- RDS_files[!str_detect(RDS_files, "/NA/")]


Actualizar_Data_Salteras<- function(RDS_Spain){
  
  nombres<- sapply(str_split(RDS_Spain, "/"), function(x) x[length(x)]) 
  nombres1<- str_remove(nombres, "Espana_")
  nombres2<- str_remove(nombres1,".RDS")
  
  path1<-here::here('Data/Parques/')
  
  
  for (i in 1:length(RDS_Spain)) {
    
    list_RDS<- readRDS(RDS_Spain[i])
    
    
    #Salteras
    Longitud_Parque=-6.10
    Latitud_Parque=37.49
    
    path_Salteras<- paste0(path1,"Salteras/Salteras_",nombres2[i],".RDS" )
    
    if(!file.exists(path_Salteras)){
      Salteras_list<- Cortar_datos(list_hoy = list_RDS,
                                 Longitud_Parque = Longitud_Parque,
                                 Latitud_Parque=Latitud_Parque)
      
      saveRDS(Salteras_list, file = path_Salteras)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_Salteras))}
   
  }
}


#####Ejecutamos la función para crear el histórico de SALTERAS. 
#Actualizar_Data_Salteras(RDS_files1)



Salteras_files<- list.files(here::here('Data/Parques/Salteras/'),
                            full.names = T) %>% .[str_detect(., ".RDS")] %>% 
  str_remove(.,"Salteras_") %>% str_remove(.,".RDS") %>% ymd(.) %>% 
  .[] > ymd("2019/01/01")
Salteras_files_enero<- list.files(here::here('Data/Parques/Salteras/'),
                                  full.names = T)[Salteras_files]


Lista_localizacion<- list() 
for (i in 1:length(Salteras_files_enero)) {
  Salteras_data<- readRDS(Salteras_files_enero[i])
  Salteras_lolat<- lon_lat_df_ls(Salteras_data)
  Salteras_lolat1<- lapply(Salteras_lolat, uv_transformation)
  Salteras_rain<- lapply(Salteras_lolat1, extract_wind_data)
  Lista_localizacion[[i]]<- Salteras_rain
}





names_fechas<- sapply(Salteras_files_enero, function(x){
  r<- str_split(x, "/")
  return(str_remove(str_remove(r[[1]][length(r[[1]])], ".RDS"), "Salteras_"))
})

names(Lista_localizacion)<- names_fechas

Lista_localizacion2<- list()
for (i in 1:length(Lista_localizacion[[1]])) {
  Lista_localizacion2[[i]]<- lapply(Lista_localizacion, 
                                    function(x) return(x[[i]]))
}

names(Lista_localizacion2)<- names(Lista_localizacion[[1]])


Lista_total_MF<- lapply(Lista_localizacion2, function(x) bind_rows(x))

Lista_d1_d2_loc<- list()
for (i in 1:length(Lista_total_MF)) {
  p<- Lista_total_MF[[i]]
  d1<- p[duplicated(p$fechas),]
  d1<-d1[!duplicated(d1$fechas),]
  d2<- p[!duplicated(p$fechas),]
  
  d2_qneed1<-d2[!(d2$fechas%in%d1$fechas),]
  
  
  d1_2<-bind_rows(d1,d2_qneed1)
  d1_2<-d1_2[order(d1_2$fechas),]
  
  d2<-d2[order(d2$fechas),]
  
  d1_2$pre_acum<- NULL
  d2$pre_acum<- NULL
  
  colnames(d1_2)<- c("Date", "LON", "LAT", "S10_MEAN", "GUST10M","G10_MAX", "WS","WS_MAX", "WD", "WD_MAX")
  colnames(d2)<-  c("Date", "LON", "LAT", "S10_MEAN", "GUST10M","G10_MAX", "WS","WS_MAX", "WD", "WD_MAX")
  
  lista_loc_d12<- list(d1_2,d2)
  names(lista_loc_d12)<- c("D1", "D2")
  
  Lista_d1_d2_loc[[i]]<- lista_loc_d12
}

names(Lista_d1_d2_loc)<- names(Lista_total_MF)

if(!dir.exists(here::here('Data/Parques/Salteras/Historico'))){
  dir.create(here::here('Data/Parques/Salteras/Historico'))}

path_Salteras<- here::here('Data/Parques/Salteras/Historico/LISTA_LOCALIZACION.RDS')
saveRDS(Lista_d1_d2_loc, path_Salteras)



Lista_salteras<- readRDS(path_Salteras)

y<- lapply(Lista_salteras, function(x){
  y<- as.data.frame(cbind(as.character(x$D1$Date),x$D1$WS, x$D1$WD))
  colnames(y)<- c("Date", "WS","WD")
  return(y)
  
})

x<- names(y) %>% str_split(., "_") %>% 
  lapply(., function(x) cbind(x[1],x[3])) %>% 
  lapply(., function(x){
    colnames(x)<-c("lon", "lat") 
    return(x)}) %>% unlist(.) 


x<- data.frame(matrix(x, nrow=42, byrow=T),stringsAsFactors=FALSE)
colnames(x)<- c("Lon", "Lat")




Tabla_WS<- data.frame(matrix(-31,ncol = length(x)+1,
                             nrow = length(y[[1]]$Date)))
for (i in 1:length(x$Lon)) {
  Tabla_WS[,i]<- as.numeric(as.character(y[[i]]$WS))
  
}


Tabla_WS<- cbind(as.character(y[[1]]$Date), Tabla_WS)
lonlat<- as.data.frame(matrix(-31,ncol=length(Tabla_WS), nrow = 2))
colnames(lonlat)<- colnames(Tabla_WS)

Tabla_WS<- rbind(lonlat, Tabla_WS)
for (j in 2:length(Tabla_WS)) {
  Tabla_WS[1,j]<-   x$Lon[j]
  Tabla_WS[2,j]<-   x$Lat[j]
  
}

path_Salteras_Velocidad<- here::here('Data/Parques/Salteras/Historico/Tabla_WS.CSV')
write.table(Tabla_WS, path_Salteras_Velocidad, 
            sep = ",",
            dec = ".",
            col.names = F,
            row.names = F)






Tabla_WD<- data.frame(matrix(-31,ncol = length(x)+1,
                             nrow = length(y[[1]]$Date)))
for (i in 1:length(x$Lon)) {
  Tabla_WD[,i]<- as.numeric(as.character(y[[i]]$WD))
  
}


Tabla_WD<- cbind(as.character(y[[1]]$Date), Tabla_WD)
lonlat<- as.data.frame(matrix(-31,ncol=length(Tabla_WD), nrow = 2))
colnames(lonlat)<- colnames(Tabla_WD)

Tabla_WD<- rbind(lonlat, Tabla_WD)
for (j in 2:length(Tabla_WD)) {
  Tabla_WD[1,j]<-   x$Lon[j]
  Tabla_WD[2,j]<-   x$Lat[j]
  
}

path_Salteras_Velocidad<- here::here('Data/Parques/Salteras/Historico/Tabla_WD.CSV')
write.table(Tabla_WD, path_Salteras_Velocidad, 
            sep = ",",
            dec = ".",
            col.names = F,
            row.names = F)




# DONWLOAD SALTERAS  ------------------------------------------------------

library(XML)
library(request)

if(!dir.exists(here::here('Data/Parques/Salteras/WEB'))){dir.create(here::here('Data/Parques/Salteras/WEB'))}
####VEMOS EN LA PÁGINA QUE EL PRIMER DATO ES DE EL 20 DE DICIEMBRE 

#EL FORMATO ES EL SIGUIENTE YYYY-MM-D 2019-01-2
FIRST_DAY<- ymd("2019/05/11")

lista_web<- list()
k<- 1
while (TRUE) {
  HOY<- now() %>% as.character() %>% str_split(" ") %>% .[[1]] %>% .[1] %>% ymd()
  FECHA<- FIRST_DAY %>% as.character()
  URL_SALTERAS<- paste0("https://www.wunderground.com/dashboard/pws/ISALTERA2/table/",FECHA,"/",FECHA,"/daily")
  
  
  tables_salteras<- tryCatch({readHTMLTable(htmlParse(GET(URL_SALTERAS))) %>% .[[4]]},
                             error=function(e){"NO-DATA"})
  if(tables_salteras=="NO-DATA"){k<-k+1}else{
    colnames(tables_salteras)<- c("Time","Temperature",	"Dew Point",	"Humidity", 	"Wind", 	"Speed", 	"Gust",
                                  "Pressure",	"Precip. Rate.",	"Precip. Accum.", 	"UV",	"Solar")
    
    
    ########CAMBIOS DE UNIDADES SALTERAS
    tables_salteras$Temperature<- as.character(tables_salteras$Temperature) %>% 
      str_remove("F") %>% as.numeric() %>% -32 %>% "*"(5) %>% "/"(9) %>% round(digits = 2)
    
    tables_salteras$`Dew Point`<- as.character(tables_salteras$`Dew Point`) %>% 
      str_remove("F") %>% as.numeric() %>% -32 %>% "*"(5) %>% "/"(9) %>% round(digits = 2)
    tables_salteras$Humidity<- as.character(tables_salteras$Humidity) %>%
      str_remove("%") %>% as.numeric()
    tables_salteras$Speed<- as.character(tables_salteras$Speed) %>%
      str_remove("mph") %>% as.numeric() %>% "/"(2.237) %>% round(digits = 3)
    tables_salteras$Gust<- as.character(tables_salteras$Gust) %>%
      str_remove("mph") %>% as.numeric() %>% "/"(2.237) %>% round(digits = 3)
    
    tables_salteras$Pressure<- as.character(tables_salteras$Pressure) %>%
      str_remove("in") %>% as.numeric() %>% "*"(25.4) %>% round(digits = 3)
    tables_salteras$`Precip. Rate.`<- as.character(tables_salteras$`Precip. Rate.`) %>%
      str_remove("in") %>% as.numeric() %>% "*"(25.4) %>% round(digits = 3)
    tables_salteras$`Precip. Accum.`<- as.character(tables_salteras$`Precip. Accum.`) %>%
      str_remove("in") %>% as.numeric() %>% "*"(25.4) %>% round(digits = 3)
    
    
    tables_salteras$Solar<- as.character(tables_salteras$Solar) %>%
      str_remove("w/m²") %>% as.numeric()
    lista_web[[FIRST_DAY %>% as.character()]]<- tables_salteras
    
  }
  if(k>=10 | HOY==FIRST_DAY){break}
  FIRST_DAY<- FIRST_DAY+1
}


for (i in 1:length(lista_web)) {
  FECHA<- names(lista_web)[i] 
  lista_web[[i]]$Time  <- paste0(FECHA, " ",lista_web[[i]]$Time) %>% ymd_hm()
}

data_frame_salteras<- bind_rows(lista_web)
fecha_guardado<- now() %>% as.character() %>% str_replace(' ','_')
saveRDS(data_frame_salteras, paste0(here::here('Data/Parques/Salteras/WEB/Salteras_web_data_'),
                                    fecha_guardado,".RDS"))



# DATA CALMET -------------------------------------------------------------
#DOWNLOAD CALMET
#http://troposfera.es/datos/sevilla/calmet_20190522.nc
library(RNetCDF)

PATH_TO_CALMET = here::here('Data/Parques/Salteras/CALMET/')

CALMET_NC<- list.files(PATH_TO_CALMET, recursive = T, full.names = T) %>% .[str_detect(.,"nc")] 
sapply(CALMET_NC, function(x){
  if(file.size(x) < 200){
    file.remove(x)
  }
})

CALMET_NC<- list.files(PATH_TO_CALMET, recursive = T, full.names = T) %>% .[str_detect(.,"nc")] 
CALMET_NC %>% .[2] %>% open.nc() %>% print.nc()

Lista_Total<- list()
for (netcdf in 1:length(CALMET_NC)) {
  CALMET_netcdf<- CALMET_NC[netcdf] %>%  open.nc() %>% 
    read.nc(unpack = T)
  
  FECHAS_CALMET<- CALMET_netcdf$TFLAG[1,2,] %>% paste(str_sub(.,start = 1, end = 4),
                                                      str_sub(.,start = 5, end = 8)) %>% 
    str_split(' ') %>% lapply(., function(x){
      y<- ymd('2019-05-01')
      year(y)<- as.numeric(x[2])
      yday(y)<- as.numeric(x[3])
      return(y)
    }) %>% lapply(., function(x) as.character(x)) %>% unlist() 
  
  Lista_lon_lat<- list()
  for (lon in 1:80) {
    for (lat in 1:80) {
      
      List_calmet<- list()
      for (i in 2:length(CALMET_netcdf)){
        List_calmet[[i-1]]<-CALMET_netcdf[[i]][lon,lat,]
      }
      
      Tabla_calmet<- List_calmet %>% sapply(., function(x) x) %>% as.data.frame() %>% 
        cbind(FECHAS_CALMET,.) %>% as.data.frame()
      colnames(Tabla_calmet)<-c("Date",names(CALMET_netcdf)[2:length(CALMET_netcdf)])
      
      name_lista<- paste0(as.character(lon),
                          "_",
                          as.character(lat))
      Lista_lon_lat[[name_lista]]<- Tabla_calmet
    }
    
  }
  
  Lista_Total[[netcdf]]<- Lista_lon_lat
}


if(!dir.exists(here::here('Data/Parques/Salteras/CALMET'))){dir.create(here::here('Data/Parques/Salteras/CALMET'))}
saveRDS(Lista_Total, file = here::here('Data/Parques/Salteras/CALMET/Lista_20190828.RDS'))




# TRABAJAMOS CON CALMET ---------------------------------------------------
###A PARTIR DE AQUI HABRÁ QUE JUNTAR CALMET CON LOS DATOS OBTENIDOS DE LA PÁGINA WEB...
## QUEDA PENDIENTE AÑADIR LAS HORAS A LAS FECHAS, 


######SOLUCIONAMOS MIERDA FECHAS
#ESTO ME PARECE UNA MUY BUENA IDEA PARA RELLENAR LAS HORAS EN LAS FECHAS
#CARGAMOS LOS DATOS
salteras_infor<- readRDS( here::here('Data/Parques/Salteras/CALMET/Lista_20190828.RDS'))

#ARREGLAMOS FORMATO FECHAS... AÑADIENDO LAS HORAS
hours_ch<- as.character(seq(0,23)) %>% ifelse(nchar(.)==1, paste0(0,.), .) %>% rep(.,2)

salteras_infor<- salteras_infor  %>% lapply(., function(x){
  lapply(x, function(y){
    y$Date<- ymd_h(paste(as.character(y$Date), hours_ch[1:length(y$Date)] ))
    return(y)
  })
})

lista_merged<- list()
for (i in 1:length(salteras_infor[[1]])){
  salteras_table<- data.frame(matrix(ncol = ncol(salteras_infor[[1]][[1]])))
  colnames(salteras_table)<- colnames(salteras_infor[[1]][[1]])
  for (j in 1:length(salteras_infor)) {
    salteras_table<- bind_rows(salteras_table, salteras_infor[[j]][[i]])
    
  }
  lista_merged[[i]]<- salteras_table
  
}
names(lista_merged)<- names(salteras_infor[[1]])

lista_merged_clean<- lapply(lista_merged, function(x){
  x[complete.cases(x), ]
  x[!duplicated(x$Date),]
})
rm(salteras_infor)

View(lista_merged_clean)

####SOLUCIONAMOS MIERDA LON-LAT
###EL CALMET OFRECE SOLAMENTE LAS COORDENADAS DE ORIGEN EN UTM
### LA COORDENADA DE ORIGEN SE ENCUENTRA EN LA ESQUINA INFERIOR IZQUIERDA 
# SABIENDO QUE HAY 250 M ENTRE PUNTOS PUES YA SE PUENDE CALCULAR LA COORDENADA
##WGS84
LAT_orig<- 37.406
LON_orig<- 6.207


distance_grid<- seq(0,by=250,length.out = 80)
LONS<- destPoint(c(LON_orig,LAT_orig), 90, distance_grid, a=6378137)[,1]
LATS<- destPoint(c(LON_orig,LAT_orig), 0, distance_grid, a=6378137)[,2]

names(lista_merged_clean)<- names(lista_merged_clean) %>% str_split("_") %>% lapply(., function(x){
  paste0(LONS[as.numeric(as.character(x[1]))],"_",
    LATS[as.numeric(as.character(x[2]))])
})

path_Salteras<- here::here('Data/Parques/Salteras/CALMET/')
saveRDS(lista_merged_clean,paste0(path_Salteras, "Lista_20190828_clean.RDS"))





# JUNTAMOS CALMET Y WEB DATA ----------------------------------------------
####Historico unido
#CARGAMOS DATOS DE LA PÁGINA WEB
WEB<- list.files(here::here('Data/Parques/Salteras/WEB/'), full.names = T) %>% .[length(.)] %>% readRDS()
colnames(WEB)<- c("Date", colnames(WEB)[2:length(WEB)])

#CARGAMOS DATOS CALMET
path_Salteras<- here::here('Data/Parques/Salteras/CALMET/')
lista_merged_clean<-paste0(path_Salteras, "Lista_13_21_clean.RDS") %>%  readRDS()

LISTA_CONDATOSWEB<- list()
for (i in 1:length(lista_merged_clean)) {
  
  Historico_NEW<- lista_merged_clean[[i]] %>% .[complete.cases(.), ]
  
  WEB2<- data.frame(matrix(ncol=12))
  for (j in 1:nrow(Historico_NEW)) {
    WEB2[j,]<- WEB[which.min(abs(WEB$Date-Historico_NEW$Date[j])),]
  }
  colnames(WEB2)<- c("Date", colnames(WEB)[2:length(WEB)])
  WEB2$Date<- as.POSIXct(WEB2$Date,origin = "1970-01-01",tz = "GMT")
  WEB2<- WEB2[!duplicated(WEB2$Date),] 
  WEB2$Date<- round_date(WEB2$Date, unit = "hour")
  
  #JUNTAMOS DATOS 
  LISTA_CONDATOSWEB[[i]]<- left_join(Historico_NEW,WEB2, by="Date")
  
  
}
names(LISTA_CONDATOSWEB)<- names(lista_merged_clean)

saveRDS(LISTA_CONDATOSWEB,here::here('Data/Parques/Salteras/Tabla_unida_CALMET_WEB_13-22.RDS'))






# TRABAJAMOS CON LOS DATOS DE SALTERAS ------------------------------------
#### YA TENEMOS LOS DATOS GUARDADOS Y 

SALTERAS<- readRDS(here::here('Data/Parques/Salteras/Tabla_unida_CALMET_WEB_13-22.RDS'))

SALTERAS_VIENTO<-  SALTERAS %>% lapply(., function(x){
  y<- x[,c("Date", "U", "V","Wind","Speed","Gust")]
  u10<- y$U
  v10<- y$V
  wind_abs <- sqrt(u10^2 + v10^2)
  wind_dir_rad <-  atan2(u10/wind_abs, v10/wind_abs) 
  wind_dir_deg1 <-  wind_dir_rad * 180/pi 
  wind_dir_deg2 <-  wind_dir_deg1+ 180 
  
  y$Dir<-  ifelse(y$Wind=="North",0,
         ifelse(y$Wind=="NNE", 22.5,
                ifelse(y$Wind=="NE", 45,
                       ifelse(y$Wind=="ENE", 67.5,
                              ifelse(y$Wind=="East", 90,
                                     ifelse(y$Wind=="ESE", 112.5,
                                            ifelse(y$Wind=="SE", 135,
                                                   ifelse(y$Wind=="SSE", 157,
                                                          ifelse(y$Wind=="South",180,
                                                                 ifelse(y$Wind=="SSW", 202.5,
                                                                        ifelse(y$Wind=="SW", 225.0,
                                                                               ifelse(y$Wind=="WSW", 247.5,
                                                                                      ifelse(y$Wind=="West", 270,
                                                                                             ifelse(y$Wind=="WNW", 292.5,
                                                                                                    ifelse(y$Wind=="NW", 315,
                                                                                                           ifelse(y$Wind=="NNW", 337.5,NA))))))))))))))))
  
  


  y$wind_calmet<- wind_abs
  y$dir_calmet<- cut(wind_dir_deg2, 
                     breaks = c(0,seq(11.5,360,22.5), 361),
                     labels = seq(0,360,22.5))%>% 
    as.character() %>% ifelse(.==360,0,. ) %>% as.numeric()
  return(y)
})
names(SALTERAS_VIENTO)<- names(SALTERAS)


lat<-  37.488919 
lon<- -6.093440
Kauras<- c(lon,lat)

Dist_CALMET<-names(SALTERAS_VIENTO) %>% str_split("_") %>% sapply(., function(x){
  x<- as.numeric(x)
  y<- c(-x[1], x[2])
  distm(y, Kauras)
})


#RESTRICCION DE FECHA
#%>%.[which(.$Date>ymd("2019/05/20")),]

SALTERAS_VIENTO[[which.min(Dist_CALMET)]] %>%.[complete.cases(.), ]  %>% 
  ggplot()+
  geom_line(aes(x=Date, y=SMA(Speed, 2)),
                     colour="red")+
  geom_point(aes(x=Date, y=Speed),
            colour="red",
            alpha= 0.4,
            size=1.1)+
  geom_line(aes(x=Date, y=SMA(wind_calmet, 2)))+
  geom_point(aes(x=Date, y=wind_calmet),
            alpha= 0.4,
            size=1.1)+
  ylab("Wind Speed [m/s]")+
  labs(subtitle =  "CALMET OUTPUT(BLACK)\nVs\nOBSERVED(RED)")+
  theme_light()+
  theme(plot.subtitle =element_text(hjust = 0.5))

#LOCALIZACION KAURAS KOPRODUCTS



#Rosa de los vientos
#install.packages("openair")
library(openair)

PUNTOMC<- SALTERAS_VIENTO[[which.min(Dist_CALMET)]] %>%.[complete.cases(.), ] 


windRose(PUNTOMC,
         ws="wind_calmet",
         wd="dir_calmet",
         paddle = F,
         ws.int = 0.25,
         breaks = round(max(PUNTOMC$Speed)/0.25),
         cols = "jet",
         key.position = "right",
         offset = 0.01,
         dig.lab = 1)
windRose(PUNTOMC,
         ws="Speed",
         wd="Dir",
         paddle = F,
         ws.int = 0.25,
         breaks = round(max(PUNTOMC$Speed)/0.25),
         cols = "jet",
         key.position = "right",
         offset = 0.01,
         dig.lab = 1)