library(here)
source(here::here('libraries.R'))

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


#print.nc()  para informacion aunque ya la he guardado como CALMET_VARIABLES EN MB/
list.files(here::here(), recursive = T) %>% .[str_detect(.,"nc")] %>% .[2] %>% open.nc() %>% print.nc()



Lista_Total<- list()
for (netcdf in 1:19) {
  CALMET_netcdf<- list.files(here::here(), recursive = T) %>% .[str_detect(.,"nc")] %>% .[netcdf] %>% open.nc() %>% 
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

hours_ch<- as.character(seq(0,23)) %>% ifelse(nchar(.)==1, paste0(0,.), .) %>% rep(.,2)

Lista_Total<- Lista_Total%>% lapply(., function(x){
  lapply(x, function(y){
    y$Date<- ymd_h(paste(as.character(y$Date), hours_ch[1:length(y$Date)] ))
    return(y)
  })
})

lista_merged<- list()
for (i in 1:length(Lista_Total[[1]])){
  salteras_table<- data.frame(matrix(ncol = ncol(Lista_Total[[1]][[1]])))
  colnames(salteras_table)<- colnames(Lista_Total[[1]][[1]])
  for (j in 1:length(Lista_Total)) {
    salteras_table<- bind_rows(salteras_table, Lista_Total[[j]][[i]])
    
  }
  lista_merged[[i]]<- salteras_table
  
}
names(lista_merged)<- names(Lista_Total[[1]])

lista_merged_clean<- lapply(lista_merged, function(x){
  x[complete.cases(x), ]
  x[!duplicated(x$Date),]
})


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
saveRDS(lista_merged_clean,paste0(path_Salteras, "Lista_Total_clean.RDS"))





# JUNTAMOS CALMET Y WEB DATA ----------------------------------------------

#CARGAMOS DATOS DE LA PÁGINA WEB
WEB<- list.files(here::here('Data/Parques/Salteras/WEB/'), full.names = T) %>% .[length(.)] %>% readRDS()
colnames(WEB)<- c("Date", colnames(WEB)[2:length(WEB)])

#CARGAMOS DATOS CALMET
path_Salteras<- here::here('Data/Parques/Salteras/CALMET/')
lista_merged_clean<-paste0(path_Salteras, "Lista_Total_clean.RDS") %>%  readRDS()

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

SALTERAS_VIENTO<-  LISTA_CONDATOSWEB %>% lapply(., function(x){
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
names(SALTERAS_VIENTO)<- names(LISTA_CONDATOSWEB)

path_Salteras<- here::here('Data/Parques/Salteras/Historico/')
saveRDS(SALTERAS_VIENTO,paste0(path_Salteras, "LISTA_LOCALIZACIONES.RDS"))


# GRÁFICAS PRELIMINARES ---------------------------------------------------

View(SALTERAS_VIENTO)

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