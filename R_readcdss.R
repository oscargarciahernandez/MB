library(here)
source(here::here('libraries.R'))


dir_path_spain<- "/usr1/uems/runs/spain1"
dir_path_europe<- "/usr1/uems/runs/europe1/"
folders_spain<- list.dirs(path = dir_path_spain)
folders_europe<- list.dirs(path=dir_path_europe)

netcdf_folder_spain<- folders_spain[str_detect(folders_spain, "wrfprd")]
netcdf_files_spain<- list.files(netcdf_folder_spain, full.names = T)

netcdf_folder_europe<- folders_europe[str_detect(folders_europe, "wrfprd")]
netcdf_files_europe<- list.files(netcdf_folder_europe, full.names = T)





#Esta funcion crea una lista con todas la variables que nos interesan
list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)

list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)



# Europe pressure and Temperature ---------------------------------------------------------

vec_days_str<- names(list_europe)
vec_days<- which(str_detect(vec_days_str, "14:00:00"))

for (days in vec_days){
  CSV_Europe<- list_europe[[days]]$Variable[ ,c('lon', 'lat', 
                                                              'T02_MAX',
                                                              'T02_MIN',
                                                              'HGT',
                                                              'PSFC')]
  CSV_Europe$Presion<- (CSV_Europe$PSFC/100)*(1-(0.0065*CSV_Europe$HGT)/((CSV_Europe$T02_MAX-273.15)+0.0065*CSV_Europe$HGT+273.15))^(-5.257)
  CSV_Europe$T02_MAX<- NULL
  CSV_Europe$HGT<- NULL
  CSV_Europe$PSFC<- NULL
  CSV_Europe$T02_MIN<- CSV_Europe$T02_MIN-273.15 
  
  colnames(CSV_Europe)<- c("lon","lat","Temp","Presion")
  nombre<- paste0("España_2019-04-03") 
  
  nombre<- vec_days_str[days] %>% str_split(., " ") %>% unlist(.)
  nombre_ent<- paste0("europe_",nombre[1])
  
  
  write.table(CSV_europe, here::here(paste0(nombre_ent,'.CSV')), 
              sep = ";",
              dec = ".", 
              row.names = F,
              quote = F)
  
  
}


# España temperatura ------------------------------------------------------


vec_days_str<- names(list_espana)
vec_days<- which(str_detect(vec_days_str, "14:00:00"))

for (days in vec_days){
  CSV_Espana<- list_espana[[days]]$Variable[ ,c('lon', 'lat','T02_MAX')]
  CSV_Espana$T02_MAX<- CSV_Espana$T02_MAX-273.15 
  
  colnames(CSV_Espana)<- c("lon","lat","Temp")
  nombre<- paste0("España_2019-04-03") 
  
  nombre<- vec_days_str[days] %>% str_split(., " ") %>% unlist(.)
  nombre_ent<- paste0("Espana_",nombre[1])
  
  
  write.table(CSV_Espana, here::here(paste0(nombre_ent,'.CSV')), 
              sep = ";",
              dec = ".", 
              row.names = F,
              quote = F)
  
  
  }




# Parques eólicos ---------------------------------------------------------

#Lubian
Longitud_Parque=-6.84653
Latitud_Parque=42.04514


#El Cerro 
Longitud_Parque=-3.64
Latitud_Parque=42.87

#La Sia
Longitud_Parque=-3.57
Latitud_Parque=43.14


#La Belesar 
Longitud_Parque=-7.6854
Latitud_Parque=42.72343056 



lon <- list_espana[[1]]$Variable$lon
lat <- list_espana[[1]]$Variable$lat

lon_select<- lon[order(abs(lon-Longitud_Parque))[1:400]]
lat_select<- lat[order(abs(lat-Latitud_Parque))[1:1000]]

lon_lat1<- list_espana[[1]]$Variable[which(lon%in%lon_select), c("lon","lat")]
lon_lat1<- lon_lat1[, c("lon","lat")]

lon_lat2<- lon_lat1[lon_lat1$lat%in%lat_select,]



#Seteamos el tamaño del mapa, para ello habrá que elegir 
n=max(lon_lat1$lat)    
s=min(lon)    
e=max(c(Coord_era$lon),Coord_anemo$lon)    
w=min(c(Coord_era$lon),Coord_anemo$lon)    


#Fijamos incremento para hacer más grande el mapa

incr<- 0.0215


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right
