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
#En principio esto habría que ejecutarlo una vez que tenemos los ficheros 
#Del modelo. 
Hoy<- as.character(lubridate::today())
list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)
list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)


if(!file.exists(paste0(here::here('Data/Europa/'),"/Europe_",Hoy,".RDS"))){
  saveRDS(list_europe, file = paste0(here::here('Data/Europa/'),"/Europe_",Hoy,".RDS"))
}
if(!file.exists(paste0(here::here('Data/Espana/'),"/Espana_",Hoy,".RDS"))){
  saveRDS(list_espana, file = paste0(here::here('Data/Espana/'),"/Espana_",Hoy,".RDS"))
}


# Europe pressure and Temperature ---------------------------------------------------------
CSV_generator_Europe(list_europe)

# España temperatura ------------------------------------------------------

CSV_generator_Spain(list_espana)

# Parques eólicos ---------------------------------------------------------
Hoy<- as.character(lubridate::today())
list_hoy<- readRDS(file = paste0(here::here('Data/Espana/'),"Espana_",Hoy,".RDS"))



#Lubian
Longitud_Parque=-6.84653
Latitud_Parque=42.04514
Lubian_list<- Cortar_datos(list_hoy = list_hoy,
             Longitud_Parque = Longitud_Parque,
             Latitud_Parque=Latitud_Parque)

if(!file.exists(paste0(here::here('Data/Parques/Lubian/'),"Lubian_",Hoy,".RDS"))){
  saveRDS(Lubian_list, file = paste0(here::here('Data/Parques/Lubian/'),"/Lubian_",Hoy,".RDS"))
}else{print(paste0("Hoy ya se han guardado este archivo: ", "Lubian_",Hoy,".RDS"))}

#El Cerro 
Longitud_Parque=-3.64
Latitud_Parque=42.87
ElCerro_list<- Cortar_datos(list_hoy = list_hoy,
                           Longitud_Parque = Longitud_Parque,
                           Latitud_Parque=Latitud_Parque)

if(!file.exists(paste0(here::here('Data/Parques/ElCerro/'),"/ElCerro_",Hoy,".RDS"))){
  saveRDS(ElCerro_list, file = paste0(here::here('Data/Parques/ElCerro/'),"ElCerro_",Hoy,".RDS"))
}else{print(paste0("Hoy ya se han guardado este archivo: ", "ElCerro_",Hoy,".RDS"))}
#La Sia
Longitud_Parque=-3.57
Latitud_Parque=43.14
LaSia_list<- Cortar_datos(list_hoy = list_hoy,
                           Longitud_Parque = Longitud_Parque,
                           Latitud_Parque=Latitud_Parque)

if(!file.exists(paste0(here::here('Data/Parques/LaSia/'),"/LaSia_",Hoy,".RDS"))){
  saveRDS(LaSia_list, file = paste0(here::here('Data/Parques/LaSia/'),"LaSia_",Hoy,".RDS"))
}else{print(paste0("Hoy ya se han guardado este archivo: ", "LaSia_",Hoy,".RDS"))}


#La Belesar 
Longitud_Parque=-7.6854
Latitud_Parque=42.72343056 
LaBelesar_list<- Cortar_datos(list_hoy = list_hoy,
                           Longitud_Parque = Longitud_Parque,
                           Latitud_Parque=Latitud_Parque)


if(!file.exists(paste0(here::here('Data/Parques/Belesar/'),"/Belesar_",Hoy,".RDS"))){
  saveRDS(LaBelesar_list, file = paste0(here::here('Data/Parques/Belesar/'),"Belesar_",Hoy,".RDS"))
}else{print(paste0("Hoy ya se han guardado este archivo: ", "Belesar_",Hoy,".RDS"))}





## Convertir a listas por localizaciones y con toda la serie temporal
Lubian_lolat<- lon_lat_df_ls(Lubian_list)
ElCerro_lolat<- lon_lat_df_ls(ElCerro_list)
LaSia_lolat<- lon_lat_df_ls(LaSia_list)
LaBelesar_lolat<- lon_lat_df_ls(LaBelesar_list)


##Convetir componentes U y V a formato WS y WD
Lubian_lolat<- lapply(Lubian_lolat, uv_transformation)
ElCerro_lolat<- lapply(ElCerro_lolat, uv_transformation)
LaSia_lolat<- lapply(LaSia_lolat, uv_transformation)
LaBelesar_lolat<- lapply(LaBelesar_lolat, uv_transformation)









#Lo de los mapas esta a la espera de poder instalar JAVA
# Mapeos de puntos y demas ------------------------------------------------


#Seteamos el tamaño del mapa, para ello habrá que elegir 
n=max(Datos2$lat)    
s=min(Datos2$lat)    
e=max(Datos2$lon)    
w=min(Datos2$lon)    


#Fijamos incremento para hacer más grande el mapa

incr<- 0.0215


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right


download_maps(ul, lr, maptyp = "bing", res=15)
