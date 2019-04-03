library(here)
source(here::here('libraries.R'))


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









# Cargar datos de los parques ---------------------------------------------

Lubian_files<- list.files(here::here('Data/Parques/Lubian/'), full.names = T)
Belesar_files<- list.files(here::here('Data/Parques/Belesar/'), full.names = T)
Lasia_files<- list.files(here::here('Data/Parques/LaSia/'), full.names = T)
Elcerro_files<- list.files(here::here('Data/Parques/ElCerro/'), full.names = T)

Lubian_list<- readRDS(Lubian_files)
ElCerro_list<- readRDS(Elcerro_files)
LaSia_list<- readRDS(Lasia_files)
LaBelesar_list<- readRDS(Belesar_files)







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





