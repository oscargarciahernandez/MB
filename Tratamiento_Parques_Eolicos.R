library(here)
source(here::here('libraries.R'))


# Actualizar data parques eólicos ---------------------------------------------------------
Actualizar_Data_Parques()


# Cargar Data -------------------------------------------------------------

##Buscar Data
Lubian_files<- list.files(here::here('Data/Parques/Lubian'), full.names = T)
Belesar_files<- list.files(here::here('Data/Parques/Belesar'), full.names = T)
Lasia_files<- list.files(here::here('Data/Parques/LaSia'), full.names = T)
Elcerro_files<- list.files(here::here('Data/Parques/ElCerro'), full.names = T)


#Elegir la ultima
Lubian_list<- readRDS(Lubian_files[length(Lubian_files)])
ElCerro_list<- readRDS(Elcerro_files[length(Elcerro_files)])
LaSia_list<- readRDS(Lasia_files[length(Lasia_files)])
Belesar_list<- readRDS(Belesar_files[length(Belesar_files)])


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



## BELESAR TRATAMIENTO
Belesar_rain<- lapply(LaBelesar_lolat, extract_rain_data)



prueba<-Belesar_rain$`-8.02328491210938__42.1343421936035`
barplot_cumulative_Belesar(prueba)














