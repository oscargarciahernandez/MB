#install.packages("RNetCDF")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("here")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("tmap")



#install.packages("rJava")
#install.packages()
#library(tmap)
#library(OpenStreetMap)
#library(dplyr)
#library(xlsx)

library(ggplot2)
library(RNetCDF)
library(stringr)
library(lubridate)
library(request)
library(XML)
library(dplyr)
library(TTR)
library(data.table)
library(GGally)
library(e1071)
library(geosphere)



# Funciones principales para el tratamiento de  los datos -----------------

#Busca dentro la carpeta donde se generan los datos 
first_date<- function(netcdf_files){
  first_date<- str_split(netcdf_files[1],"/")[[1]]
  first_date_1<- str_replace(str_remove(first_date[length(first_date)], "wrfout_d01_"),"_"," ")
  first_date_2<- ymd_hms(first_date_1)
  return(first_date_2)
  
  
}
get_netcdf_list<- function(netcdf_files){
  fecha_ini<- str_split(netcdf_files[1],"/")[[1]]
  fecha_ini<- fecha_ini[length(fecha_ini)]
  fecha_ini<- str_remove(fecha_ini,"wrfout_d01_")
  fecha_ini<- str_replace(fecha_ini, "_"," ")
  
  netcdf_list<- list()
  vec_time<- vector()
  for (n_files in 1:length(netcdf_files)) {
    NC_prueba<- open.nc(netcdf_files[n_files])
    
    
    lons<-var.get.nc(NC_prueba,"XLONG",unpack=TRUE)
    lats<-var.get.nc(NC_prueba,"XLAT",unpack=TRUE)
    
    
    i<-1:nrow(lons)
    j<-1:ncol(lons)
    k<-1:(ncol(lons)*nrow(lons))
    LONS_COL<- 1:(ncol(lons)*nrow(lons))
    LATS_COL<- 1:(ncol(lons)*nrow(lons))
    LONS_COL[k]<-lons[i,j]
    LATS_COL[k]<-lats[i,j]
    
    
    
    #"ACCUMULATED TOTAL CUMULUS PRECIPITATION"  
    RAINC<- var.get.nc(NC_prueba, "RAINC", unpack = TRUE)   
    #"ACCUMULATED TOTAL GRID SCALE PRECIPITATION"
    RAINNC<- var.get.nc(NC_prueba, "RAINNC", unpack = TRUE)
    #ACCUMULATED SHALLOW CUMULUS PRECIPITATION
    RAINSH<- var.get.nc(NC_prueba, "RAINSH", unpack = TRUE)
    #"Period Maximum Total Precipitation Rate
    #MAX_PRATE<- var.get.nc(NC_prueba, "MAX_PRATE", unpack = TRUE)   
    #"Total Accumulated Precipitation
    #TACC_PRECIP<- var.get.nc(NC_prueba, "TACC_PRECIP", unpack = TRUE)
    #ACCUMULATED SHALLOW CUMULUS PRECIPITATION
    #INST_PRATE<- var.get.nc(NC_prueba, "INST_PRATE", unpack = TRUE)
    #Period Maximum Convective Precipitation Rate (RAINC)/DT
    #MAX_CURATE<- var.get.nc(NC_prueba, "MAX_CURATE", unpack = TRUE)
    #Period Maximum Freezing Rainfall Rate
    #MAX_ZRATE<- var.get.nc(NC_prueba, "MAX_ZRATE", unpack = TRUE)
    #Total Accumulated Rainfall
    #TACC_RAIN<- var.get.nc(NC_prueba, "TACC_RAIN", unpack = TRUE)
    
   
    
    
    #ACCUMULATED SNOW
    ACSNOW<- var.get.nc(NC_prueba, "ACSNOW", unpack = TRUE)
    #ACCUMULATED TOTAL GRID SCALE SNOW AND ICE
    SNOWNC<- var.get.nc(NC_prueba, "SNOWNC", unpack = TRUE)
    #FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)"
    SNOWC<- var.get.nc(NC_prueba, "SNOWC", unpack = TRUE)
    
    
    #""TEMP at 2 M" ;
    T2<- var.get.nc(NC_prueba, "T2", unpack = TRUE)
    #POT TEMP at 2 M
    TH2<- var.get.nc(NC_prueba, "TH2", unpack = TRUE)
    #"Minimum Shelter Temperature"
    T02_MIN<- var.get.nc(NC_prueba, "T02_MIN", unpack = TRUE)
    #"Maximum Shelter Temperature"
    T02_MAX<- var.get.nc(NC_prueba, "T02_MAX", unpack = TRUE)
    #"Mean Shelter Temperature" 
    T02_MEAN<- var.get.nc(NC_prueba, "T02_MEAN", unpack = TRUE)
    
    #U wind component of maximum 10 M wind speed
    V10_MAX<- var.get.nc(NC_prueba, "V10_MAX", unpack = TRUE)
    #U wind component of maximum 10 M wind speed
    U10_MAX<- var.get.nc(NC_prueba, "U10_MAX", unpack = TRUE)
    
    #Maximum 10 M wind speed"
    S10_MAX<- var.get.nc(NC_prueba, "S10_MAX", unpack = TRUE)
    #"Maximum 10 M wind gust speed"
    G10_MAX<- var.get.nc(NC_prueba, "G10_MAX", unpack = TRUE)
    #"Instantaneous 10 M wind gust speed potential
    GUST10M<- var.get.nc(NC_prueba, "GUST10M", unpack = TRUE)
    
    #Mean 10 M wind speed between output times" ;
    S10_MEAN<- var.get.nc(NC_prueba, "S10_MEAN", unpack = TRUE)
    #V wind component of mean 10 M wind speed" ;
    V10_MEAN<- var.get.nc(NC_prueba, "V10_MEAN", unpack = TRUE)
    #U wind component of mean 10 M wind speed" ;
    U10_MEAN<- var.get.nc(NC_prueba, "U10_MEAN", unpack = TRUE)
    
    
    #Terrain heigth m
    HGT<- var.get.nc(NC_prueba, "HGT", unpack = TRUE)
    
    #Surface pressure Pa
    PSFC<- var.get.nc(NC_prueba, "PSFC", unpack = TRUE)
    
    
    
    
    Tabla<-data.frame(LONS_COL,LATS_COL, RAINC[i,j][k], RAINNC[i,j][k], RAINSH[i,j][k],
                      U10_MEAN[i,j][k], U10_MAX[i,j][k],
                      V10_MEAN[i,j][k], V10_MAX[i,j][k],
                      S10_MEAN[i,j][k],GUST10M[i,j][k],G10_MAX[i,j][k],
                      T02_MEAN[i,j][k],T02_MAX[i,j][k],T02_MIN[i,j][k],
                      SNOWC[i,j][k],SNOWNC[i,j][k],ACSNOW[i,j][k],
                      HGT[i,j][k], 
                      PSFC[i,j][k])
    colnames(Tabla)<-c("lon","lat", "RAINC", "RAINNC", "RAINSH",
                       "U10_MEAN", "U10_MAX",
                       "V10_MEAN", "V10_MAX",
                       "S10_MEAN","GUST10M","G10_MAX",
                       "T02_MEAN","T02_MAX","T02_MIN",
                       "SNOWC","SNOWNC","ACSNOW",
                       "HGT", 
                       "PSFC")
    
    
    
    lonsU<- var.get.nc(NC_prueba, "XLONG_U", unpack = TRUE)
    latsU<- var.get.nc(NC_prueba, "XLAT_U", unpack = TRUE)
    i_u<-1:nrow(lonsU)
    j_u<-1:ncol(lonsU)
    k_u<-1:(ncol(lonsU)*nrow(lonsU))
    LONS_COL_u<- 1:(ncol(lonsU)*nrow(lonsU))
    LATS_COL_u<- 1:(ncol(lonsU)*nrow(lonsU))
    LONS_COL_u[k_u]<-lonsU[i_u,j_u]
    LATS_COL_u[k_u]<-latsU[i_u,j_u]
    
    lonsV<- var.get.nc(NC_prueba, "XLONG_V", unpack = TRUE)
    latsV<- var.get.nc(NC_prueba, "XLAT_V", unpack = TRUE)
    i_v<-1:nrow(lonsV)
    j_v<-1:ncol(lonsV)
    k_v<-1:(ncol(lonsV)*nrow(lonsV))
    LONS_COL_v<- 1:(ncol(lonsV)*nrow(lonsV))
    LATS_COL_v<- 1:(ncol(lonsV)*nrow(lonsV))
    LONS_COL_v[k_v]<-lonsV[i_v,j_v]
    LATS_COL_v[k_v]<-latsV[i_v,j_v]
    
    
    Bottom_Top_ZNU<- var.get.nc(NC_prueba, "ZNU", unpack = TRUE)
    Bottom_Top_STAG_ZNW<- var.get.nc(NC_prueba, "ZNW", unpack = TRUE)
    
    U_comp<-  var.get.nc(NC_prueba, "U", unpack = TRUE)
    V_comp<-  var.get.nc(NC_prueba, "V", unpack = TRUE)
    
    
    Tabla_U_level<-data.frame(LONS_COL_u,
                              LATS_COL_u, 
                              U_comp[i_u,j_u,40][k_u])
    Tabla_V_level<-data.frame(LONS_COL_v,
                              LATS_COL_v, 
                              V_comp[i_v,j_v,40][k_v])
    
    
    time<-  var.get.nc(NC_prueba, "XTIME", unpack = TRUE)
    time1<- as.data.frame(utcal.nc(paste0("minutes since ",fecha_ini), time))
    time2<- paste(time1$year,"-",time1$month,"-",
                  time1$day," ",time1$hour,"-",
                  time1$minute,"-",time1$second, sep = '')
    time3<- ymd_hms(time2)
    
    list_ch<- list(Tabla,
                   Tabla_V_level,
                   Tabla_U_level,
                   Bottom_Top_ZNU,
                   Bottom_Top_STAG_ZNW)
    names(list_ch)<- c("Variable", "V","U", "ZNU", "ZNW")
    
    netcdf_list[[n_files]]<- list_ch 
    vec_time[n_files]<- as.character(time3)
    
  }
  
  
  names(netcdf_list)<- vec_time
  
  return(netcdf_list)
}
Cortar_datos<- function(list_hoy, Longitud_Parque, Latitud_Parque){
  lista_parque<- list()
  
  for (j in 1:length(list_hoy)) {
    Datos<- list_hoy[[j]]$Variable
    
    lon <- list_hoy[[j]]$Variable$lon
    lat <- list_hoy[[j]]$Variable$lat
    
    lon_select<- lon[order(abs(lon-Longitud_Parque))[1:400]]
    lat_select<- lat[order(abs(lat-Latitud_Parque))[1:1000]]
    
    Datos1<- Datos[which(lon%in%lon_select), ]
    Datos2<- Datos1[Datos1$lat%in%lat_select, ]
    
    lista_parque[[j]]<- Datos2
    
  }
  
  names(lista_parque)<- names(list_hoy)
  return(lista_parque)
  
  
}
CSV_generator_Europe<- function(list_europe, path_europe){
 
  
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
    
    nombre<- vec_days_str[days] %>% str_split(., " ") %>% unlist(.)
    nombre_ent<- paste0("Europe_",nombre[1])
    
    if(!dir.exists(path_europe)){dir.create(path_europe)}
    write.table(CSV_Europe, paste0(path_europe,nombre_ent,'.CSV'), 
                sep = ";",
                dec = ".", 
                row.names = F,
                quote = F)
    
    
  }
  
}
CSV_generator_Spain<- function(list_espana,path_espana){
  
  
  vec_days_str<- names(list_espana)
  vec_days<- which(str_detect(vec_days_str, "14:00:00"))
  
  for (days in vec_days){
    CSV_Espana<- list_espana[[days]]$Variable[ ,c('lon', 'lat','T02_MAX')]
    CSV_Espana$T02_MAX<- CSV_Espana$T02_MAX-273.15 
    
    colnames(CSV_Espana)<- c("lon","lat","Temp")

    nombre<- vec_days_str[days] %>% str_split(., " ") %>% unlist(.)
    nombre_ent<- paste0("Espana_",nombre[1])
    
    if(!dir.exists(path_espana)){dir.create(path_espana)}
    write.table(CSV_Espana, paste0(path_espana, nombre_ent,'.CSV'), 
                sep = ";",
                dec = ".", 
                row.names = F,
                quote = F)
    
    
  }
  
  
  
}
lon_lat_df_ls<- function(parque_list){
  fecha<- names(parque_list)
  fecha[!str_detect(fecha, " ")]<- paste0(fecha[!str_detect(fecha, " ")], " 00:00:00")
  
  n_lon<- parque_list[[1]]$lon
  n_lat<- parque_list[[1]]$lat
  
  nombres<- paste0(n_lon,"__",n_lat)
  
  
  list_localizaciones<- list()
  for (localizaciones in 1:length(parque_list[[1]][,1]) ) {
    l<- lapply(parque_list, function(x) x[localizaciones,])
    df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
    df<- as.data.frame(cbind(ymd_hms(fecha), df))
    colnames(df)<-c("fechas", colnames(parque_list[[1]]))
    list_localizaciones[[localizaciones]]<- df
  }
  
  names(list_localizaciones)<- nombres
  return(list_localizaciones)
  
  
}
uv_transformation<- function(tabla_comp){
  
  nombres<- colnames(tabla_comp)
  
  u10<- tabla_comp$U10_MEAN
  v10<- tabla_comp$V10_MEAN
  
  u10_max<- tabla_comp$U10_MAX
  v10_max<- tabla_comp$V10_MAX
  
  wind_abs <- sqrt(u10^2 + v10^2)
  wind_dir_rad <-  atan2(u10/wind_abs, v10/wind_abs) 
  wind_dir_deg1 <-  wind_dir_rad * 180/pi 
  wind_dir_deg2 <-  wind_dir_deg1+ 180 
  
  
  wind_abs_max <- sqrt(u10_max^2 + v10_max^2)
  wind_dir_rad_max <-  atan2(u10_max/wind_abs_max, v10_max/wind_abs_max) 
  wind_dir_deg1_max <-  wind_dir_rad_max * 180/pi 
  wind_dir_deg2_max <-  wind_dir_deg1_max+ 180 
  
  tabla_comp<- as.data.frame(cbind(tabla_comp,wind_abs,wind_dir_deg2,
                                   wind_abs_max,wind_dir_deg2_max))
  colnames(tabla_comp)<- c(nombres,"WS","WD","WS_MAX","WD_MAX")
  tabla_comp$U10_MAX<- NULL
  tabla_comp$V10_MAX<- NULL
  tabla_comp$U10_MEAN<- NULL
  tabla_comp$V10_MEAN<- NULL
  return(tabla_comp)
  
}
extract_rain_data<- function(Belesar_lolat_df){
  
  rain<- Belesar_lolat_df[,c(1,2,3,4,5)]
  rain$pre_acum<- rain$RAINC+rain$RAINNC
  rain$RAINC<- NULL
  rain$RAINNC<- NULL
  
  prep_hourly<- vector()
  for (i in 1:length(rain$pre_acum)) {
    if(i==1){prep_hourly[i]<- rain$pre_acum[i]}else{
      prep_hourly[i]<- rain$pre_acum[i]-rain$pre_acum[i-1]
    }
    
  }
  rain$prep_hourly<- prep_hourly 
  
  return(rain)
}

extract_rain_data2<- function(Belesar_lolat_df){
  
  rain<- Belesar_lolat_df[,c(1,2,3,4,5,6,10,17,20)]
  rain$pre_acum<- rain$RAINC+rain$RAINNC
  
  prep_hourly<- vector()
  for (i in 1:length(rain$pre_acum)) {
    if(i==1){prep_hourly[i]<- rain$pre_acum[i]}else{
      prep_hourly[i]<- rain$pre_acum[i]-rain$pre_acum[i-1]
    }
    
  }
  rain$prep_hourly<- prep_hourly 
  
  return(rain)
}


Actualizar_Data_Parques<- function(){
  RDS_Spain<- list.files(here::here('Data/Espana/'), recursive=T, full.names = T)
  RDS_Spain<- RDS_Spain[str_detect(RDS_Spain, ".RDS")]
  
  nombres<- sapply(str_split(RDS_Spain, "/"), function(x) x[length(x)]) 
  nombres1<- str_remove(nombres, "Espana_")
  nombres2<- str_remove(nombres1,".RDS")
  
  path1<-here::here('Data/Parques/')
  
  
  for (i in 1:length(RDS_Spain)) {
    
    list_RDS<- readRDS(RDS_Spain[i])
    
    
    #Lubian
    Longitud_Parque=-6.84653
    Latitud_Parque=42.04514
    
    path_lubian<- paste0(path1,"Lubian/Lubian_",nombres2[i],".RDS" )
    
    if(!file.exists(path_lubian)){
      Lubian_list<- Cortar_datos(list_hoy = list_RDS,
                                 Longitud_Parque = Longitud_Parque,
                                 Latitud_Parque=Latitud_Parque)
      
      saveRDS(Lubian_list, file = path_lubian)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_lubian))}
    
    #El Cerro 
    Longitud_Parque=-3.64
    Latitud_Parque=42.87
    
    path_ElCerro<- paste0(path1,"ElCerro/ElCerro_",nombres2[i],".RDS" )
    
    if(!file.exists(path_ElCerro)){
      ElCerro_list<- Cortar_datos(list_hoy = list_RDS,
                                  Longitud_Parque = Longitud_Parque,
                                  Latitud_Parque=Latitud_Parque)
      
      saveRDS(ElCerro_list, file = path_ElCerro)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_ElCerro))}
    
    #La Sia
    Longitud_Parque=-3.57
    Latitud_Parque=43.14
    
    path_LaSia<- paste0(path1,"LaSia/LaSia_",nombres2[i],".RDS" )
    
    if(!file.exists(path_LaSia)){
      LaSia_list<- Cortar_datos(list_hoy = list_RDS,
                                Longitud_Parque = Longitud_Parque,
                                Latitud_Parque=Latitud_Parque)
      
      saveRDS(LaSia_list, file = path_LaSia)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_LaSia))}
    
    
    #La Belesar 
    Longitud_Parque=-7.6854
    Latitud_Parque=42.72343056 
    
    
    
    path_Belesar<- paste0(path1,"Belesar/Belesar_",nombres2[i],".RDS" )
    
    if(!file.exists(path_Belesar)){
      Belesar_list<- Cortar_datos(list_hoy = list_RDS,
                                  Longitud_Parque = Longitud_Parque,
                                  Latitud_Parque=Latitud_Parque)
      
      saveRDS(Belesar_list, file = path_Belesar)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_Belesar))}
    
  }
}

create_list_from_netcdf<- function(netcdf_files){
  fecha_ini<- str_split(netcdf_files[1],"/")[[1]]
  fecha_ini<- fecha_ini[length(fecha_ini)]
  fecha_ini<- str_remove(fecha_ini, "wrfout_d[[:digit:]]+_")
  fecha_ini<- str_replace(fecha_ini, "_"," ")
  fecha_ini<- str_replace_all(fecha_ini, "_", ":")
  
  netcdf_list<- list()
  vec_time<- vector()
  for (n_files in 1:length(netcdf_files)) {
    NC_prueba<- open.nc(netcdf_files[n_files])
    
    
    lons<-var.get.nc(NC_prueba,"XLONG",unpack=TRUE)
    lats<-var.get.nc(NC_prueba,"XLAT",unpack=TRUE)
    
    
    i<-1:nrow(lons)
    j<-1:ncol(lons)
    k<-1:(ncol(lons)*nrow(lons))
    LONS_COL<- 1:(ncol(lons)*nrow(lons))
    LATS_COL<- 1:(ncol(lons)*nrow(lons))
    LONS_COL[k]<-lons[i,j]
    LATS_COL[k]<-lats[i,j]
    
    
    
    #"ACCUMULATED TOTAL CUMULUS PRECIPITATION"  
    RAINC<- var.get.nc(NC_prueba, "RAINC", unpack = TRUE)   
    #"ACCUMULATED TOTAL GRID SCALE PRECIPITATION"
    RAINNC<- var.get.nc(NC_prueba, "RAINNC", unpack = TRUE)
    #ACCUMULATED SHALLOW CUMULUS PRECIPITATION
    RAINSH<- var.get.nc(NC_prueba, "RAINSH", unpack = TRUE)
    
    
    #ACCUMULATED SNOW
    ACSNOW<- var.get.nc(NC_prueba, "ACSNOW", unpack = TRUE)
    #ACCUMULATED TOTAL GRID SCALE SNOW AND ICE
    SNOWNC<- var.get.nc(NC_prueba, "SNOWNC", unpack = TRUE)
    #FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)"
    SNOWC<- var.get.nc(NC_prueba, "SNOWC", unpack = TRUE)
    
    
    #""TEMP at 2 M" ;
    T2<- var.get.nc(NC_prueba, "T2", unpack = TRUE)
    #POT TEMP at 2 M
    TH2<- var.get.nc(NC_prueba, "TH2", unpack = TRUE)
    #"Minimum Shelter Temperature"
    T02_MIN<- var.get.nc(NC_prueba, "T02_MIN", unpack = TRUE)
    #"Maximum Shelter Temperature"
    T02_MAX<- var.get.nc(NC_prueba, "T02_MAX", unpack = TRUE)
    #"Mean Shelter Temperature" 
    T02_MEAN<- var.get.nc(NC_prueba, "T02_MEAN", unpack = TRUE)
    
    #U wind component of maximum 10 M wind speed
    V10_MAX<- var.get.nc(NC_prueba, "V10_MAX", unpack = TRUE)
    #U wind component of maximum 10 M wind speed
    U10_MAX<- var.get.nc(NC_prueba, "U10_MAX", unpack = TRUE)
    
    #Maximum 10 M wind speed"
    S10_MAX<- var.get.nc(NC_prueba, "S10_MAX", unpack = TRUE)
    #"Maximum 10 M wind gust speed"
    G10_MAX<- var.get.nc(NC_prueba, "G10_MAX", unpack = TRUE)
    #"Instantaneous 10 M wind gust speed potential
    GUST10M<- var.get.nc(NC_prueba, "GUST10M", unpack = TRUE)
    
    #Mean 10 M wind speed between output times" ;
    S10_MEAN<- var.get.nc(NC_prueba, "S10_MEAN", unpack = TRUE)
    #V wind component of mean 10 M wind speed" ;
    V10_MEAN<- var.get.nc(NC_prueba, "V10_MEAN", unpack = TRUE)
    #U wind component of mean 10 M wind speed" ;
    U10_MEAN<- var.get.nc(NC_prueba, "U10_MEAN", unpack = TRUE)
    
    
    #Terrain heigth m
    HGT<- var.get.nc(NC_prueba, "HGT", unpack = TRUE)
    
    #Surface pressure Pa
    PSFC<- var.get.nc(NC_prueba, "PSFC", unpack = TRUE)
    
    
    
    
    Tabla<-data.frame(LONS_COL,LATS_COL, RAINC[i,j][k], RAINNC[i,j][k], RAINSH[i,j][k],
                      U10_MEAN[i,j][k], U10_MAX[i,j][k],
                      V10_MEAN[i,j][k], V10_MAX[i,j][k],
                      S10_MEAN[i,j][k],GUST10M[i,j][k],G10_MAX[i,j][k],
                      T02_MEAN[i,j][k],T02_MAX[i,j][k],T02_MIN[i,j][k],
                      SNOWC[i,j][k],SNOWNC[i,j][k],ACSNOW[i,j][k],
                      HGT[i,j][k], 
                      PSFC[i,j][k])
    colnames(Tabla)<-c("lon","lat", "RAINC", "RAINNC", "RAINSH",
                       "U10_MEAN", "U10_MAX",
                       "V10_MEAN", "V10_MAX",
                       "S10_MEAN","GUST10M","G10_MAX",
                       "T02_MEAN","T02_MAX","T02_MIN",
                       "SNOWC","SNOWNC","ACSNOW",
                       "HGT", 
                       "PSFC")
    
    
    
    lonsU<- var.get.nc(NC_prueba, "XLONG_U", unpack = TRUE)
    latsU<- var.get.nc(NC_prueba, "XLAT_U", unpack = TRUE)
    i_u<-1:nrow(lonsU)
    j_u<-1:ncol(lonsU)
    k_u<-1:(ncol(lonsU)*nrow(lonsU))
    LONS_COL_u<- 1:(ncol(lonsU)*nrow(lonsU))
    LATS_COL_u<- 1:(ncol(lonsU)*nrow(lonsU))
    LONS_COL_u[k_u]<-lonsU[i_u,j_u]
    LATS_COL_u[k_u]<-latsU[i_u,j_u]
    
    lonsV<- var.get.nc(NC_prueba, "XLONG_V", unpack = TRUE)
    latsV<- var.get.nc(NC_prueba, "XLAT_V", unpack = TRUE)
    i_v<-1:nrow(lonsV)
    j_v<-1:ncol(lonsV)
    k_v<-1:(ncol(lonsV)*nrow(lonsV))
    LONS_COL_v<- 1:(ncol(lonsV)*nrow(lonsV))
    LATS_COL_v<- 1:(ncol(lonsV)*nrow(lonsV))
    LONS_COL_v[k_v]<-lonsV[i_v,j_v]
    LATS_COL_v[k_v]<-latsV[i_v,j_v]
    
    
    Bottom_Top_ZNU<- var.get.nc(NC_prueba, "ZNU", unpack = TRUE)
    Bottom_Top_STAG_ZNW<- var.get.nc(NC_prueba, "ZNW", unpack = TRUE)
    
    U_comp<-  var.get.nc(NC_prueba, "U", unpack = TRUE)
    V_comp<-  var.get.nc(NC_prueba, "V", unpack = TRUE)
    
    
    Tabla_U_level<-data.frame(LONS_COL_u,
                              LATS_COL_u, 
                              U_comp[i_u,j_u,40][k_u])
    Tabla_V_level<-data.frame(LONS_COL_v,
                              LATS_COL_v, 
                              V_comp[i_v,j_v,40][k_v])
    
    
    time<-  var.get.nc(NC_prueba, "XTIME", unpack = TRUE)
    time1<- as.data.frame(utcal.nc(paste0("minutes since ",fecha_ini), time))
    time2<- paste(time1$year,"-",time1$month,"-",
                  time1$day," ",time1$hour,"-",
                  time1$minute,"-",time1$second, sep = '')
    time3<- ymd_hms(time2)
    
    list_ch<- list(Tabla,
                   Tabla_V_level,
                   Tabla_U_level,
                   Bottom_Top_ZNU,
                   Bottom_Top_STAG_ZNW)
    names(list_ch)<- c("Variable", "V","U", "ZNU", "ZNW")
    
    netcdf_list[[n_files]]<- list_ch 
    vec_time[n_files]<- as.character(time3)
    
  }
  
  
  names(netcdf_list)<- vec_time
  close.nc(NC_prueba)
  return(netcdf_list)
  
}



# BELESAR -----------------------------------------------------------------

#Funcion para crear gráficos de lluvia acumulada e instantanea
#Está pensada para meter los datos de una localización y verla
barplot_cumulative_Belesar<- function(Belesar_rain_cut){
  k<- max(Belesar_rain_cut$pre_acum)/max(Belesar_rain_cut$prep_hourly)
  ggplot(data=Belesar_rain_cut, aes(x=fechas))+
    geom_bar(aes(y=prep_hourly), stat="identity")+
    xlab("Date")+ylab("Lluvia por hora [mm/h]")+theme(panel.background = element_blank(), 
                                                      panel.grid = element_blank())+
    geom_line(aes(y = pre_acum/k), group = 1, col="red") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k), name = " LLuvia acumulada [mm]", 
                                           breaks = seq(min(Belesar_rain_cut$pre_acum),
                                                        max(Belesar_rain_cut$pre_acum),
                                                        by=1)),
                       breaks = seq(min(Belesar_rain_cut$prep_hourly),
                                    max(Belesar_rain_cut$prep_hourly),
                                    by=0.1))}


#La función que viene está pensada para hacer ploteos de varios gráficos y guardarlos 
#Por ello es necesario meter path_belesar
grafp_Belesar<- function(Belesar_rain_grid, path_belesar){
  nombres_archivos<- names(Belesar_rain_grid)
  for (i in 1:length(nombres_archivos)) {
    barplot_cumulative_Belesar(Belesar_rain_grid[[i]])
    ggsave(filename =paste0(path_belesar,"/",nombres_archivos[i]),
           device = "png",
           dpi=200,
           width = 7,
           height = 7,
           units = "in")
  }
}

##download belesar
Down_E001_Belesar<- function(){
  
  ###LLUVIA
  url<- "http://saih.chminosil.es/index.php?url=/datos/graficas_numeros/tag:E001_ACPA01H&historia="
  Lluvia_list<- list()
  i<- 0
  while(TRUE) {
    url1<- paste0(url,i)
    Lluvia<- httr::GET(url1)
    html_encoded<-content(Lluvia, "text", encoding = "ISO-8859-1")
    Belesar_real<-readHTMLTable(htmlParse(html_encoded), rm_nodata_cols = F)
    
    if(length(Belesar_real)==1){break}else{
      
      Belesar_precipitacion<- Belesar_real[[2]]
      colnames(Belesar_precipitacion)<- c("Date", "mm")
      Belesar_precipitacion<- Belesar_precipitacion [2:length(Belesar_precipitacion$Date),]
      Belesar_precipitacion$Date<- dmy_hm(Belesar_precipitacion$Date,tz=Sys.timezone()) 
      Belesar_precipitacion$Date<- with_tz(Belesar_precipitacion$Date, tzone="UTC")
      
      j<- i+1
      Lluvia_list[[j]]<- Belesar_precipitacion
      i<- i+1
    }
    
  }
  
  ###NIVEL
  url<- "http://saih.chminosil.es/index.php?url=/datos/graficas_numeros/tag:E001_AINEMBA&historia="
  Nivel_list<- list()
  i<- 0
  while(TRUE) {
    url1<- paste0(url,i)
    Nivel<- httr::GET(url1)
    html_encoded<-content(Nivel, "text", encoding = "ISO-8859-1")
    Belesar_real<-readHTMLTable(htmlParse(html_encoded), rm_nodata_cols = F)
    if(length(Belesar_real)==1){break}else{
      
      Belesar_nivel<- Belesar_real[[2]]
      colnames(Belesar_nivel)<- c("Date", "msnm")
      Belesar_nivel<- Belesar_nivel [2:length(Belesar_nivel$Date),]
      Belesar_nivel$Date<- dmy_hm(Belesar_nivel$Date,tz=Sys.timezone()) 
      Belesar_nivel$Date<- with_tz(Belesar_nivel$Date, tzone="UTC")
      j<- i+1
      Nivel_list[[j]]<- Belesar_nivel
      i<- i+1
    }
    
  }
  
  ###TEMP
  url<- "http://saih.chminosil.es/index.php?url=/datos/graficas_numeros/tag:E001_AITEMEX&historia="
  Temp_list<- list()
  i<- 0
  while(TRUE) {
    url1<- paste0(url,i)
    Temp<- httr::GET(url1)
    html_encoded<-content(Temp, "text", encoding = "ISO-8859-1")
    Belesar_real<-readHTMLTable(htmlParse(html_encoded), rm_nodata_cols = F)
    if(length(Belesar_real)==1){break}else{
      
      Belesar_Temp<- Belesar_real[[2]]
      colnames(Belesar_Temp)<- c("Date", "Temp[ºC]")
      Belesar_Temp<- Belesar_Temp [2:length(Belesar_Temp$Date),]
      Belesar_Temp$Date<- dmy_hm(Belesar_Temp$Date, tz=Sys.timezone()) 
      Belesar_Temp$Date<- with_tz(Belesar_Temp$Date, tzone="UTC")
      j<- i+1
      Temp_list[[j]]<- Belesar_Temp
      i<- i+1
    }
    
  }
  
  ###Vol
  url<- "http://saih.chminosil.es/index.php?url=/datos/graficas_numeros/tag:E001_ACVEMBA&historia="
  Vol_list<- list()
  i<- 0
  while(TRUE) {
    url1<- paste0(url,i)
    Vol<- httr::GET(url1)
    html_encoded<-content(Vol, "text", encoding = "ISO-8859-1")
    Belesar_real<-readHTMLTable(htmlParse(html_encoded), rm_nodata_cols = F)
    
    if(length(Belesar_real)==1){break}else{
      
      Belesar_Vol<- Belesar_real[[2]]
      colnames(Belesar_Vol)<- c("Date", "Vol[Hm³]")
      Belesar_Vol<- Belesar_Vol [2:length(Belesar_Vol$Date),]
      Belesar_Vol$Date<- dmy_hm(Belesar_Vol$Date, tz=Sys.timezone()) 
      Belesar_Vol$Date<- with_tz(Belesar_Vol$Date, tzone="UTC")
      j<- i+1
      Vol_list[[j]]<- Belesar_Vol
      i<- i+1
    }
    
  }
  
  
  
  ###PEr
  url<- "http://saih.chminosil.es/index.php?url=/datos/graficas_numeros/tag:E001_ACPORCE&historia="
  Per_list<- list()
  i<- 0
  while(TRUE) {
    url1<- paste0(url,i)
    Porcentaje<- httr::GET(url1)
    html_encoded<-content(Porcentaje, "text", encoding = "ISO-8859-1")
    Belesar_real<-readHTMLTable(htmlParse(html_encoded), rm_nodata_cols = F)
    
    
    
    
    if(length(Belesar_real)==1){break}else{
      Belesar_Porcentaje<- Belesar_real[[2]]
      colnames(Belesar_Porcentaje)<- c("Date", "Porcentaje[%]")
      Belesar_Porcentaje<- Belesar_Porcentaje [2:length(Belesar_Porcentaje$Date),]
      Belesar_Porcentaje$Date<- dmy_hm(Belesar_Porcentaje$Date, tz=Sys.timezone()) 
      Belesar_Porcentaje$Date<- with_tz(Belesar_Porcentaje$Date, tzone="UTC")
      j<- i+1
      Per_list[[j]]<- Belesar_Porcentaje
      i<- i+1
    }
    
  }
  
  df_lluvia <- bind_rows(Lluvia_list)
  df_per <- bind_rows(Per_list)
  df_vol <- bind_rows(Vol_list)
  df_nivel <- bind_rows(Nivel_list)
  df_temp <- bind_rows(Temp_list)
  
  Belesar_total<- as.data.frame(cbind(df_lluvia,
                                      df_nivel$msnm, 
                                      df_temp$`Temp[ºC]`,
                                      df_vol$`Vol[Hm³]`,
                                      df_per$`Porcentaje[%]`))
  colnames(Belesar_total)<- c("Date", "Lluvia[mm]", "Nivel[msnm]", "Temp[ºC]","Vol[Hm³]","Vol[%]")
  periodo<- sapply(str_split(as.character(range(Belesar_total$Date)), " "), function(x) x[1])
  nombre<- paste0(periodo[1], "_",periodo[2])
  path<- here::here('Data/Parques/Belesar/Historico/')
  path1<- paste0(path, "BelesarE001_", nombre,".RDS")
  path_base<- paste0(path, "BelesarE001_", nombre)
  n=2
  while(TRUE){
    if(file.exists(path1)){
      path1<- paste0(path_base,"_V",n,".RDS")}else{break}
    if(file.exists(path1)){n<- n+1}}
  saveRDS(Belesar_total, file = path1)
  
}


Actualizar_Historico_WRF_desdeElements<-function(){
  solo_fechas<- grepl("^[[:digit:]]+$",
                      list.dirs("/media/asus/Elements",
                                recursive = F, 
                                full.names = F))
  dirs_wrf<- list.dirs("/media/asus/Elements", recursive = F)[solo_fechas]
  
  for (dirs in 1:length(dirs_wrf)) {
    
    netcdf_files<- list.files(dirs_wrf[dirs], full.names = T)
    netcdf_files_d1<- netcdf_files[str_detect(netcdf_files, "wrfout_d01" )]
    netcdf_files_d2<- netcdf_files[str_detect(netcdf_files, "wrfout_d02" )]
    
    folder_name<- first_date(list.files(dirs_wrf[dirs]))
    folder_spain<- str_remove_all(as.character(folder_name),"-")
    
    path_check<- paste0(here::here('Data/Espana/'),folder_spain,"/")
    if(dir.exists(path_check)){
      print(paste0(("Archivo ya almacenado: "),path_check ))
      
    }else{
      if(length(netcdf_files_d2)==0 && length(netcdf_files_d1)==0){}else{
        
        if(length(netcdf_files_d2)==0){
          netcdf_files2<- netcdf_files_d1
          path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/d01/")
          
          if(!dir.exists(path_espana)){
            dir.create(paste0(here::here('Data/Espana/'),folder_spain, "/"))
            dir.create(path_espana)}
          
          list_espana<- create_list_from_netcdf(netcdf_files = netcdf_files2)
          
          saveRDS(list_espana, file = paste0(path_espana,"Espana_",folder_spain,".RDS"))
          
          
        }else{
          netcdf_files2<- list(netcdf_files_d1, netcdf_files_d2)
          names(netcdf_files2)<- c("dom1","dom2")
          
          for (dom in 1:2) {
            
            if(dom==1){path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/d01/")
            }else{path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/d02/")}
            
            if(!dir.exists(paste0(here::here('Data/Espana/'),folder_spain, "/"))){dir.create(paste0(here::here('Data/Espana/'),folder_spain, "/"))}
            if(!dir.exists(path_espana)){dir.create(path_espana)}
            
            list_espana<- create_list_from_netcdf(netcdf_files = netcdf_files2[[dom]])
            
            saveRDS(list_espana, file = paste0(path_espana,"Espana_",folder_spain,".RDS"))
          }
          
        }
      }
      
      
      
    }
    
    
    
    
    
    
    
    
  }
}


Actualizar_Data_Parques_2<- function(RDS_Spain){
  
  nombres<- sapply(str_split(RDS_Spain, "/"), function(x) x[length(x)]) 
  nombres1<- str_remove(nombres, "Espana_")
  nombres2<- str_remove(nombres1,".RDS")
  
  path1<-here::here('Data/Parques/')
  
  
  for (i in 1:length(RDS_Spain)) {
    
    list_RDS<- readRDS(RDS_Spain[i])
    
    
    #Lubian
    Longitud_Parque=-6.84653
    Latitud_Parque=42.04514
    
    path_lubian<- paste0(path1,"Lubian/Lubian_",nombres2[i],".RDS" )
    
    if(!file.exists(path_lubian)){
      Lubian_list<- Cortar_datos(list_hoy = list_RDS,
                                 Longitud_Parque = Longitud_Parque,
                                 Latitud_Parque=Latitud_Parque)
      
      saveRDS(Lubian_list, file = path_lubian)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_lubian))}
    
    #El Cerro 
    Longitud_Parque=-3.64
    Latitud_Parque=42.87
    
    path_ElCerro<- paste0(path1,"ElCerro/ElCerro_",nombres2[i],".RDS" )
    
    if(!file.exists(path_ElCerro)){
      ElCerro_list<- Cortar_datos(list_hoy = list_RDS,
                                  Longitud_Parque = Longitud_Parque,
                                  Latitud_Parque=Latitud_Parque)
      
      saveRDS(ElCerro_list, file = path_ElCerro)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_ElCerro))}
    
    #La Sia
    Longitud_Parque=-3.57
    Latitud_Parque=43.14
    
    path_LaSia<- paste0(path1,"LaSia/LaSia_",nombres2[i],".RDS" )
    
    if(!file.exists(path_LaSia)){
      LaSia_list<- Cortar_datos(list_hoy = list_RDS,
                                Longitud_Parque = Longitud_Parque,
                                Latitud_Parque=Latitud_Parque)
      
      saveRDS(LaSia_list, file = path_LaSia)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_LaSia))}
    
    
    #La Belesar 
    Longitud_Parque=-7.6854
    Latitud_Parque=42.72343056 
    
    
    
    path_Belesar<- paste0(path1,"Belesar/Belesar_",nombres2[i],".RDS" )
    
    if(!file.exists(path_Belesar)){
      Belesar_list<- Cortar_datos(list_hoy = list_RDS,
                                  Longitud_Parque = Longitud_Parque,
                                  Latitud_Parque=Latitud_Parque)
      
      saveRDS(Belesar_list, file = path_Belesar)
    }else{
      print(paste0("Hoy ya se ha guardado este archivo: ",path_Belesar))}
    
  }
}


Return_periodo_Belesar<- function(){
  All_files_Belesar<- list.files(here::here('Data/Parques/Belesar/'), 
                                 recursive = F, full.names = T)
  RDS_Belesar<- All_files_Belesar[str_detect(All_files_Belesar, ".RDS")]
  
  RDS_Belesar1<- RDS_Belesar[!str_detect(RDS_Belesar, "E001")]
  
  
  Belesar_data<- readRDS(RDS_Belesar1[1])
  Belesar_lolat<- lon_lat_df_ls(Belesar_data)
  Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
  Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)
  
  fecha_ini<- Belesar_rain$`-8.02328491210938__42.1343421936035`$fechas[1]
  
  Belesar_data<- readRDS(RDS_Belesar1[length(RDS_Belesar)])
  Belesar_lolat<- lon_lat_df_ls(Belesar_data)
  Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
  Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)
  
  fecha_last<- Belesar_rain$`-8.02328491210938__42.1343421936035`$fechas[length(Belesar_rain$`-8.02328491210938__42.1343421936035`$fechas)]
  
  periodo_WRF<- seq(fecha_ini, fecha_last, by="hour")
  
  Tabla_WRF<- as.data.frame(matrix(ncol = 10, nrow = length(periodo_WRF)))
  colnames(Tabla_WRF)<- colnames(Belesar_rain[[1]])
  Tabla_WRF$fechas<- periodo_WRF
  return(Tabla_WRF)
}

# Carpetas necesarias -----------------------------------------------------

if(!dir.exists(here::here('Data'))){dir.create(here::here('Data'))}
if(!dir.exists(here::here('Data/Europa/'))){dir.create(here::here('Data/Europa/'))}
if(!dir.exists(here::here('Data/Espana/'))){dir.create(here::here('Data/Espana/'))}

if(!dir.exists(here::here('Data/Parques'))){dir.create(here::here('Data/Parques'))}
if(!dir.exists(here::here('Data/Parques/Lubian'))){dir.create(here::here('Data/Parques/Lubian'))}
if(!dir.exists(here::here('Data/Parques/ElCerro'))){dir.create(here::here('Data/Parques/ElCerro'))}
if(!dir.exists(here::here('Data/Parques/LaSia'))){dir.create(here::here('Data/Parques/LaSia'))}
if(!dir.exists(here::here('Data/Parques/Salteras'))){dir.create(here::here('Data/Parques/Salteras'))}
if(!dir.exists(here::here('Data/Parques/Belesar'))){dir.create(here::here('Data/Parques/Belesar'))}
if(!dir.exists(here::here('graph/'))){dir.create(here::here('graph/'))}
if(!dir.exists(here::here('graph/Belesar/'))){dir.create(here::here('graph/Belesar/'))}
if(!dir.exists( here::here('Data/Parques/Belesar/CSV/'))){dir.create(here::here('Data/Parques/Belesar/CSV/'))}
if(!dir.exists(here::here('Data/Parques/Belesar/Historico/'))){dir.create(here::here('Data/Parques/Belesar/Historico/'))}
if(!dir.exists(here::here('Data/Parques/Belesar/Historico/WEB/'))){dir.create(here::here('Data/Parques/Belesar/Historico/WEB/'))}




# Maps --------------------------------------------------------------------

#Descargar mapas seteando coordenadas
download_maps<- function(ul,lr, 
                         maptyp=NULL,
                         res=40){
  if(is.character(maptyp)){
    maptypes<- maptyp
  }else{
    maptypes<- c("waze", "bing",
                 "esri","esri-topo", "nps", 
                 "apple-iphoto", "skobbler",
                 "hillshade")
  }
  if(length(maptypes)>1){
    res1<- res
    for (i in 1:length(maptypes)) {
      res<- res1
      
      
      tryCatch({
        while(TRUE){
          tryCatch({
            map1<- openmap(ul,lr, minNumTiles=res,
                           type=maptypes[i],
                           zoom=NULL)
            
          },error=function(e){cat("Error Java")})
          
          if(!exists("map1")){
            res<- res-1
            print("Bajando minNumtiles")
            if(res<1){break}
          }else{
            print(paste0(maptypes[i],": Descargado con minNumtiles=", res))
            break}
        }
        map.latlon <- openproj(map1, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        rm(map1)
        
        dirpath<- here::here(paste0("NUEVO/Mapas/",ul[1],"_",lr[2],"/"))
        
        if(!dir.exists(dirpath)){dir.create(dirpath)}
        
        save(map.latlon, file=paste0(dirpath,"/",maptypes[i],res,".Rdata"))
        print(paste0("Guardado ",paste0(dirpath,"/",maptypes[i],res,".Rdata")))
        rm(map1) 
        
        
      }, error=function(e){print(paste0(maptypes[i],": No descargado"))})
    }
    
  }else{
    while(TRUE){
      tryCatch({
        map1<- openmap(ul,lr, minNumTiles=res,
                       type=maptypes,
                       zoom=NULL)
        
      },error=function(e){cat("Error Java")})
      
      if(!exists("map1")){
        res<- res-1
        print("Bajando minNumtiles")
        if(res<1){break}
      }else{
        print(paste0(maptypes,": Descargado con minNumtiles=", res))
        break}
    }
    
    map.latlon <- openproj(map1, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    rm(map1)
    
    dirpath<- here::here(paste0("NUEVO/Mapas/",ul[1],"_",lr[2],"/"))
    
    if(!dir.exists(dirpath)){dir.create(dirpath)}
    save(map.latlon, file=paste0(dirpath,"/",maptypes,res,".Rdata"))
    print(paste0("Guardado ",paste0(dirpath,"/",maptypes,res,".Rdata")))
    
  }
  
}


