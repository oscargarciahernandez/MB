library(here)
source(here::here('libraries.R'))


Belesar_files<- list.files(here::here('Data/Parques/Belesar'), full.names = T)
Belesar_files1<- Belesar_files[!str_detect(Belesar_files, "E001")]
Belesar_files2<- Belesar_files1[str_detect(Belesar_files1,".RDS")]
Belesar_ultimo<- Belesar_files2[length(Belesar_files2)] 





#Para generar todas los CSV's
#lapply(Belesar_files2, Belesar_CSV_Generator)

##Generar CSV Belesar
Belesar_CSV_Generator(Belesar_ultimo)



#Para generar todas los CSV's
#lapply(Belesar_files2, Belesar_CSV_Generator_Point,point = 19)
#Geenrar CSV_Belesar
Belesar_CSV_Generator_Point<- function(Belesar_ultimo, point){
  Belesar_data<- readRDS(Belesar_ultimo)
  Belesar_lolat<- lon_lat_df_ls(Belesar_data)
  Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
  Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)
  
  nombres<- names(Belesar_rain)
  precipitacion_horaria<- lapply(Belesar_rain, function(x) x$prep_hourly)
  tabla_precip<- data.frame(matrix(nrow = length(precipitacion_horaria$`-8.02328491210938__42.1343421936035`)))
  for (i in 1:length(precipitacion_horaria)) {
    tabla_precip<- as.data.frame(cbind(tabla_precip,precipitacion_horaria[[i]]))
  }
  
  tabla_precip[,1]<- Belesar_rain[[1]]$fechas
  
  colnames(tabla_precip)<- c("Date", nombres)
  path<- here::here('Data/Parques/Belesar/')
  nombre1<- str_split(Belesar_ultimo, "/")
  nombre2<- nombre1[[1]][length(nombre1[[1]])]
  nombre3<- str_remove(nombre2 ,".RDS")
  
  path_nombre<-paste0(path,nombre3,"_",point,".CSV")
  
  tabla_precipita_localizacion<- tabla_precip[,c(1,point)]
  
  
  write.table(tabla_precipita_localizacion, path_nombre, 
              sep = ";",
              dec = ".", 
              row.names = F,
              quote = F)
  
}




#Download data... con ejecutar de vez en cuanto tenemos... 
# Down_E001_Belesar()


##Leer historico de Belesar... 

historico<- read.csv(here::here('DHI_Datos Belesar.csv'), sep = ";")
historico1<- historico[3:length(historico$X),]

col__names<- as.vector(unlist(lapply(historico[1,], as.character)))
col__names[1]<- "DATE"

colnames(historico1)<- col__names

historico1$DATE<- dmy_hms(historico1$DATE)
historico1[,-1]<- as.numeric(as.character(historico1[,-1]))





##Historico WRF 

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





x<- list.dirs(here::here('Data/Espana/'), recursive = T, full.names = T)
unique(x)
y<- list.files(here::here('Data/Espana/'), recursive = T, full.names = T)
y1<- str_split(y,"/") %>% lapply(., function(x) x[1:(length(x)-1)]) %>%  sapply(., function(x) paste(x, collapse = "/"))
y2<- unique(y1)
carpetas_vacias<- x[x%in%y2]

##Construir historico WRF
All_files_Spain<- list.files(here::here('Data/Espana/'), recursive = T, full.names = T)
d01_files<- All_files_Spain[!str_detect(All_files_Spain, "/d02/")]



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

Actualizar_Data_Parques_2(d01_files)



All_files_Belesar<- list.files(here::here('Data/Parques/Belesar/'), 
                             recursive = F, full.names = T)
RDS_Belesar<- All_files_Belesar[str_detect(All_files_Belesar), ".RDS"]

Belesar_data<- readRDS(RDS_Belesar[1])
Belesar_lolat<- lon_lat_df_ls(Belesar_data)
Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)






