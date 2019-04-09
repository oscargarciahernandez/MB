library(here)
source(here::here('libraries.R'))






# Generar CSV's de todas las localizacones y puntos concretos -------------



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





# Descargar Data_ Internet ------------------------------------------------


# Down_E001_Belesar()



# Leer historico DHI ------------------------------------------------------



historico<- read.csv(here::here('DHI_Datos Belesar.csv'), sep = ";")
historico1<- historico[3:length(historico$X),]

col__names<- as.vector(unlist(lapply(historico[1,], as.character)))
col__names[1]<- "DATE"

colnames(historico1)<- col__names

historico1$DATE<- dmy_hms(historico1$DATE)
historico2<-historico1[complete.cases(historico1),]

indx <- sapply(historico2, is.factor)
historico2[indx] <- lapply(historico2[indx], function(x) as.numeric(str_replace(as.character(x),",",".")))


path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_DHI_Belesar.RDS')
saveRDS(historico2, nombre_lista)



# Historico WRF -----------------------------------------------------------


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


Actualizar_Historico_WRF_desdeElements()


x<- list.dirs(here::here('Data/Espana/'), recursive = T, full.names = T)
unique(x)
y<- list.files(here::here('Data/Espana/'), recursive = T, full.names = T)
y1<- str_split(y,"/") %>% lapply(., function(x) x[1:(length(x)-1)]) %>%  sapply(., function(x) paste(x, collapse = "/"))
y2<- unique(y1)
carpetas_vacias<- x[x%in%y2]



# Historico WRF de parques ------------------------------------------------
All_files_Spain<- list.files(here::here('Data/Espana/'),
                             recursive = T, 
                             full.names = T)

d01_files<- All_files_Spain[!str_detect(All_files_Spain, "/d02/")]
RDS_files<- d01_files[str_detect(d01_files, ".RDS")]
RDS_files1<- RDS_files[!str_detect(RDS_files, "/NA/")]





Actualizar_Data_Parques_2(RDS_files1)



# Belesar. Construir historico --------------------------------------------



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

Tabla_WRF<- as.data.frame(matrix(ncol = 5, nrow = length(periodo_WRF)))
colnames(Tabla_WRF)<- colnames(Belesar_rain[[1]])
Tabla_WRF$fechas<- periodo_WRF

Lista_total<- list()
for (j in 1:length(Belesar_rain)) {
  Lista_localizacion<- list() 
  for (i in 1:length(RDS_Belesar)) {
    Belesar_data<- readRDS(RDS_Belesar1[i])
    Belesar_lolat<- lon_lat_df_ls(Belesar_data)
    Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
    Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)
    Lista_localizacion[[i]]<- Belesar_rain[[j]]
  }
  Lista_total[[j]]<- Lista_localizacion
}


names_fechas<- sapply(RDS_Belesar, function(x){
  r<- str_split(x, "/")
  return(r[length(r)])
})

names_loc<- names(Belesar_rain)

Lista_total2<- lapply(Lista_total, function(x) names(x)<- names_fechas)
names(Lista_total2)<- names_loc


Lista_total1<- Lista_total

Lista_total3<- list(Lista_total1[1:35],Lista_total)
Lista_total3<- Lista_total
Lista_total3[1:35]<- Lista_total1[1:35]

names(Lista_total3)<- names_loc

path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_WRF_Belesar.RDS')
nombre_lista2<- paste0(path_lista_total, 'Historico_WRF_Belesar_rdata.Rdata')
saveRDS(Lista_total3, nombre_lista)
save(Lista_total3, file=nombre_lista2)
