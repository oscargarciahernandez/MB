
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




