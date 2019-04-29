library(here)
source(here::here('libraries.R'))


# Función de descarga -----------------------------------------------------
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
  nombre<- str_replace(as.character(ymd_hms(now())), " ", "_")
  path<- here::here('Data/Parques/Belesar/Historico/WEB/')
  path1<- paste0(path, "BelesarE001_", nombre,".RDS")
  path_base<- paste0(path, "BelesarE001_", nombre)
  n=2
  while(TRUE){
    if(file.exists(path1)){
      path1<- paste0(path_base,"_V",n,".RDS")}else{break}
    if(file.exists(path1)){n<- n+1}}
  saveRDS(Belesar_total, file = path1)
  
}

Down_E001_Belesar()


# Juntar y guardar histórico ----------------------------------------------
hist1<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/'), full.names = T)
hist2<- hist1[str_detect(hist1, "HIST_WEB_")]
hist3<- str_remove(str_remove(hist2,  "HIST_WEB_"), ".RDS") %>% 
  str_replace(.,"_"," ") %>% ymd_hms()
path_hist<- hist2[which.max(hist3)]


hist<- readRDS(path_hist)



RDS_down_path<- list.files(here::here('Data/Parques/Belesar/Historico/WEB'), 
                           full.names = T) %>%
  .[str_detect(.,"E001")] %>% .[str_detect(., ":")]


download_day<- sapply(str_split(RDS_down_path, "_"), 
                                 function(x) paste0(x[2], " ",
                                                    str_remove(x[3], ".RDS")))%>%  ymd_hms(.)



index_min<- which.max(download_day)



RDS_download<- readRDS(RDS_down_path[index_min])
RDS_download1<-as.data.frame(apply(RDS_download[2:length(RDS_download)],2, function(x) as.numeric(as.character(str_replace_all( x,",", "."))) ))
RDS_download1$Date<- ymd_hms(RDS_download$Date)
RDS_download1<- RDS_download1[, c(6,1,2,3,4,5)]
colnames(RDS_download1)<- c("Date", "lluvia","nivel",
                            "Temp", "Vol", "porcentaje")



completando<- hist[!hist$Date%in%RDS_download1$Date,]
completado<- bind_rows(RDS_download1, completando)
completado<- completado[order(completado$Date),]


path_hist<- here::here('Data/Parques/Belesar/Historico/WEB/')
name<- paste0("HIST_WEB_",str_replace(as.character(ymd_hms(now())), " ", "_"), ".RDS")
path_total<- paste0(path_hist, name)
saveRDS(completado, path_total)


# Actualizar histórico WRF ------------------------------------------------



#Listamos archivos dentro de la carpeta de Belesar
All_files_Belesar<- list.files(here::here('Data/Parques/Belesar/'),
                               recursive = F, full.names = T)


#Detectamos cuales son RDS
RDS_Belesar<- All_files_Belesar[str_detect(All_files_Belesar, ".RDS")]

#Eliminamos los RDS que no son de WRF
RDS_Belesar1<- RDS_Belesar[!str_detect(RDS_Belesar, "E001")]

#Construimos Lista para cada instante de tiempo
Lista_localizacion<- list() 
for (i in 1:length(RDS_Belesar1)) {
  Belesar_data<- readRDS(RDS_Belesar1[i])
  Belesar_lolat<- lon_lat_df_ls(Belesar_data)
  Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
  Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data2)
  Lista_localizacion[[i]]<- Belesar_rain
}

#Nombramos la lista
names_fechas<- sapply(RDS_Belesar1, function(x){
  r<- str_split(x, "/")
  return(str_remove(str_remove(r[[1]][length(r[[1]])], ".RDS"), "Belesar_"))
})
names(Lista_localizacion)<- names_fechas


#Creamos una lista por localización
Lista_localizacion2<- list()
for (i in 1:length(Lista_localizacion[[1]])) {
  Lista_localizacion2[[i]]<- lapply(Lista_localizacion, 
                                    function(x) return(x[[i]]))
}

#Nombramos la lista
names(Lista_localizacion2)<- names(Lista_localizacion[[1]])

#Guardamos la lista
path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_WRF_Belesar_Variables.RDS')
saveRDS(Lista_localizacion2, nombre_lista)

#Cargamos lista
path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_WRF_Belesar_Variables.RDS')
Lista_total1<- readRDS(nombre_lista)

#Juntamos todos los Dataframes
Lista_total_MF<- lapply(Lista_total1, function(x) bind_rows(x))

#creamos dos data.frames... uno para D1 y otro para D2
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
  
  colnames(d1_2)<- c("Date", "LON", "LAT", "RAINC", "RAINNC","RAINSH", "T02_MEAN","PSFC","WS_MAX", "prep_hourly")
  
  colnames(d2)<-  c("Date", "LON", "LAT", "RAINC", "RAINNC","RAINSH", "T02_MEAN","PSFC","WS_MAX", "prep_hourly")
  
  lista_loc_d12<- list(d1_2,d2)
  names(lista_loc_d12)<- c("D1", "D2")
  
  Lista_d1_d2_loc[[i]]<- lista_loc_d12
}

#nombramos la lsta
names(Lista_d1_d2_loc)<- names(Lista_total_MF)

Tabla_periodo1<- Return_periodo_Belesar()
colnames(Tabla_periodo1)<- c("Date", "LON", "LAT", "RAINC", "RAINNC","RAINSH", "T02_MEAN","PSFC","WS_MAX", "prep_hourly")

Lista_d1_d2_loc2<- list()
for (j in 1:length(Lista_d1_d2_loc)) {
  prueba_list<- Lista_d1_d2_loc[[j]]
  lista_retorno<- list()
  for(i in 1:2){
    prueba<- prueba_list[[i]]
    Tabla_periodo<- Tabla_periodo1
    Tabla_periodo$LON[match(prueba$Date,Tabla_periodo$Date)] <- prueba$LON
    Tabla_periodo$LAT[match(prueba$Date,Tabla_periodo$Date)] <- prueba$LAT
    Tabla_periodo$RAINC[match(prueba$Date,Tabla_periodo$Date)] <- prueba$RAINC
    Tabla_periodo$RAINNC[match(prueba$Date,Tabla_periodo$Date)] <- prueba$RAINNC
    Tabla_periodo$RAINSH[match(prueba$Date,Tabla_periodo$Date)] <- prueba$RAINSH
    Tabla_periodo$T02_MEAN[match(prueba$Date,Tabla_periodo$Date)] <- prueba$T02_MEAN
    Tabla_periodo$PSFC[match(prueba$Date,Tabla_periodo$Date)] <- prueba$PSFC
    Tabla_periodo$WS_MAX[match(prueba$Date,Tabla_periodo$Date)] <- prueba$WS_MAX
    Tabla_periodo$prep_hourly[match(prueba$Date,Tabla_periodo$Date)] <- prueba$prep_hourly
    lista_retorno[[i]]<- Tabla_periodo
  }
  names(lista_retorno)<- c("D1", "D2")
  Lista_d1_d2_loc2[[j]]<- lista_retorno 
  
  
}
names(Lista_d1_d2_loc2)<- names(Lista_d1_d2_loc)

#Creamos lista con las variables afinadas
Lista_d1_d2_loc3<- lapply(Lista_d1_d2_loc2, function(x){
  x[[1]]$RAINC<- NULL
  x[[1]]$RAINNC<- NULL
  x[[1]]$RAINSH<- NULL
  x[[2]]$RAINC<- NULL
  x[[2]]$RAINNC<- NULL
  x[[2]]$RAINSH<- NULL
  
  return(x)})

names(Lista_d1_d2_loc3)<- names(Lista_d1_d2_loc2)

#Guardamos
path_hist_WRF<- here::here('Data/Parques/Belesar/Historico/Historico_WRF_Belesar_Variables_D1D2.RDS')
saveRDS(Lista_d1_d2_loc3,path_hist_WRF)

# Actualizar WRF-WEB ------------------------------------------------------

RDS_down_path<- list.files(here::here('Data/Parques/Belesar/Historico/WEB'), 
                           full.names = T) %>%
  .[str_detect(.,"HIST_WEB_")] %>% .[str_detect(., ":")]


download_day<- sapply(str_split(RDS_down_path, "_"), 
                      function(x) paste0(x[3], " ",
                                         str_remove(x[4], ".RDS")))%>%  ymd_hms(.)



Belesar_DHI<- readRDS(RDS_down_path[which.max(download_day)])
Belesar_WRF<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_WRF_Belesar_Variables_D1D2.RDS'))

df2<- Belesar_DHI

Belesar_Merge<- list()
for (j in 1:length(Belesar_WRF)) {
  lista_retorno<- list()
  for(i in 1:2){
    df1<-  Belesar_WRF[[j]][[i]]
    Merge_table<- left_join(df1, df2, by=c("Date"))
    lista_retorno[[i]]<- Merge_table
  }
  names(lista_retorno)<- c("D1", "D2")
  Belesar_Merge[[j]]<- lista_retorno 
}
names(Belesar_Merge)<- names(Belesar_WRF)






#Belesar merge completecases
Belesar_Merge_cc<- list()
for (j in 1:length(Belesar_Merge)) {
  lista_retorno<- list()
  for(i in 1:2){
    df1<-  Belesar_Merge[[j]][[i]]
    df1$Temp<-NULL
    df1$T02_MEAN<-NULL
    df1$PSFC<-NULL
    df1$WS_MAX<-NULL
    Table_fine<- df1[order(df1$Date),]
    lista_retorno[[i]]<- Table_fine
  }
  names(lista_retorno)<- c("D1", "D2")
  Belesar_Merge_cc[[j]]<- lista_retorno 
}
names(Belesar_Merge_cc)<- names(Belesar_Merge)


#Guardamos
name<- paste0("WRF_WEB_",str_replace(as.character(ymd_hms(now())), " ", "_"),".RDS" )

path_hist_WRF<-paste0( here::here('Data/Parques/Belesar/Historico/WEB/'), name)
saveRDS(Belesar_Merge_cc,path_hist_WRF)




# Investigando vol --------------------------------------------------------
prueba<- readRDS(path_hist_WRF)
prueba1<- prueba[23]
prueba2<- prueba1[prueba1$Date>ymd("2018/11/01"),]
prueba3<- prueba2[!is.na(prueba$Vol),]


rr<- SMA(diff(prueba1$nivel),24)
rr2<-SMA(diff(prueba1$Vol)/4,24)
plot(rr, type = "l", ylim = c(-0.1,0.15))
lines(rr2, col="red")


#1 hm³ son  10⁶ m³ 


prueba2<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar_Todas_Variables.RDS'))

summary(prueba2$`Turbinado_BCE[m³/s]`+prueba2$`Turbinado[m³/s]`)




