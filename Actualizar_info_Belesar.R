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
  periodo<- sapply(str_split(as.character(range(Belesar_total$Date)), " "), function(x) x[1])
  nombre<- paste0(periodo[1], "_",periodo[2])
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
  .[str_detect(.,"E001")]


download_day<- str_remove(sapply(str_split(RDS_down_path, "_"), function(x) x[3])
                          , ".RDS") %>%  ymd(.)

Last_Day_historico<- range(hist$Date)[2]


index_min<- which.max(as.Date(download_day))



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