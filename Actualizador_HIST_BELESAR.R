library(here)
source(here::here('libraries.R'))

# Completar histórico Belesar ---------------------------------------------

# Download Belesar --------------------------------------------------------
Down_E001_Belesar<- function(){
  
x<- read.csv(here::here('Data/Parques/Belesar/Historico/marzlluviaBelesarmarz9.csv'))
x<- x[6:length(x$Estación),1:2 ]
colnames(x)<- c("Date", "lluvia")
x$Date<- dmy_hm(x$Date)
x$lluvia<- as.numeric(str_replace(as.character(x$lluvia), ",", "."))
x<- x[complete.cases(x),]

head(x)


x2<- read.csv(here::here('Data/Parques/Belesar/Historico/marzlluviaBelesar.csv'), sep = ";")
x2<- x2[6:length(x2$Estación),1:2 ]
colnames(x2)<- c("Date", "lluvia")
x2$Date<- dmy_hm(x2$Date)
x2$lluvia<- as.numeric(str_replace(as.character(x2$lluvia), ",", "."))
x2<- x2[complete.cases(x2),]

head(x2)


x22<- read.csv(here::here('Data/Parques/Belesar/Historico/marzlluviaBelesarmarz26.csv'), sep = ";")
x22<- x22[6:length(x22$Estación),1:2 ]
colnames(x22)<- c("Date", "lluvia")
x22$Date<- dmy_hm(x22$Date)
x22$lluvia<- as.numeric(str_replace(as.character(x22$lluvia), ",", "."))
x22<- x22[complete.cases(x22),]


x12<- x2[!x2$Date%in%x$Date,]
x6<- rbind(x,x12)
colnames(x6)<- c("Date", "lluvia")

x26<- x22[!x22$Date%in%x6$Date,]
x7<- rbind(x6,x26)

x7<- x7[order(x7$Date),]

##Nivel
x3<- read.csv(here::here('Data/Parques/Belesar/Historico/marznivelBelesar.csv'), sep = ";")
x3<- x3[6:length(x3$Estación),1:2 ]
colnames(x3)<- c("Date", "lluvia")
head(x3)
x3$Date<- dmy_hm(x3$Date)
x3$lluvia<- as.numeric(str_replace(as.character(x3$lluvia), ",", "."))
x3<- x3[complete.cases(x3),]


x4<- read.csv(here::here('Data/Parques/Belesar/Historico/Nivelfeb6-mar6.csv'), sep = ";")
x4<- x4[6:length(x4$Estación),1:2 ]
colnames(x4)<- c("Date", "lluvia")
head(x4)
x4$Date<- dmy_hm(x4$Date)
x4$lluvia<- as.numeric(str_replace(as.character(x4$lluvia), ",", "."))
x4<- x4[complete.cases(x4),]



x44<- read.csv(here::here('Data/Parques/Belesar/Historico/nivelBelesarmarz26.csv'), sep = ";")
x44<- x44[6:length(x44$Estación),1:2 ]
colnames(x44)<- c("Date", "lluvia")
head(x44)
x44$Date<- dmy_hm(x44$Date)
x44$lluvia<- as.numeric(str_replace(as.character(x44$lluvia), ",", "."))
x44<- x44[complete.cases(x44),]


length(x4$Date)
x34<- x3[!x3$Date%in%x4$Date,]
x5<- rbind(x4,x34)
colnames(x5)<- c("Date", "lluvia")

x45<- x44[!x44$Date%in%x5$Date,]
x8<- rbind(x5,x45)
colnames(x8)<- c("Date", "lluvia")


x8<- x8[order(x8$Date),]



Nivel_lluvia_marzo<- left_join(x7,x8, by = "Date")
Nivel_lluvia_marzo<-Nivel_lluvia_marzo[complete.cases(Nivel_lluvia_marzo),]
colnames(Nivel_lluvia_marzo)<- c("Date", "lluvia", "nivel")







write.table(Nivel_lluvia_marzo,here::here('Data/Parques/Belesar/Historico/HIST_lluvia_nivel_marzo.csv'), sep = ";")


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



# Construir febrero marzo -------------------------------------------------
marzo<- read.csv(file=here::here('Data/Parques/Belesar/Historico/HIST_lluvia_nivel_marzo.csv'),
           sep = ";",
           dec = ".",
         header = TRUE)
colnames(marzo)<- c("Date", "lluvia", "nivel")

marzo$Date<- ymd_hms(as.character(marzo$Date))


RDS_down_path<- list.files(here::here('Data/Parques/Belesar/Historico/'), 
                      full.names = T) %>%
  .[str_detect(.,"E001")]


pagina_web<- readRDS(RDS_down_path[1])
df1<- pagina_web[,1:3]
colnames(df1)<- c("Date", "lluvia", "nivel")
df1$lluvia<- as.numeric(str_replace(as.character(df1$lluvia), ",", "."))
df1$nivel<- as.numeric(str_replace(as.character(df1$nivel), ",", "."))




rr<- marzo[!marzo$Date%in%df1$Date,]
rr2<- rbind(df1,rr)
colnames(rr2)<- c("Date", "lluvia", "nivel")


rr2<- rr2[order(rr2$Date),]
saveRDS(rr2, here::here('Data/Parques/Belesar/Historico/WEB/HIST_WEB.RDS'))





# Actualizar HIST_WEB -----------------------------------------------------
#dir.create(here::here('Data/Parques/Belesar/Historico/WEB'))

Down_E001_Belesar()
Down_E001_Belesar()

hist1<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/'), full.names = T)
hist2<- hist1[str_detect(hist1, "HIST_WEB_")]
hist3<- str_remove(str_remove(hist2,  "HIST_WEB_"), ".RDS") %>% 
str_replace(.,"_"," ") %>% ymd_hms()
path_hist<- hist2[which.max(hist3)]


hist<- readRDS(path_hist)



RDS_down_path<- list.files(here::here('Data/Parques/Belesar/Historico/'), 
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




# Completar DHI y WEB -----------------------------------------------------

DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/DHI_historico_afinado'))
prueba<- DHI[,c("Date","Lluvia_mm",
                "aport_mean",
                "nivel_mean")]
colnames(prueba)<- c("Date", "lluvia","aport", "nivel")



hist1<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/'), full.names = T)
hist2<- hist1[str_detect(hist1, "HIST_WEB_")]
hist3<- str_remove(str_remove(hist2,  "HIST_WEB_"), ".RDS") %>% 
  str_replace(.,"_"," ") %>% ymd_hms()
path_hist<- hist2[which.max(hist3)]


hist<- readRDS(path_hist)





completando<- prueba[!prueba$Date%in%hist$Date,]
completado<- bind_rows(hist, completando)
completado<- completado[order(completado$Date),]


path_hist<- here::here('Data/Parques/Belesar/Historico/WEB/')
name<- paste0("HIST_WEB_",str_replace(as.character(ymd_hms(now())), " ", "_"), ".RDS")
path_total<- paste0(path_hist, name)
saveRDS(completado, path_total)

