library(here)
source(here::here('libraries.R'))

Last_WEB<-list.files(here::here('Data/Parques/Belesar/Historico/WEB/'), full.names = T) %>% .[which.max(.[str_detect(.,"E001")] %>% 
                                                                                                str_remove_all("BelesarE001_|.RDS") %>% .[str_detect(.,":")] %>% str_split("_") %>% 
                                                                                                sapply(function(x){
                                                                                                  as.character(ymd_hms(paste(x,collapse = " ")))
                                                                                                }) %>% ymd_hms )] %>% 
  readRDS()



Last_predict<- list.files(here::here('Data/Parques/Belesar/CSV/'), full.names = T) %>% .[str_detect(.,"comprobacion")] %>%
  .[4] %>% read.table(header = T, sep = ";")


Last_predict$Date<- ymd_hms(as.character(Last_predict$Date))



Tabla_comprobacion<- left_join(Last_WEB, Last_predict, by="Date")
Tabla_comprobacion$`Lluvia[mm]`<- as.numeric(str_replace(Tabla_comprobacion$`Lluvia[mm]`, ",", "."))
Tabla_comprobacion$`Nivel[msnm]`<- as.numeric(str_replace(Tabla_comprobacion$`Nivel[msnm]`, ",", "."))
Tabla_comprobacion$`Temp[ºC]`<-NULL
Tabla_comprobacion$`Vol[Hm³]`<-NULL
Tabla_comprobacion$`Vol[%]`<-NULL
Tabla_comprobacion<- Tabla_comprobacion[order(Tabla_comprobacion$Date),]
Tabla_comprobacion$diff_real<- c(NA,diff(Tabla_comprobacion$`Nivel[msnm]`))
Tabla_comprobacion$diff_real_SMA<- SMA(Tabla_comprobacion$diff_real, 12)
Tabla_comprobacion<- Tabla_comprobacion[complete.cases(Tabla_comprobacion),]


#plot(Tabla_comprobacion$diff_real, type = "l", x=Tabla_comprobacion$Date,ylim = c(-0.05,0.09))
lines(Tabla_comprobacion$diff_real_SMA, 
      x=Tabla_comprobacion$Date,col="blue")
lines(Tabla_comprobacion$Predicted_Diff_nivel.msnm., 
      x=Tabla_comprobacion$Date,col="red")
lines(Tabla_comprobacion$Predicted_Diff_nivel.msnm., 
      x=Tabla_comprobacion$Date,col="green")
lines(Tabla_comprobacion$Predicted_Diff_nivel.msnm., 
      x=Tabla_comprobacion$Date,col="orange")
