library(here)
source(here::here('libraries.R'))

Last_WEB<-list.files(here::here('Data/Parques/Belesar/Historico/WEB/'), full.names = T) %>% .[which.max(.[str_detect(.,"E001")] %>% 
                                                                                                str_remove_all("BelesarE001_|.RDS") %>% .[str_detect(.,":")] %>% str_split("_") %>% 
                                                                                                sapply(function(x){
                                                                                                  as.character(ymd_hms(paste(x,collapse = " ")))
                                                                                                }) %>% ymd_hms )] %>% 
  readRDS()

##ESTO ES PARA GENERAR TODAS LAS COMPROBACIONES.... 
HACER_TODAS_COMPROBACIONES<- FALSE
if(HACER_TODAS_COMPROBACIONES){
  
  for (i in 1:length(list.files(here::here('Data/Parques/Belesar/CSV/'), full.names = T) %>% 
                     .[str_detect(.,"comprobacion")])) {
    Last_predict<- list.files(here::here('Data/Parques/Belesar/CSV/'), full.names = T) %>% 
      .[str_detect(.,"comprobacion")] %>%
      .[i] %>% read.table(header = T, sep = ";")
    
    
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
    
    path_comprobacion<- here::here('graph/Belesar/Comprobacion/')
    nombre_comprobacion<- paste(range(Tabla_comprobacion$Date), collapse = "__") %>% 
      str_replace_all(" ","_")
    
    if(!dir.exists(path_comprobacion)){dir.create(path_comprobacion)}
    
    
    ggplot(Tabla_comprobacion)+
      geom_line(aes(x=Date, y=diff_real_SMA), size=1.05, alpha=0.7)+
      geom_line(aes(x=Date, y=Predicted_Diff_nivel.msnm.), col="red", size=1.5, alpha=0.75)+
      geom_line(aes(x=Date, y=0), col="green", size=0.8, alpha=0.75, linetype=2)+
      theme_light()+
      ylab("Variacion de nivel")+labs(title = "Prediccion(Rojo) Vs Realidad(Negro)")
    
    
    ggsave(filename =  paste0(path_comprobacion, "Comprobacion_",nombre_comprobacion,".png"),
           device = "png")
    
    
    
  }
  
}



#HACEMOS LAS COMPROBACIONES DE HACE 4 DIAS UNA VEZ SE GENERE EL MODELO 
Last_predict<- list.files(here::here('Data/Parques/Belesar/CSV/'), full.names = T) %>% 
  .[str_detect(.,"comprobacion")] %>%
  .[length(.)-4] %>% read.table(header = T, sep = ";")


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

path_comprobacion<- here::here('graph/Belesar/Comprobacion/')
nombre_comprobacion<- paste(range(Tabla_comprobacion$Date), collapse = "__") %>% 
  str_replace_all(" ","_")

if(!dir.exists(path_comprobacion)){dir.create(path_comprobacion)}


ggplot(Tabla_comprobacion)+
  geom_line(aes(x=Date, y=diff_real_SMA), size=1.05, alpha=0.7)+
  geom_line(aes(x=Date, y=Predicted_Diff_nivel.msnm.), col="red", size=1.5, alpha=0.75)+
  geom_line(aes(x=Date, y=0), col="green", size=0.8, alpha=0.75, linetype=2)+
  theme_light()+
  ylab("Variacion de nivel")+labs(title = "Prediccion(Rojo) Vs Realidad(Negro)")


ggsave(filename =  paste0(path_comprobacion, "Comprobacion_",nombre_comprobacion,".png"),
       device = "png")
