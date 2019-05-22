library(here)
source(here::here('libraries.R'))

#############################TRAINING PARAMETERS 
###########WRF---DN
modelos_WRF_DN<- here::here('Data/Parques/Belesar/Modelos/WRF_DN/') %>% list.files(full.names = T) %>% 
  .[str_detect(., ".RDS")]

Lista_results<- list()
for (i in 1:length(modelos_WRF_DN)) {
  Lista_results[[i]]<- tryCatch({readRDS(modelos_WRF_DN[i])}, error=function(e){0}) 
  
}

Lista_modelos_buenos<- Lista_results[sapply(Lista_results, function(x) length(class(x))==2)]

names(Lista_modelos_buenos)<- Lista_modelos_buenos %>% sapply(., function(x) x$method)

Tabla_results<- lapply(Lista_modelos_buenos, function(x) x$results[,((length(x$results)-5):length(x$results))]) %>% 
  bind_rows(.id ="Tipo_modelo")


Tabla_results %>% group_by(Tipo_modelo) %>% summarise(MRMSE=min(RMSE),MMAE=min(MAE)) %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_bar(aes(y=MRMSE, x=Tipo_modelo),
           stat = "identity",
           fill="red",
           alpha=0.5)+
  geom_bar(aes(y=MMAE, x=Tipo_modelo),
           stat = "identity",
           fill="blue",
           alpha=0.5)+
  ylab("RMSE---MAE")+
  labs(subtitle = "Modelo WRF a diferencia nivel")+
  theme_light()



###############################DN-----AP
modelos_WRF_DN<- here::here('Data/Parques/Belesar/Modelos/DN_AP/') %>% list.files(full.names = T) %>% 
  .[str_detect(., ".RDS")]

Lista_results<- list()
for (i in 1:length(modelos_WRF_DN)) {
  Lista_results[[i]]<- tryCatch({readRDS(modelos_WRF_DN[i])}, error=function(e){0}) 
  
}

Lista_modelos_buenos<- Lista_results[sapply(Lista_results, function(x) length(class(x))==2)]

names(Lista_modelos_buenos)<- Lista_modelos_buenos %>% sapply(., function(x) x$method)

Tabla_results<- lapply(Lista_modelos_buenos, function(x) x$results[,((length(x$results)-5):length(x$results))]) %>% 
  bind_rows(.id ="Tipo_modelo")


Tabla_results %>% group_by(Tipo_modelo) %>% summarise(MRMSE=min(RMSE),MMAE=min(MAE)) %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_bar(aes(y=MRMSE, x=Tipo_modelo),
           stat = "identity",
           fill="red",
           alpha=0.5)+
  geom_bar(aes(y=MMAE, x=Tipo_modelo),
           stat = "identity",
           fill="blue",
           alpha=0.5)+
  ylab("RMSE---MAE")+
  labs(subtitle = "Modelo WRF a diferencia nivel")+
  theme_light()



Tabla_results %>% group_by(Tipo_modelo) %>% summarise(MRMSE=min(RMSE),MMAE=min(MAE)) %>% as.data.frame() %>% .[which.min(.[,3]),] 




# #############################TEST PARAMETERS -----------------------------------------------------------
rmse <- function(error){sqrt(mean(error^2))}

# Function that returns Mean Absolute Error
mae <- function(error){mean(abs(error))}





if(!(exists("Tabla_1")&exists("Tabla_2")&exists("Tabla_3"))){
  
  Obs_Data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
    .[str_detect(.,"Obs_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("Obs_|.RDS") %>% 
                                               str_replace("--"," ") %>% ymd_hms())] %>% readRDS()
  
  WRF_data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
    .[str_detect(.,"WRF_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("WRF_|.RDS") %>% 
                                               str_replace("--"," ") %>% ymd_hms())] %>% readRDS()
  
  
  
  
  #CREAMOS TABLA 1... LA TABLA COMPLETA QUE INCLUYE LOS DATOS DE LA PÁGINA WEB E HISTORICO DHI
  #range(Tabla_1$Date)
  # "2018-01-02 00:00:00 UTC" "2019-05-13 07:00:00 UTC"[ACTUALIDAD]
  
  Tabla_1<- Obs_Data %>% mutate(diff_nivel=c(NA,diff(nivel))) 
  Tabla_1<- left_join(Tabla_1, WRF_data[[23]]$D1[,c("Date", "prep_hourly")], by="Date")
  
  
  
  # CREEAMOS TABLA_2: ESTO SOLAMENTE COMPRENDE EL HISTÓRICO DE DHI. YA QUE ES LA FUENTE DE INFORMACION
  # SOBRE APORTACION
  #range(Tabla_2$Date)
  #"2018-01-02 12:00:00 UTC" "2019-02-07 23:00:00 UTC"
  
  Tabla_2<- Tabla_1
  Tabla_2[,c("Vol","Temp", "porcentaje","prep_hourly")]<- NULL
  Tabla_2<- Tabla_2[complete.cases(Tabla_2),]
  Tabla_2$aport_SMA<- SMA(Tabla_2$aport, SMA_APORTACION)
  Tabla_2$difnivel_SMA<- SMA(Tabla_2$diff_nivel, SMA_DIFF_NIVEL)
  Tabla_2<- Tabla_2[complete.cases(Tabla_2),]
  
  #CREAMOS TABLA_3: ESTO SOLAMENTE COMPRENDE DESDE FINALES DE OCTUBRE HASTA LA ACTUALIDAD
  #range(Tabla_3$Date)
  #"2018-10-25 01:00:00 UTC"----"2019-05-13 07:00:00 UTC"[ACTUALIDAD]
  
  Tabla_3<- Tabla_1
  Tabla_3[,c("Vol","Temp", "porcentaje", "aport")]<- NULL
  Tabla_3<- Tabla_3[complete.cases(Tabla_3),]
  #Tabla_3$aport_SMA<- SMA(Tabla_3$aport, SMA_APORTACION)
  Tabla_3$difnivel_SMA<- SMA(Tabla_3$diff_nivel, SMA_DIFF_NIVEL)
  Tabla_3$lluvia_SMA<- SMA(Tabla_3$lluvia, SMA_LLUVIA_OBS)
  Tabla_3$WRF_SMA<- SMA(Tabla_3$prep_hourly, SMA_LLUVIA_WRF)
  Tabla_3$WRF_SMA_lag<- lag(Tabla_3$WRF_SMA, LAG_LLUVIA_WRF)
  Tabla_3$lluvia_SMA_lag<- lag(Tabla_3$lluvia_SMA, LAG_LLUVIA_OBS)
  Tabla_3$DN_SMA_lag<- lag(Tabla_3$difnivel_SMA, LAG_DIFF_NIVEL)
  Tabla_3<- Tabla_3[complete.cases(Tabla_3),]
  #Por encima de octubre
  Tabla_3<- Tabla_3[Tabla_3$Date>ymd("2018/10/25"), ]
  
  
}



################### WRF---DN 
PORCENTAJE_ENTRENAMIENTO<- 0.8
LOGIC_TRAIN<- ifelse(1:nrow(Tabla_3)%in%(1:round(nrow(Tabla_3)*PORCENTAJE_ENTRENAMIENTO)), TRUE,FALSE)
prediccion_data<- Tabla_3[!LOGIC_TRAIN, ]

OBSERVED_AP<- prediccion_data_DN_A$aport_SMA
OBSERVED_DIFNIVEL<- prediccion_data$difnivel_SMA



modelos_WRF_DN<- here::here('Data/Parques/Belesar/Modelos/WRF_DN/') %>% list.files(full.names = T) %>% 
  .[str_detect(., ".RDS")]

Lista_results<- list()
for (i in 1:length(modelos_WRF_DN)) {
  Lista_results[[i]]<- tryCatch({readRDS(modelos_WRF_DN[i])}, error=function(e){0}) 
  
}

Lista_modelos_buenos<- Lista_results[sapply(Lista_results, function(x) length(class(x))==2)]

names(Lista_modelos_buenos)<- Lista_modelos_buenos %>% sapply(., function(x) x$method)

Lista_parameters<-list()
for (i in 1:length(Lista_modelos_buenos)) {
  MODEL_WRF_DN_PREDICTION<-  predict(Lista_modelos_buenos[[i]], newdata= prediccion_data)
  error<- OBSERVED_DIFNIVEL- MODEL_WRF_DN_PREDICTION
  Lista_parameters[[i]] <- c(rmse(error), mae(error))
  }
Tabla_results<- Lista_parameters %>% unlist() %>% matrix(ncol=2, byrow = T) %>% as.data.frame() %>% 
  cbind(names(Lista_modelos_buenos))


Tabla_results<- lapply(Lista_modelos_buenos, function(x) x$results[,((length(x$results)-5):length(x$results))]) %>% 
  bind_rows(.id ="Tipo_modelo")


Tabla_results %>% group_by(Tipo_modelo) %>% summarise(MRMSE=min(RMSE),MMAE=min(MAE)) %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_bar(aes(y=MRMSE, x=Tipo_modelo),
           stat = "identity",
           fill="red",
           alpha=0.5)+
  geom_bar(aes(y=MMAE, x=Tipo_modelo),
           stat = "identity",
           fill="blue",
           alpha=0.5)+
  ylab("RMSE---MAE")+
  labs(subtitle = "Modelo WRF a diferencia nivel")+
  theme_light()



###############################DN-----AP
modelos_WRF_DN<- here::here('Data/Parques/Belesar/Modelos/DN_AP/') %>% list.files(full.names = T) %>% 
  .[str_detect(., ".RDS")]

Lista_results<- list()
for (i in 1:length(modelos_WRF_DN)) {
  Lista_results[[i]]<- tryCatch({readRDS(modelos_WRF_DN[i])}, error=function(e){0}) 
  
}

Lista_modelos_buenos<- Lista_results[sapply(Lista_results, function(x) length(class(x))==2)]

names(Lista_modelos_buenos)<- Lista_modelos_buenos %>% sapply(., function(x) x$method)


################PARA LA PREDICCION DE DN A AP
prediccion_data_DN_A<- Tabla_2[Tabla_2$Date> ymd("2019/01/25"), ]
MODEL_DN_AP_PREDICTION<- predict(modelo_DN_AP, newdata= prediccion_data_DN_A)
OBSERVED_AP<- prediccion_data_DN_A$aport_SMA










Tabla_results<- lapply(Lista_modelos_buenos, function(x) x$results[,((length(x$results)-5):length(x$results))]) %>% 
  bind_rows(.id ="Tipo_modelo")


Tabla_results %>% group_by(Tipo_modelo) %>% summarise(MRMSE=min(RMSE),MMAE=min(MAE)) %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_bar(aes(y=MRMSE, x=Tipo_modelo),
           stat = "identity",
           fill="red",
           alpha=0.5)+
  geom_bar(aes(y=MMAE, x=Tipo_modelo),
           stat = "identity",
           fill="blue",
           alpha=0.5)+
  ylab("RMSE---MAE")+
  labs(subtitle = "Modelo WRF a diferencia nivel")+
  theme_light()



Tabla_results %>% group_by(Tipo_modelo) %>% summarise(MRMSE=min(RMSE),MMAE=min(MAE)) %>% as.data.frame() %>% .[which.min(.[,3]),] 


