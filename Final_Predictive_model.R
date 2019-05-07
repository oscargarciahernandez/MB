library(caret)
library(doMC)
library(here)
source(here::here('libraries.R'))

#Cargamos los ultimos datos proporcionados por Actualizar_info_Belesar.R

Obs_Data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
  .[str_detect(.,"Obs_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("Obs_|.RDS") %>% 
                                             str_replace("--"," ") %>% ymd_hms())] %>% readRDS()

WRF_data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
    .[str_detect(.,"WRF_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("WRF_|.RDS") %>% 
                                               str_replace("--"," ") %>% ymd_hms())] %>% readRDS()





# Tratamos Datos ----------------------------------------------------------
###SOlamente emplearemos los datos de diferencia de nivel ofrecidos por la página web...
### Es decir, a partir de febrero

Tabla_1<- Obs_Data %>% mutate(diff_nivel=c(NA,diff(nivel))) 
Tabla_1<- left_join(Tabla_1, WRF_data[[23]]$D1[,c("Date", "prep_hourly")], by="Date")



# -------------------------------------------------------------------------
# Parte1: Predecir aportacion a partir de diferencia de nivel... 
Tabla_2<- Tabla_1
Tabla_2[,c("Vol","Temp", "porcentaje","prep_hourly")]<- NULL
Tabla_2<- Tabla_2[complete.cases(Tabla_2),]
Tabla_2$aport_SMA<- SMA(Tabla_2$aport, 12)
Tabla_2$difnivel_SMA<- SMA(Tabla_2$diff_nivel, 12)
Tabla_2<- Tabla_2[complete.cases(Tabla_2),]



train_data<- Tabla_2[Tabla_2$Date< ymd("2019/01/25"), ]
prediccion_data<- Tabla_2[Tabla_2$Date> ymd("2019/01/25"), ]



modelo_DN_AP<- train(aport_SMA ~ difnivel_SMA,
               data=train_data,
               method="svmLinear",
               tuneLength=50)


ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$aport, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$aport_SMA, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Aportacion [m³/s]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=predict(modelo, newdata= prediccion_data),
                x=Date), 
            col="red", lty=2)+
  theme_light()   


# -------------------------------------------------------------------------
# Parte2: Predecir differencia de nivel a través de lluvia WRF 

Tabla_3<- Tabla_1
Tabla_3[,c("Vol","Temp", "porcentaje")]<- NULL
Tabla_3<- Tabla_3[complete.cases(Tabla_3),]
Tabla_3$aport_SMA<- SMA(Tabla_3$aport, 12)
Tabla_3$difnivel_SMA<- SMA(Tabla_3$diff_nivel, 24)
Tabla_3$WRF_SMA<- SMA(Tabla_3$prep_hourly, 48)
Tabla_3$WRF_SMA_lag<- lag(Tabla_3$WRF_SMA, 24)
Tabla_3$DN_SMA_lag<- lag(Tabla_3$difnivel_SMA, 24)



Tabla_3<- Tabla_3[complete.cases(Tabla_3),]

#Por encima de octubre
Tabla_3<- Tabla_3[Tabla_3$Date>ymd("2018/10/25"), ]



train_data<- Tabla_3[Tabla_3$Date< ymd("2019/01/25"), ]
prediccion_data<- Tabla_3[Tabla_3$Date> ymd("2019/01/25"), ]



modelo_WRF_DN<- train(difnivel_SMA ~ WRF_SMA_lag,
               data=train_data,
               method="svmLinear",
               tuneLength=50)


ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$diff_nivel, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$difnivel_SMA, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Variacion nivel [msnm]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=predict(modelo, newdata= prediccion_data),
                x=Date), 
            col="red", lty=2)+
  theme_light()




prediccion_0<-predict(modelo, newdata= prediccion_data) 
alfa<- 0-prediccion_0[1]
alfa2<- 0-prediccion_data$difnivel_SMA[1]



ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$difnivel_SMA+alfa2, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Variacion nivel [msnm]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=predict(modelo, newdata= prediccion_data)+alfa,
                x=Date), 
            col="red", lty=2)+
  theme_light()




# Parte 3: De variacion de nivel a aportacion  -------------------------------------
Tabla_4<- Tabla_1
Tabla_4[,c("Vol","Temp", "porcentaje")]<- NULL
Tabla_4<- Tabla_4[complete.cases(Tabla_4),]
Tabla_4$aport_SMA<- SMA(Tabla_4$aport, 12)
Tabla_4$difnivel_SMA<- SMA(Tabla_4$diff_nivel, 24)
Tabla_4$WRF_SMA<- SMA(Tabla_4$prep_hourly, 48)
Tabla_4$WRF_SMA_lag<- lag(Tabla_4$WRF_SMA, 24)
Tabla_4$DN_SMA_lag<- lag(Tabla_4$difnivel_SMA, 24)


train_data<- Tabla_4[Tabla_4$Date< ymd("2019/01/25"), ]
prediccion_data<- Tabla_4[Tabla_4$Date> ymd("2019/01/25"), ]




AP_prediction<- predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN, 
                                                               newdata = prediccion_data)))

ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$aport, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$aport_SMA, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Aportacion [m³/s]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=AP_prediction,
                x=Date), 
            col="red", lty=2)+
  theme_light()   
