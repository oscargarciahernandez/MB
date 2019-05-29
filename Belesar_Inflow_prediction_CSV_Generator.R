library(here)
source(here::here('libraries.R'))


####ACTUALIZAMOS INFOR BELESAR..... ESTE SCRIPT HAY QUE ACTUALIZAR
source('~/MB/Actualizar_info_BelesarV2.R')


#CARGAMOS DATOS DE LA PÁGINA WEB Y DE WRF
Obs_Data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
  .[str_detect(.,"Obs_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("Obs_|.RDS") %>% 
                                             str_replace("--"," ") %>% ymd_hms())] %>% readRDS()

WRF_data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
  .[str_detect(.,"WRF_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("WRF_|.RDS") %>% 
                                             str_replace("--"," ") %>% ymd_hms())] %>% readRDS()

###########CARGAMOS MODELOS
modelo_DN_AP<- readRDS(here::here('Data/Parques/Belesar/Modelos/Actual_model/modelo_DNAP.RDS'))
modelo_WRF_DN1<- readRDS(here::here('Data/Parques/Belesar/Modelos/Actual_model/modelo_WRF1.RDS'))
modelo_WRF_DN2<- readRDS(here::here('Data/Parques/Belesar/Modelos/Actual_model/modelo_WRF2.RDS'))

####CARGAMOS VARIABLES DE LOS MODELOS
Variables<- read.table(here::here('Data/Parques/Belesar/Modelos/Actual_model/Variables.csv'),
                       stringsAsFactors = F) 

for (i in 1:length(Variables[,1])) {assign(Variables[i,1], Variables[i,2])}

#######CONSTRUIMOS PREDICCIÓN
Tabla_predecir<- Obs_Data %>% mutate(diff_nivel=c(NA,diff(nivel))) 
Tabla_predecir<- left_join(WRF_data[[23]]$D1[,c("Date", "prep_hourly")],Tabla_predecir, by="Date")
Tabla_predecir[,c("Vol","Temp", "porcentaje", "aport","nivel", "lluvia")]<- NULL

#PREDICCIÓN EMPLEANDO EL MODELO DE DIFF -1... QUE SOLO VALE PARA 1 DIA VISTA
T_p1<- Tabla_predecir[complete.cases(Tabla_predecir), ]
T_p1$difnivel_SMA<- SMA(T_p1$diff_nivel, SMA_DIFF_NIVEL)
T_p1$WRF_SMA<- SMA(T_p1$prep_hourly, SMA_LLUVIA_WRF)
T_p1$WRF_SMA_lag<- lag(T_p1$WRF_SMA, LAG_LLUVIA_WRF)
T_p1$DN_SMA_lag<- lag(T_p1$difnivel_SMA, LAG_DIFF_NIVEL)


fecha_cut<- now()-as.difftime(2, units = "days")

T_p1<-T_p1[T_p1$Date>fecha_cut, ] 
T_p1$predicted<- predict(modelo_WRF_DN1, newdata = T_p1)
T_p1$AP_suposed<- predict(modelo_DN_AP, 
                          newdata= data.frame(difnivel_SMA=T_p1$difnivel_SMA))
T_p1$AP_prediction<- predict(modelo_DN_AP, 
                             newdata= data.frame(difnivel_SMA=T_p1$predicted))



##PREDICCION MONTADA ÚNICAMENTE CON LLUVIA WRF PARA MÁS DE UN DÍA VISTA                             
T_p2<- Tabla_predecir[,c("Date", "prep_hourly")]
T_p2<- T_p2[complete.cases(T_p2),]
T_p2$WRF_SMA<- SMA(T_p2$prep_hourly, SMA_LLUVIA_WRF)
T_p2$WRF_SMA_lag<- lag(T_p2$WRF_SMA, LAG_LLUVIA_WRF)
T_p2<-T_p2[range(T_p1$Date)[2]<T_p2$Date, ] 
T_p2$predicted<- predict(modelo_WRF_DN2, newdata = T_p2)
T_p2$AP_prediction<- predict(modelo_DN_AP, 
                             newdata= data.frame(difnivel_SMA=T_p2$predicted))


#####CORREGIMOS LA UNIÓN DE AMBAS PREDICCIONES
alfa_nivel<- T_p1$predicted[length(T_p1$predicted)]-T_p2$predicted[1]
alfa_aport<- T_p1$AP_prediction[length(T_p1$AP_prediction)]-T_p2$AP_prediction[1]

T_p2$predicted<- T_p2$predicted+alfa_nivel
T_p2$AP_prediction<- T_p2$AP_prediction+alfa_aport


######MONTAMOS TABLA APORTACION 
Prediccion_aport<- as.data.frame(rbind(T_p1[ ,c("Date","AP_prediction")], 
                                       T_p2[ ,c("Date","AP_prediction")]))

Prediccion_aport<- as.data.frame(left_join(Prediccion_aport,
                                           Tabla_predecir[,c( "Date","prep_hourly")],
                                           by="Date"))

colnames(Prediccion_aport)<- c("Date", "Predicted_Inflow(m³/s)","Predicted_rainfall(mm/hour)")
Prediccion_aport$`Predicted_Inflow(m³/s)`<- round(Prediccion_aport$`Predicted_Inflow(m³/s)`,
                                                  digits = 4)
Prediccion_aport$`Predicted_rainfall(mm/hour)`<- round(as.numeric(as.character(Prediccion_aport$`Predicted_rainfall(mm/hour)`)),
                                                  digits = 4)







##########MONTAMOS TABLA DIFERENCIA DE NIVEL
Prediccion_diff_nivel<- as.data.frame(rbind(T_p1[,c("Date","predicted")],
                                            T_p2[,c("Date","predicted")]))

Prediccion_diff_nivel<- left_join(Prediccion_diff_nivel, 
                                  Tabla_predecir[,c( "Date",
                                                     "prep_hourly")],
                                  by="Date")



colnames(Prediccion_diff_nivel)<- c("Date", "Predicted_Diff_nivel(msnm)","Predicted_rainfall(mm/hour)")
Prediccion_diff_nivel$`Predicted_Diff_nivel(msnm)`<- round(Prediccion_diff_nivel$`Predicted_Diff_nivel(msnm)`,
                                                           digits = 4)
Prediccion_diff_nivel$`Predicted_rainfall(mm/hour)`<- round(Prediccion_diff_nivel$`Predicted_rainfall(mm/hour)`,
                                                           digits = 4)

fecha_hoy<- now() %>% as.character() %>% str_split(" ") %>% .[[1]] %>% .[1] %>% ymd()

Prediccion_aport<- Prediccion_aport[Prediccion_aport$Date>=fecha_hoy, ]
Prediccion_diff_nivel<- Prediccion_diff_nivel[Prediccion_diff_nivel$Date>=fecha_hoy, ]


CSV_aportacion<-Prediccion_aport[, c("Date",  "Predicted_Inflow(m³/s)"  )] 
CSV_lluvia<-Prediccion_aport[, c("Date","Predicted_rainfall(mm/hour)")] 
CSV_comprobacion<- Prediccion_diff_nivel

Path_Belesar<- here::here('Data/Parques/Belesar/CSV/')
nombre_csv_stream<- paste0("Belesar_", 
                           str_replace_all(as.character(fecha_hoy),"-",""),
                           "_stream.CSV")
nombre_csv_rain<- paste0("Belesar_", 
                           str_replace_all(as.character(fecha_hoy),"-",""),
                           "_rainfall.CSV")
nombre_csv_comprobacion<- paste0("Belesar_", 
                                 str_replace_all(as.character(fecha_hoy),"-",""),
                                 "_comprobacion.CSV")
write.table(CSV_aportacion, 
            file = paste0(Path_Belesar, nombre_csv_stream), 
            row.names = F,
            sep = ";")
write.table(CSV_lluvia, 
            file = paste0(Path_Belesar, nombre_csv_rain), 
            row.names = F,
            sep = ";")
write.table(CSV_comprobacion, 
            file = paste0(Path_Belesar, nombre_csv_comprobacion), 
            row.names = F,
            sep=";")






# Plot --------------------------------------------------------------------
########LLUVIA APORTACION 

k<- max(Prediccion_aport$`Predicted_Inflow(m³/s)`)/max(Prediccion_aport$`Predicted_rainfall(mm/hour)`)
ggplot(data=Prediccion_aport, aes(x=Date))+
  geom_bar(aes(y=`Predicted_rainfall(mm/hour)`), 
           stat="identity", 
           fill="blue", 
           alpha=0.5, 
           size=0.05,
           col="cyan")+
  xlab("Date")+
  ylab("Lluvia diaria [mm/h]")+
  geom_line(aes(y = `Predicted_Inflow(m³/s)`/k), 
            group = 1,
            col="forestgreen", 
            alpha=0.8) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k),
                                         name = "Aportacion [m³/s]", 
                                         breaks = seq(min(Prediccion_aport$`Predicted_Inflow(m³/s)`),
                                                      max(Prediccion_aport$`Predicted_Inflow(m³/s)`),
                                                      by=15)),
                     breaks = seq(min(Prediccion_aport$`Predicted_rainfall(mm/hour)`),
                                  max(Prediccion_aport$`Predicted_rainfall(mm/hour)`),
                                  by=1))+
  labs(title = "Predicción de lluvia y aportacion") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


path_graphs_Belesar<- here::here('graph/Belesar/Prediccion/')
if(!dir.exists(path_graphs_Belesar)){dir.create(path_graphs_Belesar)}
nombre_graph<- now() %>% as.character() %>% str_replace(" ","_")

ggsave(paste0(path_graphs_Belesar,nombre_graph,"_Aportacion.png"), 
       dpi = 200,
       device = "png")

######LLUVIA VARIACION DE NIVEL 
k<- max(Prediccion_diff_nivel$`Predicted_Diff_nivel(msnm)`)/max(Prediccion_diff_nivel$`Predicted_rainfall(mm/hour)`)
ggplot(data=Prediccion_diff_nivel, aes(x=Date))+
  geom_bar(aes(y=`Predicted_rainfall(mm/hour)`), 
           stat="identity", 
           fill="blue", 
           alpha=0.5, 
           size=0.05,
           col="cyan")+
  xlab("Date")+
  ylab("Lluvia diaria [mm/h]")+
  geom_line(aes(y = `Predicted_Diff_nivel(msnm)`/k), 
            group = 1, col="darkred", alpha=0.9) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k),
                                         name = "Variación de nivel [m]", 
                                         breaks = seq(min(Prediccion_diff_nivel$`Predicted_Diff_nivel(msnm)`),
                                                      max(Prediccion_diff_nivel$`Predicted_Diff_nivel(msnm)`),
                                                      by=0.01)),
                     breaks = seq(min(Prediccion_diff_nivel$`Predicted_rainfall(mm/hour)`),
                                  max(Prediccion_diff_nivel$`Predicted_rainfall(mm/hour)`),
                                  by=1))+
  labs(title = "Predicción de lluvia y variación de nivel ") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


ggsave(paste0(path_graphs_Belesar,nombre_graph,"_diffnivel.png"), 
       dpi = 200,
       device = "png")

