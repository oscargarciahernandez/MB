####################  LEEME ##############
#LOS PARÁMETROS QUE PRIMERO DEBEMOS AJUSTAR SON LOS VALORES DE LAS 
# MOVING AVERAGES Y EL DESFASE. 
# TODOS LOS VALORES LOS PONDREMOS EN HORAS. 
# TODAS LAS GRÁFICAS Y MODELOS SE ESTÁN GUARDANDO AUTOMÁTICAMENTE EN 
# ~/MB/DATA/PARQUES/BELESAR/MODELOS/DN_AP --> MODELOS PARA PREDECIR APORTACION CON DIFERENCIA DE NIVEL
# ~/MB/DATA/PARQUES/BELESAR/MODELOS/WRF_DN --> MODELOS PARA PREDECIR DIFERENCIA DE NIVEL

# LA PREDICCIÓN DE APORTACION FINAL SERÁ UNA COMBINACIÓN DE AMBOS MODELOS, ES DECIR
# CON LLUVIA WRF PREDECIMOS DIFERENCIA NIVEL Y CON LA DIFERENCIA NIVEL CALCULAMOS APORTACION
# ESTAS GRÁFICAS COMPARATIVAS SE GUARDARÁN EN 
# ~/MB/DATA/PARQUES/BELESAR/MODELOS/WRF_AP --> GRÁFICAS FINALES DE PREDICCIÓN DE APORTACIÓN


####################  PARÁMETROS PARA MODIFICAR LAS VARIABLES ##############


####### MOVING AVERAGES
#ESTOS PARÁMETROS SIRVEN PARA "ELIMINAR RUIDO"... UN MOVING AVERAGE MAYOR IMPLICA 
# SEÑALES MAS "SUAVES" CON MENOS RUIDO, MÁS FÁCIL DE PREDECIR, PERO PROBABLEMENTE MÁS 
# ALEJADA DE LA REALIDAD. 

#### MOVING AVERAGE SOBRE APORTACION
SMA_APORTACION<- 12

#### MOVING AVERAGE SOBRE DIFERENCIA DE NIVEL 
SMA_DIFF_NIVEL<- 36

#### MOVING AVERAGE SOBRE LLUVIA WRF
SMA_LLUVIA_WRF<- 48


####### LAG SOBRE LAS VARIABLES 
#ESTO ES: CON CUANTO RETRASO VOY A METER LA VARIABLE AL MODELO PREDICTIVO
# EN PRINCIPIO LAS VARIABLES CON LAS VAMOS A JUGAR SON LAS SIGUIENTES...
# LLUVIA PREDICHA POR WRF -X -->X SON HORAS
# DIFERENCIA DEL NIVEL  -X -->X SON HORAS


#### DESFASE LLUVIA WRF (LAG)
LAG_LLUVIA_WRF<- 24

### DESFASE   DIFERENCIA DE NIVEL 
LAG_DIFF_NIVEL<- 24 

####################  PARÁMETROS PARA MODIFICAR LOS MODELOS ##############
# BÁSICAMENTE AQUÍ ELEGIREMOS EL MODELO QUE QUEREMOS EMPLEAR Y CON CUANTO "ESFUERZO"
# INTENTARÁ OBTENER EL MEJOR RESULTADO...

# CADA MODELO TIENE SUS PROPIOS PARÁMETROS (ALGUNOS NO) QUE SE PUEDEN PONER MANUALMENTE.
# SI NO PONEMOS PARÁMETROS EL MODELO SE EJECUTA CON UN PARÁMETRO POR DEFECTO, PERO, 
#PODEMOS DECIRLE QUE PRUEBE DIFERENTES PARÁMETROS... CON "TUNELENGTH"
# SI PONEMOS TUNELENGTH 20, PROBARÁ CON 20 PARÁMETROS DIFERENTES Y DEVOLVERÁ EL MEJOR VALOR


# EN CUANTO A LOS MODELOS. CARET TIENEN 238 ALGORITMOS DE PREDICCIÓN. PERO, HAY ALGUNOS ALGORITMOS
# QUE SON EXCLUSIVOS PARA CLASIFICACIÓN---- NUESTRO MODELO ES UN MODELO REGRESIVO O CONTINUO,
# POR LO TANTO TODOS ESTOS MODELOS NO NOS VALEN. A CONTINUACIÓN PONGO LOS MODELOS QUE A NOSOTROS 
# NOS INTERESAN; 

#REGRESION 
#  ANFIS , blasso , blassoAveraged , bridge , brnn , cubist , DENFIS , enet , FIR.DM , 
#   foba , FS.HGD , GFS.FR.MOGUL , GFS.LT.RS , GFS.THRIFT , glm.nb , HYFIS , icr , krlsPoly , 
#   krlsRadial , lars , lars2 , lasso , leapBackward , leapForward , leapSeq , lm , lmStepAIC ,
#     M5 , M5Rules , neuralnet , nnls , pcr , penalized , ppr , qrf , qrnn , relaxo , ridge ,
#    rlm , rqlasso , rqnc , rvmLinear , rvmPoly , rvmRadial , SBC , spikeslab , superpc , WM


#REGRESION-CLASIFICACION
# "avNNet , bag , bagEarth , bagEarthGCV , bam , bartMachine , bayesglm , blackboost , 
#   BstLm , bstSm , bstTree , cforest , ctree , ctree2 , dnn , earth , elm , evtree , 
#    extraTrees , gam , gamboost , gamLoess , gamSpline , gaussprLinear , gaussprPoly , 
#  gaussprRadial , gbm_h2o , gbm , gcvEarth , glm , glmboost , glmnet_h2o , glmnet , 
# glmStepAIC , kernelpls , kknn , knn , logicBag , logreg , mlp , mlpKerasDecay , 
# mlpKerasDropout , mlpML , mlpSGD , mlpWeightDecay , mlpWeightDecayML , monmlp , 
# msaenet , mxnet , mxnetAdam , nnet , nodeHarvest , null , parRF , partDSA , pcaNNet , 
#  pls , plsRglm , randomGLM , ranger , rbf , rbfDDA , Rborist , rf , rfRules , rpart , 
#  rpart1SE , rpart2 , RRF , RRFglobal , simpls , spls , svmBoundrangeString , svmExpoString , 
# svmLinear , svmLinear2 , svmLinear3 , svmPoly , svmRadial , svmRadialCost , svmRadialSigma , 
#   svmSpectrumString , treebag , widekernelpls , xgbDART , xgbLinear , xgbTree , xyf"

# PÁGINA WEB PARA TENER INFO DE CADA ALGORITMO
# https://topepo.github.io/caret/available-models.html



#IMPORTANTE: EL METODO SIEMPRE ENTRE COMILLAS
METODO<- "svmLinear"

#TUNELENGH: CUIDADO CON ESTO, AUMENTA MUCHO LA NECESIDAD DE COMPUTACIÓN. 
TUNELENGTH<- 1



####################  PARÁMETROS ORDENADOR ##############
#CARET PERMITE LA COMPUTACION EN PARALELO... PARA ELLO TENDREMOS QUE SABER EL NUMERO DE 
# CORES/NÚCLES DE NUESTRO ORDENADOR
# EN LINUX PONEMOS ~$ nproc POR TERMINAL Y NOS DEVUELVE EL NUMERO DE PROCESADORES
# ESTO ES POQUITÍN PELIGROSO PORQUE NO PODEMOS PONER A COMPUTAR LA MÁQUINA EMPLEANDO 
# TODOS SUS NUCLES... RECOMIENDAN PONER 1 MENOS... PERO POR SEGURIDAD SE PUEDAN PONER 
# 2 MENOS DE LOS QUE DISPONE EL ORDENADOR. 


NCORES<- 6

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################



library(caret)
library(doMC)
library(here)
source(here::here('libraries.R'))

registerDoMC(cores = NCORES)

source('~/MB/Actualizar_info_Belesar.R')
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
Tabla_2$aport_SMA<- SMA(Tabla_2$aport, SMA_APORTACION)
Tabla_2$difnivel_SMA<- SMA(Tabla_2$diff_nivel, SMA_DIFF_NIVEL)
Tabla_2<- Tabla_2[complete.cases(Tabla_2),]



train_data<- Tabla_2[Tabla_2$Date< ymd("2019/01/25"), ]
prediccion_data<- Tabla_2[Tabla_2$Date> ymd("2019/01/25"), ]



modelo_DN_AP<- train(aport_SMA ~ difnivel_SMA,
                     data=train_data,
                     method=METODO,
                     tuneLength=TUNELENGTH)


ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$aport, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$aport_SMA, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Aportacion [m³/s]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= prediccion_data),
                x=Date), 
            col="red", lty=2)+
  theme_light()   



# -------------------------------------------------------------------------
# Parte2: Predecir differencia de nivel a través de lluvia WRF 

Tabla_3<- Tabla_1
Tabla_3[,c("Vol","Temp", "porcentaje")]<- NULL
Tabla_3<- Tabla_3[complete.cases(Tabla_3),]
Tabla_3$aport_SMA<- SMA(Tabla_3$aport, SMA_APORTACION)
Tabla_3$difnivel_SMA<- SMA(Tabla_3$diff_nivel, SMA_DIFF_NIVEL)
Tabla_3$WRF_SMA<- SMA(Tabla_3$prep_hourly, SMA_LLUVIA_WRF)
Tabla_3$WRF_SMA_lag<- lag(Tabla_3$WRF_SMA, LAG_LLUVIA_WRF)
Tabla_3$DN_SMA_lag<- lag(Tabla_3$difnivel_SMA, LAG_DIFF_NIVEL)



Tabla_3<- Tabla_3[complete.cases(Tabla_3),]

#Por encima de octubre
Tabla_3<- Tabla_3[Tabla_3$Date>ymd("2018/10/25"), ]



train_data<- Tabla_3[Tabla_3$Date< ymd("2019/01/25"), ]
prediccion_data<- Tabla_3[Tabla_3$Date> ymd("2019/01/25"), ]



modelo_WRF_DN<- train(difnivel_SMA ~ WRF_SMA_lag,
               data=train_data,
               method=METODO,
               tuneLength=TUNELENGTH)

modelo_WRF_DN4<- train(difnivel_SMA ~ WRF_SMA_lag + DN_SMA_lag,
                       data=train_data,
                       method=METODO,
                       tuneLength=TUNELENGTH)



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
  geom_line(aes(y=predict(modelo_WRF_DN4, newdata= prediccion_data),
                x=Date), 
            col="blue", lty=2)+
  theme_light()





alfa<- 0-predict(modelo_WRF_DN, newdata= prediccion_data)[1]
alfa1<- 0-predict(modelo_WRF_DN4, newdata= prediccion_data)[1]
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
  geom_line(aes(y=predict(modelo_WRF_DN4, newdata= prediccion_data)+alfa1,
                x=Date), 
            col="blue", lty=2)+
  theme_light()




# Parte 3: De variacion de nivel a aportacion  -------------------------------------
Tabla_4<- Tabla_1
Tabla_4[,c("Vol","Temp", "porcentaje")]<- NULL
Tabla_4<- Tabla_4[complete.cases(Tabla_4),]
Tabla_4$aport_SMA<- SMA(Tabla_4$aport, SMA_APORTACION)
Tabla_4$difnivel_SMA<- SMA(Tabla_4$diff_nivel, SMA_DIFF_NIVEL)
Tabla_4$WRF_SMA<- SMA(Tabla_4$prep_hourly, SMA_LLUVIA_WRF)
Tabla_4$WRF_SMA_lag<- lag(Tabla_4$WRF_SMA, LAG_LLUVIA_WRF)
Tabla_4$DN_SMA_lag<- lag(Tabla_4$difnivel_SMA, LAG_DIFF_NIVEL)

prediccion_data<- Tabla_4[Tabla_4$Date> ymd("2019/01/25"), ]

AP_prediction<- predict(modelo_DN_AP, 
                        newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN, 
                                                               newdata = prediccion_data)))
AP_prediction1<- predict(modelo_DN_AP, 
                         newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN4, 
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
  geom_line(aes(y=AP_prediction1,
                x=Date), 
            col="blue", lty=2)+
  theme_light()   


# Predecir futuros valores ------------------------------------------------

Tabla_predecir<- Obs_Data %>% mutate(diff_nivel=c(NA,diff(nivel))) 
Tabla_predecir<- left_join(WRF_data[[23]]$D1[,c("Date", "prep_hourly")],Tabla_1, by="Date")
Tabla_predecir[,c("Vol","Temp", "porcentaje", "aport","nivel", "lluvia")]<- NULL

#Prediccion empleando el mejor model que contempla dif -1

T_p1<- Tabla_predecir[complete.cases(Tabla_predecir), ]
T_p1$difnivel_SMA<- SMA(T_p1$diff_nivel, SMA_DIFF_NIVEL)
T_p1$WRF_SMA<- SMA(T_p1$prep_hourly, SMA_LLUVIA_WRF)
T_p1$WRF_SMA_lag<- lag(T_p1$WRF_SMA, LAG_LLUVIA_WRF)
T_p1$DN_SMA_lag<- lag(T_p1$difnivel_SMA, LAG_DIFF_NIVEL)


fecha_cut<- now()-as.difftime(2, units = "days")

T_p1<-T_p1[T_p1$Date>fecha_cut, ] 
T_p1$predicted<- predict(modelo_WRF_DN4, newdata = T_p1)

ggplot(data = T_p1)+
  geom_line(aes(y=difnivel_SMA + (0-difnivel_SMA[1]), 
                x=Date), 
            alpha=0.8)+
  geom_line(aes(y=predicted + (0-predicted[1]), 
                x=Date), 
            alpha=0.8,
            col="red")+
ylab("Diferencia de nviel [m³/s]")+
  xlab(paste(range(T_p1$Date), collapse = "\n"))+
  theme_light() 



T_p1$AP_suposed<- predict(modelo_DN_AP, 
                         newdata= data.frame(difnivel_SMA=T_p1$difnivel_SMA))
T_p1$AP_prediction<- predict(modelo_DN_AP, 
                         newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN4, 
                                                                  newdata = T_p1)))
                                                                  

ggplot(data = T_p1)+
  geom_line(aes(y=AP_suposed, 
                x=Date), 
            alpha=0.8)+
  geom_line(aes(y=AP_prediction1, 
                x=Date), 
            alpha=0.8,
            col="red")+
  ylab("Diferencia de nviel [m³/s]")+
  xlab(paste(range(T_p1$Date), collapse = "\n"))+
  theme_light() 


T_p2<- Tabla_predecir[,c("Date", "prep_hourly")]
T_p2<- T_p2[complete.cases(T_p2),]
T_p2$WRF_SMA<- SMA(T_p2$prep_hourly, SMA_LLUVIA_WRF)
T_p2$WRF_SMA_lag<- lag(T_p2$WRF_SMA, LAG_LLUVIA_WRF)
T_p2<-T_p2[range(T_p1$Date)[2]<T_p2$Date, ] 
T_p2$predicted<- predict(modelo_WRF_DN, newdata = T_p2)
T_p2$AP_prediction<- predict(modelo_DN_AP, 
                              newdata= data.frame(difnivel_SMA=T_p2$predicted))



alfa_nivel<- T_p1$predicted[length(T_p1$predicted)]-T_p2$predicted[1]
alfa_aport<- T_p1$AP_prediction1[length(T_p1$AP_prediction1)]-T_p2$AP_prediction2[1]


Prediccion_diff_nivel<- rbind(T_p1[,c("Date","predicted")], T_p2[,c("Date","predicted")])
Prediccion_aport<- rbind(T_p1[,c("Date","AP_prediction")], T_p2[,c("Date","AP_prediction")])





##########CUANDO SEA NECESARIO CAMBIAR EL MODELO HABRÁ QUE GUARDARLO EN ACTUAL_MODEL
value<- c(SMA_APORTACION,SMA_DIFF_NIVEL,
      SMA_LLUVIA_WRF,LAG_DIFF_NIVEL,
      LAG_LLUVIA_WRF)
variable_name<- c("SMA_APORTACION","SMA_DIFF_NIVEL",
  "SMA_LLUVIA_WRF","LAG_DIFF_NIVEL",
  "LAG_LLUVIA_WRF")

#write.table(cbind(variable_name, value), file =here::here('Data/Parques/Belesar/Modelos/Actual_model/Variables.csv') )
#saveRDS(modelo_WRF_DN4,here::here('Data/Parques/Belesar/Modelos/Actual_model/modelo_WRF1.RDS'))
#saveRDS(modelo_WRF_DN,here::here('Data/Parques/Belesar/Modelos/Actual_model/modelo_WRF2.RDS'))
#saveRDS(modelo_DN_AP,here::here('Data/Parques/Belesar/Modelos/Actual_model/modelo_DNAP.RDS'))

