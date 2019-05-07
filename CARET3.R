library(caret)
library(caret)
library(doMC)
library(here)
source(here::here('libraries.R'))

# OBTENEMOS INFORMACION DE MODELOS DISPONIBLES ----------------------------
x<- getModelInfo()
type<- lapply(x, function(y) {if(length(y$type > 1)){
  return(paste(y$type, collapse = "_"))
}else{return(y$type)}})

label<- lapply(x, function(y) {if(length(y$label > 1)){
  return(paste(y$label, collapse = "_"))
}else{return(y$label)}})



Tabla_method<- as.data.frame(cbind(names(x),unlist(type), unlist(label)) , row.names = F) 
colnames(Tabla_method)<- c("Method", "type", "Label")

Tabla_both<- Tabla_method[which(str_detect(Tabla_method$type, 
                                           "Regression")),] %>% 
  .[str_detect(.$type,"_"),]

Tabla_regresion<- Tabla_method[which(str_detect(Tabla_method$type, 
                                                "Regression")),] %>% 
  .[!str_detect(.$type,"_"),]


sum(str_detect(Tabla_method$Method, "[m5]"))
Tabla_method$Label[str_detect(Tabla_method$Label, "Neural")]

M5_alg<- Tabla_method[str_detect(Tabla_method$Method, "M5"),] %>% .[which(str_detect(.$type, 
                                                                                     "Regression")),] 

Neural_alg<- Tabla_method[str_detect(Tabla_method$Label, "Neural"),]%>% .[which(str_detect(.$type, 
                                                                                           "Regression")),] 



SVM_alg<- Tabla_method[str_detect(Tabla_method$Method, "svm"),] %>% .[which(str_detect(.$type, 
                                                                                       "Regression")),] 


# IMPORTAMOS DATOS --------------------------------------------------------


RDS_down_path<- list.files(here::here('Data/Parques/Belesar/Historico/WEB'),
                           full.names = T) %>%
  .[str_detect(.,"WRF_WEB_")] %>% .[str_detect(., ":")]


download_day<- sapply(str_split(RDS_down_path, "_"), 
                      function(x) paste0(x[3], " ",
                                         str_remove(x[4], ".RDS")))%>%  ymd_hms(.)


clean_data<- readRDS(RDS_down_path[which.max(download_day)])
clean_data1<- lapply(clean_data, function(x){
  y<- lapply(x, function(r){
    r$dif_nivel<- c(diff(r$nivel)[],0)
    r$dif_nivel[diff(r$Date)>3600]<- 0
    return(r)
  })
})



# CORTAMOS DATOS EN ENTRENAMIENTO Y PREDICCION  ---------------------------


fecha_cut<- ymd("2019/04/15")
#cortar en entrenamiento y predicción
cut_train<- lapply(clean_data1, function(x,fecha_end){
  y<- lapply(x, function(r){
    
    
    fecha_ini<- ymd("2018/12/01")
    
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
},fecha_end=fecha_cut)
cut_train<- lapply(cut_train, function(x){
  r<- x[[1]][,c("Date","LON", "LAT", "prep_hourly", "lluvia", 
                "nivel", "dif_nivel")]
  return(r[complete.cases(r),])
  
})


cut_predict<- lapply(clean_data1, function(x, fecha_ini){
  y<- lapply(x, function(r){
    Jan_data<- r[which(r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
}, fecha_ini=fecha_cut)
cut_predict<- lapply(cut_predict, function(x){
  r<- x[[1]][,c("Date","LON", "LAT", "prep_hourly", "lluvia",
                "nivel", "dif_nivel")]
  return(r[complete.cases(r), ])
  
})






# MODELOS DE PREDICCION  ---------------------------------------------------------------------
## SERIA INTERESANTE HECHAR UN VISTAZO AL TEMA DE DOMC
# PARALEL COMPUTATION Y DEMÁS
#registerDoMC(cores = 4)



## LO MISMO, PERO AÑADIENDO LLUVIA LAG Y NIVEL LAG 



i<- 48
j<- 48

entrenamiento<- cut_train[[23]]
entrenamiento$SMA_prep<- SMA(entrenamiento$prep_hourly,i)
entrenamiento$SMA_difnivel<- SMA(entrenamiento$dif_nivel, 36)
entrenamiento$SMA_difnivel_lag1<- lag(entrenamiento$SMA_difnivel,j)
entrenamiento$SMA_prep_lag1<- lag(entrenamiento$SMA_prep,j)
entrenamiento<- entrenamiento[complete.cases(entrenamiento),]


prediccion<- cut_predict[[23]]
prediccion$SMA_prep<- SMA(prediccion$prep_hourly,i)
prediccion$SMA_difnivel<- SMA(prediccion$dif_nivel, 36)
prediccion$SMA_difnivel_lag1<- lag(prediccion$SMA_difnivel,j)
prediccion$SMA_prep_lag1<- lag(prediccion$SMA_prep,j)
prediccion<- prediccion[complete.cases(prediccion),]




# PARÁMETROS TRAIN CARET --------------------------------------------------
set.seed(123)
seeds <- vector(mode = "list", length = 715)
for(i in 1:714) seeds[[i]] <- sample.int(1000, 5)

## For the last model:
seeds[[715]] <- sample.int(1000, 1)
# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "timeslice",
                              initialWindow = 300,
                              horizon = 300,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              seeds = seeds)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)


# METODOS SVM -------------------------------------------------------------
####SIMPLE

  
modelo_knn<-train(SMA_difnivel ~ SMA_prep_lag1 + SMA_difnivel_lag1 + SMA_difnivel_lag1, data = entrenamiento,
                              method = "svmLinear",
                              trControl = control_train,
                  tuneLength=5)             
modelo_knn1 <- train(SMA_difnivel ~ SMA_prep_lag1 + SMA_prep + SMA_difnivel_lag1, data = entrenamiento,
                                 method = "svmLinear",
                                 trControl = control_train,
                     tuneLength=5)                             

modelo_knn2 <- train(SMA_difnivel ~ SMA_prep_lag1 * SMA_prep  * SMA_difnivel_lag1, 
                     data = entrenamiento,
                     method = "svmLinear",
                     trControl = control_train,
                     tuneLength=5)               


  
  
  uncorrected<-prediccion$dif_nivel
  
  
  gg_graph<- ggplot(data = prediccion)+
    geom_line(aes(y=uncorrected, 
                  x=prediccion$Date), 
              alpha=0.5)+
    xlab(paste0(range(prediccion$Date)))+
    ylab ("nivel [msnm]")+
    ggtitle(paste0("Predicción de nivel \n nivel -x y  WRF -x  \n  con un lag de  ", j, " horas"))+
    ylim( c(-0.1,0.1))+
    geom_line(aes(y=SMA_difnivel,
                  x=Date), 
              col="black", lty=2)+
    geom_line(aes(y=ifelse(rep(is.character(modelo_knn), length(prediccion$Date)), 
                           rep(0, length(prediccion$Date)),
                           predict(modelo_knn, 
                                   newdata = prediccion)),
                  x=Date), 
              col="red") +
    geom_line(aes(y=ifelse(rep(is.character(modelo_knn1), length(prediccion$Date)), 
                           rep(0, length(prediccion$Date)),
                           predict(modelo_knn1, 
                                   newdata = prediccion)),
                  x=Date), 
              col="blue")+
    geom_line(aes(y=ifelse(rep(is.character(modelo_knn2), length(prediccion$Date)), 
                           rep(0, length(prediccion$Date)),
                           predict(modelo_knn2, 
                                   newdata = prediccion)),
                  x=Date), 
              col="green")+
    theme_light()+theme(plot.title = element_text(hjust = 0.5))
  
  print(gg_graph)
  
  


##########Diferencia de nivel y aportacion 
path_hist_WRF<- here::here('Data/Parques/Belesar/Historico/Hist_D1D2_DHI_MERGED.RDS')
clean_data<- readRDS(path_hist_WRF)
clean_data_oct<-lapply(clean_data, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2018/10/01")),])
})


fecha_cut<- ymd("2019/01/15")
#cortar en entrenamiento y predicción
cut_train<- lapply(clean_data_oct[c(14,18,19)], function(x,fecha_end){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2018/12/01")
    
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
},
fecha_end=fecha_cut)
cut_train<- lapply(cut_train, function(x){
  x[[1]][,c("Date","LON", "LAT", "prep_hourly", "Lluvia_mm", 
            "aport_mean_SMA", "nivel_mean_SMA")]
  
  
})


cut_predict<- lapply(clean_data_oct[c(14,18,19)], function(x, fecha_ini){
  y<- lapply(x, function(r){
    
    
    fecha_end<- ymd("2019/02/20")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
}, 
fecha_ini=fecha_cut)
cut_predict<- lapply(cut_predict, function(x){
  x[[1]][,c("Date","LON", "LAT", "prep_hourly", "Lluvia_mm", 
            "aport_mean_SMA", "nivel_mean_SMA")]
  
  
})

######## usamos clean data oct porque train se ocupa de 
# cortar los datos. 
x<- cut_predict[[1]]
x$aport1<- lag(x$aport_mean_SMA, 24)
x$aport2<- lag(x$aport_mean_SMA, 48)
x$aport3<- lag(x$aport_mean_SMA, 72)

x$prep_hourly1<- lead(x$prep_hourly, 24)
x$prep_hourlySMA12<- SMA(x$prep_hourly, 12)
x$prep_hourlySMA24<- SMA(x$prep_hourly, 24)
x$prep_hourlySMA36<- SMA(x$prep_hourly, 36)
x$prep_hourlySMA48<- SMA(x$prep_hourly, 48)
x$prep_hourlySMA48_lag<- lag(x$prep_hourlySMA48, 24)

x<- x[complete.cases(x),]


y<- cut_train[[1]]
y$aport1<- lag(y$aport_mean_SMA, 24)
y$aport2<- lag(y$aport_mean_SMA, 48)
y$aport3<- lag(y$aport_mean_SMA, 72)
y$prep_hourly1<- lead(y$prep_hourly, 24)
y$prep_hourlySMA12<- SMA(y$prep_hourly, 12)
y$prep_hourlySMA24<- SMA(y$prep_hourly, 24)
y$prep_hourlySMA36<- SMA(y$prep_hourly, 36)
y$prep_hourlySMA48<- SMA(y$prep_hourly, 48)
y$prep_hourlySMA48_lag<- lag(y$prep_hourlySMA48, 24)

y<- y[complete.cases(y),]

indexxx<- 285
fecha_ini<- y$Date[indexxx]
fecha_end<- y$Date[indexxx]+ as.difftime(3, units = "days")

y<- y[which(y$Date> fecha_ini & y$Date< fecha_end), ]

# PARÁMETROS TRAIN CARET --------------------------------------------------
set.seed(123)
seeds <- vector(mode = "list", length = 715)
for(i in 1:714) seeds[[i]] <- sample.int(1000, 5)

## For the last model:
seeds[[715]] <- sample.int(1000, 1)
control_train <- trainControl(method = "timeslice",
                              initialWindow = 300,
                              horizon = 300,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              seeds = seeds)


fit_4  <- train(aport_mean_SMA ~ prep_hourlySMA48_lag * prep_hourlySMA48, 
                data = y,
                method= "gam")

pred_aport<- predict(fit_4, newdata= x)


uncorrected<-x$aport_mean_SMA

plot(uncorrected, x=x$Date, type = "l",
     ylim = c(0,600), 
     xlab = paste0(range(y$Date)),
     ylab = "Aportación [m³/s]",
     main =  "Predicción de aportación \n empleando diferentes métodos")
lines(pred_aport,x=x$Date, col="red")

