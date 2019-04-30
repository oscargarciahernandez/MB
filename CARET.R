library(caret)
library(rvest)
library(request)
library(doMC)

x<- getModelInfo()
type<- lapply(x, function(y) {if(length(y$type > 1)){
  return(paste(y$type, collapse = "_"))
}else{return(y$type)}})


Tabla_method<- as.data.frame(cbind(names(x),unlist(type)) , row.names = F) 
colnames(Tabla_method)<- c("Method", "Type")


Tabla_regresion<- Tabla_method[which(str_detect(Tabla_method$Type,
                                                "Regression")),]

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






# Knn ---------------------------------------------------------------------
registerDoMC(cores = 4)
i<- 48
j<- 54 
nmodel<- 1



entrenamiento<- cut_train[[23]]
entrenamiento$SMA_prep<- SMA(entrenamiento$prep_hourly,i)
entrenamiento$SMA_difnivel<- SMA(entrenamiento$dif_nivel, 36)
entrenamiento$SMA_prep_lag1<- lag(entrenamiento$SMA_prep,j)
entrenamiento<- entrenamiento[complete.cases(entrenamiento),]





prediccion<- cut_predict[[23]]
prediccion$SMA_prep<- SMA(prediccion$prep_hourly,i)
prediccion$SMA_difnivel<- SMA(prediccion$dif_nivel, 36)
prediccion$SMA_prep_lag1<- lag(prediccion$SMA_prep,j)
prediccion<- prediccion[complete.cases(prediccion),]






particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(k = seq(1,500,by=10))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_knn <- train(SMA_difnivel ~ SMA_prep_lag1, data = entrenamiento,
                    method = as.character(Tabla_regresion$Method[nmodel]),
                    trControl = control_train)

modelo_knn1 <- train(SMA_difnivel ~ SMA_prep_lag1 + SMA_prep, data = entrenamiento,
                    method = as.character(Tabla_regresion$Method[nmodel]),
                    trControl = control_train)

modelo_knn2 <- train(SMA_difnivel ~ SMA_prep_lag1 * SMA_prep, data = entrenamiento,
                    method = as.character(Tabla_regresion$Method[nmodel]),
                    trControl = control_train)




ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy del modelo KNN", x = "K") +
  theme_bw()



uncorrected<-prediccion$dif_nivel


gg_graph<- ggplot(data = prediccion)+
  geom_line(aes(y=uncorrected, x=prediccion$Date))+
  xlab(paste0(range(prediccion$Date)))+
  ylab ("nivel [msnm]")+
  ggtitle(paste0("Predicción de nivel \n solo con lluvia WRF  \n  con un lag de  ", j, " horas"))+
  labs(subtitle = paste0("Cor = ", round(cor(prediccion$SMA_difnivel,
                                             predict(modelo_knn, 
                                                     newdata = prediccion)), digits = 3)))+
  
  ylim( c(-0.1,0.1))+
  geom_line(aes(y=SMA_difnivel,
                x=Date), 
            col="black", lty=2)+
  geom_line(aes(y=predict(modelo_knn, 
                          newdata = prediccion),
                x=Date), 
            col="red")+
  geom_line(aes(y=predict(modelo_knn1, 
                          newdata = prediccion),
                x=Date), 
            col="blue")+
  geom_line(aes(y=predict(modelo_knn2, 
                          newdata = prediccion),
                x=Date), 
            col="green")+
  theme_light()+theme(plot.title = element_text(hjust = 0.5))

print(gg_graph)





# Naive Bayes -------------------------------------------------------------




registerDoMC(cores = 4)
i<- 48
j<- 54 



entrenamiento<- cut_train[[23]]
entrenamiento$SMA_prep<- SMA(entrenamiento$prep_hourly,i)
entrenamiento$SMA_difnivel<- SMA(entrenamiento$dif_nivel, 36)
entrenamiento$SMA_prep_lag1<- lag(entrenamiento$SMA_prep,j)
entrenamiento<- entrenamiento[complete.cases(entrenamiento),]





prediccion<- cut_predict[[23]]
prediccion$SMA_prep<- SMA(prediccion$prep_hourly,i)
prediccion$SMA_difnivel<- SMA(prediccion$dif_nivel, 36)
prediccion$SMA_prep_lag1<- lag(prediccion$SMA_prep,j)
prediccion<- prediccion[complete.cases(prediccion),]






particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(k = seq(1,500,by=10))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_knn <- train(SMA_difnivel ~ SMA_prep_lag1, data = entrenamiento,
                    method = "nb",
                    tuneGrid = hiperparametros,
                    trControl = control_train)

modelo_knn1 <- train(SMA_difnivel ~ SMA_prep_lag1 + SMA_prep, data = entrenamiento,
                     method = "nb",
                     tuneGrid = hiperparametros,
                     trControl = control_train)

modelo_knn2 <- train(SMA_difnivel ~ SMA_prep_lag1 * SMA_prep, data = entrenamiento,
                     method = "nb ",
                     tuneGrid = hiperparametros,
                     trControl = control_train)




ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy del modelo KNN", x = "K") +
  theme_bw()



uncorrected<-prediccion$dif_nivel


gg_graph<- ggplot(data = prediccion)+
  geom_line(aes(y=uncorrected, x=prediccion$Date))+
  xlab(paste0(range(prediccion$Date)))+
  ylab ("nivel [msnm]")+
  ggtitle(paste0("Predicción de nivel \n solo con lluvia WRF  \n  con un lag de  ", j, " horas"))+
  labs(subtitle = paste0("Cor = ", round(cor(prediccion$SMA_difnivel,
                                             predict(modelo_knn, 
                                                     newdata = prediccion)), digits = 3)))+
  
  ylim( c(-0.1,0.1))+
  geom_line(aes(y=SMA_difnivel,
                x=Date), 
            col="black", lty=2)+
  geom_line(aes(y=predict(modelo_knn, 
                          newdata = prediccion),
                x=Date), 
            col="red")+
  geom_line(aes(y=predict(modelo_knn1, 
                          newdata = prediccion),
                x=Date), 
            col="blue")+
  geom_line(aes(y=predict(modelo_knn2, 
                          newdata = prediccion),
                x=Date), 
            col="green")+
  theme_light()+theme(plot.title = element_text(hjust = 0.5))

print(gg_graph)









# miscelania --------------------------------------------------------------



#Los mejores SMA's
i<- 48
j<- 54 

  
predict_1<- cut_predict[[23]]
predict_1$SMA_prep<- SMA(predict_1$prep_hourly,i)
predict_1$SMA_difnivel<- SMA(predict_1$dif_nivel, 36)
predict_1$SMA_prep_lag1<- lag(predict_1$SMA_prep,j)

predict_1<- predict_1[complete.cases(predict_1),]

train<- cut_train[[23]]
train$SMA_prep<- SMA(train$prep_hourly,i)
train$SMA_difnivel<- SMA(train$dif_nivel, 36)
train$SMA_prep_lag1<- lag(train$SMA_prep,j)
train<- train[complete.cases(train),]



fit_2  <- lm(dif_nivel ~  SMA_prep_lag1, data = train)






uncorrected<-predict_1$dif_nivel

pred_nivel2<- predict(fit_2, data.frame(SMA_prep_lag1 =predict_1$SMA_prep_lag1))
assign(paste0("pred_nivel",j), pred_nivel2)


gg_graph<- ggplot(data = predict_1)+
  geom_line(aes(y=uncorrected, x=predict_1$Date))+
  xlab(paste0(range(predict_1$Date)))+
  ylab ("nivel [msnm]")+
  ggtitle(paste0("Predicción de nivel \n solo con lluvia WRF  \n  con un lag de  ", j, " horas"))+
  labs(subtitle = paste0("Cor = ", round(cor(predict_1$SMA_difnivel,
                                             eval(parse(text = paste0("pred_nivel",j))) ), digits = 3)))+
  
  ylim( c(-0.1,0.1))+
  geom_line(aes(y=SMA_difnivel,x=Date), col="black", lty=2)+
  geom_line(aes(y=eval(parse(text = paste0("pred_nivel",j))),x=Date), col="red")+
  theme_light()+theme(plot.title = element_text(hjust = 0.5))

print(gg_graph)


