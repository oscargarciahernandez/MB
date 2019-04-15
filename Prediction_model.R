## Empezando prediccion model
library(GGally)
library(e1071)

Historico_Belesar<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_Merged_TodasVariales.RDS'))

clean_data<- lapply(Historico_Belesar, function(x){
    x1<- lapply(x, function(y){
    y$T02_MEAN<- x$D1$T02_MEAN-273.15
    
    mean_prep <- mean(y$prep_hourly)
    mean_rainfall <- mean(y$`Rainfall[mm]`)
    
    mean_aport <- mean(y$`Aportacion[m³/s]`)
    mean_temp <- mean(y$T02_MEAN)
    mean_turb <- mean(y$`Turbinado[m³/s]`)
    mean_turbce <- mean(y$`Turbinado_BCE[m³/s]`)
   
    y$prep_hourly[which(y$prep_hourly<0)]<- mean_prep
    y$`Rainfall[mm]`[which(y$`Rainfall[mm]`<0)]<- mean_rainfall
    y$`Aportacion[m³/s]`[which(y$`Aportacion[m³/s]`<0)]<- mean_aport
    y$T02_MEAN[which(y$T02_MEAN<(-20))]<- mean_temp
    y$`Turbinado[m³/s]`[which(y$`Turbinado[m³/s]`<0)]<-mean_turb
    y$`Turbinado_BCE[m³/s]`[which(y$`Turbinado_BCE[m³/s]`<0)]<-mean_turbce
    
    
    return(y)
    
  })
    return(x1)
 
  
})

cut_train<- lapply(clean_data, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2018/10/01")
    fecha_end<- ymd("2019/02/01")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_predict<- lapply(clean_data, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/02/01")
    fecha_end<- ymd("2019/02/20")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})



ggpairs(data=cut_january$`-8.02328491210938__42.1343421936035`$D1, columns=4:14)

data_predict<- cut_train$`-8.02328491210938__42.1343421936035`$D1
data_predict2<- cut_predict$`-8.02328491210938__42.1343421936035`$D1
    
fit_1  <- lm(`Rainfall[mm]` ~ prep_hourly, data = data_predict)
fit_2  <- lm(`Rainfall[mm]` ~ prep_hourly + T02_MEAN, data = data_predict)
fit_3  <- lm(`Rainfall[mm]` ~ prep_hourly + PSFC, data = data_predict)
fit_4  <- lm(`Rainfall[mm]` ~ prep_hourly + WS_MAX, data = data_predict)
fit_5  <- lm(`Rainfall[mm]` ~ prep_hourly * T02_MEAN, data = data_predict)
fit_6  <- lm(`Rainfall[mm]` ~ prep_hourly * PSFC, data = data_predict)
fit_7  <- lm(`Rainfall[mm]` ~ prep_hourly * WS_MAX, data = data_predict)


uncorrected<-data_predict2$prep_hourly
prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             T02_MEAN =data_predict2$T02_MEAN))
prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             PSFC =data_predict2$PSFC))
prediction_rain4<- predict(fit_4, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             WS_MAX =data_predict2$WS_MAX))
prediction_rain5<- predict(fit_5, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             T02_MEAN =data_predict2$T02_MEAN))
prediction_rain6<- predict(fit_6, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             PSFC =data_predict2$PSFC))
prediction_rain7<- predict(fit_7, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             WS_MAX =data_predict2$WS_MAX))

observed_rain<-data_predict2$`Rainfall[mm]`


plot(prediction_rain, type = "l", ylim = c(0,5))
lines(observed_rain, col="red")
lines(uncorrected, col="green")
lines(prediction_rain2, col="blue")
lines(prediction_rain3, col="purple")
lines(prediction_rain4, col="yellow")

lines(prediction_rain5, col="blue", lty="dotted")
lines(prediction_rain6, col="purple",lty="dotted")
lines(prediction_rain7, col="yellow",lty="dotted")

plot(cumsum(prediction_rain), type = "l", ylim = c(0,100))
lines(cumsum(observed_rain), col="red")
lines(cumsum(uncorrected), col="green")
lines(cumsum(prediction_rain2), col="blue")
lines(cumsum(prediction_rain3), col="purple")
lines(cumsum(prediction_rain4), col="yellow")

lines(cumsum(prediction_rain5), col="blue", lty="dotted")
lines(cumsum(prediction_rain6), col="purple",lty="dotted")
lines(cumsum(prediction_rain7), col="yellow",lty="dotted")



cbind(cor(uncorrected, observed_rain),
      cor(prediction_rain, observed_rain),
      cor(prediction_rain2, observed_rain),
      cor(prediction_rain3, observed_rain),
      cor(prediction_rain4, observed_rain),
      cor(prediction_rain5, observed_rain),
      cor(prediction_rain6, observed_rain),
      cor(prediction_rain7, observed_rain))


prediction_rain31<- ifelse(prediction_rain3<0,0,prediction_rain3)
prediction_rain61<- ifelse(prediction_rain6<0,0,prediction_rain6)


plot(cumsum(observed_rain), type = "l", ylim = c(0,100))
lines(cumsum(uncorrected), col="green")
lines(cumsum(prediction_rain31), col="purple")
lines(cumsum(prediction_rain61), col="yellow")


plot(observed_rain, type = "l", ylim = c(0,5))
lines(uncorrected, col="green")
lines(prediction_rain31, col="purple")
lines(prediction_rain61, col="yellow")


#SVM

data_predict<- cut_train$`-8.02328491210938__42.1343421936035`$D1
data_predict2<- cut_predict$`-8.02328491210938__42.1343421936035`$D1

fit_1  <- svm(`Rainfall[mm]` ~ prep_hourly, data = data_predict)
fit_2  <- svm(`Rainfall[mm]` ~ prep_hourly + T02_MEAN, data = data_predict)
fit_3  <- svm(`Rainfall[mm]` ~ prep_hourly + PSFC, data = data_predict)
fit_4  <- svm(`Rainfall[mm]` ~ prep_hourly + WS_MAX, data = data_predict)
fit_5  <- svm(`Rainfall[mm]` ~ prep_hourly * T02_MEAN, data = data_predict)
fit_6  <- svm(`Rainfall[mm]` ~ prep_hourly * PSFC, data = data_predict)
fit_7  <- svm(`Rainfall[mm]` ~ prep_hourly * WS_MAX, data = data_predict)

uncorrected<-data_predict2$prep_hourly
prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             T02_MEAN =data_predict2$T02_MEAN))
prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             PSFC =data_predict2$PSFC))
prediction_rain4<- predict(fit_4, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             WS_MAX =data_predict2$WS_MAX))
prediction_rain5<- predict(fit_5, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             T02_MEAN =data_predict2$T02_MEAN))
prediction_rain6<- predict(fit_6, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             PSFC =data_predict2$PSFC))
prediction_rain7<- predict(fit_7, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             WS_MAX =data_predict2$WS_MAX))

observed_rain<-data_predict2$`Rainfall[mm]`



plot(prediction_rain, type = "l", ylim = c(0,5))
lines(observed_rain, col="red")
lines(uncorrected, col="green")
lines(prediction_rain2, col="blue")
lines(prediction_rain3, col="purple")
lines(prediction_rain4, col="yellow")

lines(prediction_rain5, col="blue", lty="dotted")
lines(prediction_rain6, col="purple",lty="dotted")
lines(prediction_rain7, col="yellow",lty="dotted")

cbind(cor(uncorrected, observed_rain),
      cor(prediction_rain, observed_rain),
      cor(prediction_rain2, observed_rain),
      cor(prediction_rain3, observed_rain),
      cor(prediction_rain4, observed_rain),
      cor(prediction_rain5, observed_rain),
      cor(prediction_rain6, observed_rain),
      cor(prediction_rain7, observed_rain))

plot(cumsum(prediction_rain), type = "l", ylim = c(0,100))
lines(cumsum(observed_rain), col="red")
lines(cumsum(uncorrected), col="green")
lines(cumsum(prediction_rain2), col="blue")
lines(cumsum(prediction_rain3), col="purple")
lines(cumsum(prediction_rain4), col="yellow")

lines(cumsum(prediction_rain5), col="blue", lty="dotted")
lines(cumsum(prediction_rain6), col="purple",lty="dotted")
lines(cumsum(prediction_rain7), col="yellow",lty="dotted")




svm_tune <- tune(svm, `Rainfall[mm]` ~ prep_hourly,data = data_predict,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
)

best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, data_predict2) 




#Queremos representar obtener todas las correlaciones del rollo. 
cut_train<- lapply(clean_data, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2018/10/01")
    fecha_end<- ymd("2019/02/01")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_predict<- lapply(clean_data, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/02/01")
    fecha_end<- ymd("2019/02/20")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
lista_TL<- list()
for (i in 1:length(cut_train)) {
  lista_d1d2<- list()
 for (j in 1:2) {
   data_predict<- cut_train[[i]][[j]]
   data_predict2<- cut_predict[[i]][[j]]
   
   fit_1  <- lm(`Rainfall[mm]` ~ prep_hourly, data = data_predict)
   fit_2  <- lm(`Rainfall[mm]` ~ prep_hourly + T02_MEAN, data = data_predict)
   fit_3  <- lm(`Rainfall[mm]` ~ prep_hourly + PSFC, data = data_predict)
   fit_4  <- lm(`Rainfall[mm]` ~ prep_hourly + WS_MAX, data = data_predict)
   fit_5  <- lm(`Rainfall[mm]` ~ prep_hourly * T02_MEAN, data = data_predict)
   fit_6  <- lm(`Rainfall[mm]` ~ prep_hourly * PSFC, data = data_predict)
   fit_7  <- lm(`Rainfall[mm]` ~ prep_hourly * WS_MAX, data = data_predict)
   
   uncorrected<-data_predict2$prep_hourly
   prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
   prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                T02_MEAN =data_predict2$T02_MEAN))
   prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                PSFC =data_predict2$PSFC))
   prediction_rain4<- predict(fit_4, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                WS_MAX =data_predict2$WS_MAX))
   prediction_rain5<- predict(fit_5, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                T02_MEAN =data_predict2$T02_MEAN))
   prediction_rain6<- predict(fit_6, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                PSFC =data_predict2$PSFC))
   prediction_rain7<- predict(fit_7, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                WS_MAX =data_predict2$WS_MAX))
   
   observed_rain<-data_predict2$`Rainfall[mm]`
   
   
   
   tabla_cor<-cbind(cor(uncorrected, observed_rain),
                    cor(prediction_rain, observed_rain),
                    cor(prediction_rain2, observed_rain),
                    cor(prediction_rain3, observed_rain),
                    cor(prediction_rain4, observed_rain),
                    cor(prediction_rain5, observed_rain),
                    cor(prediction_rain6, observed_rain),
                    cor(prediction_rain7, observed_rain))
   
   fit_1  <- svm(`Rainfall[mm]` ~ prep_hourly, data = data_predict)
   fit_2  <- svm(`Rainfall[mm]` ~ prep_hourly + T02_MEAN, data = data_predict)
   fit_3  <- svm(`Rainfall[mm]` ~ prep_hourly + PSFC, data = data_predict)
   fit_4  <- svm(`Rainfall[mm]` ~ prep_hourly + WS_MAX, data = data_predict)
   fit_5  <- svm(`Rainfall[mm]` ~ prep_hourly * T02_MEAN, data = data_predict)
   fit_6  <- svm(`Rainfall[mm]` ~ prep_hourly * PSFC, data = data_predict)
   fit_7  <- svm(`Rainfall[mm]` ~ prep_hourly * WS_MAX, data = data_predict)
   
   uncorrected<-data_predict2$prep_hourly
   prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
   prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                T02_MEAN =data_predict2$T02_MEAN))
   prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                PSFC =data_predict2$PSFC))
   prediction_rain4<- predict(fit_4, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                WS_MAX =data_predict2$WS_MAX))
   prediction_rain5<- predict(fit_5, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                T02_MEAN =data_predict2$T02_MEAN))
   prediction_rain6<- predict(fit_6, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                PSFC =data_predict2$PSFC))
   prediction_rain7<- predict(fit_7, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                WS_MAX =data_predict2$WS_MAX))
   
   observed_rain<-data_predict2$`Rainfall[mm]`
   
   tabla_cor<-cbind(tabla_cor,
                    cor(prediction_rain, observed_rain),
                    cor(prediction_rain2, observed_rain),
                    cor(prediction_rain3, observed_rain),
                    cor(prediction_rain4, observed_rain),
                    cor(prediction_rain5, observed_rain),
                    cor(prediction_rain6, observed_rain),
                    cor(prediction_rain7, observed_rain))
   
   colnames(tabla_cor)<- c("uncorrected", "1","2", "3","4","5","6","7",
                           "svm1","svm2", "svm3","svm4","svm5","svm6","svm7")
   lista_d1d2[[j]]<- tabla_cor
 }
  
  names(lista_d1d2)<- c("D1", "D2")
  lista_TL[[i]]<- lista_d1d2
}


colnames(tabla_cor)<- c("uncorrected", "1","2", "3","4","5","6","7",
                        "svm1","svm2", "svm3","svm4","svm5","svm6","svm7")
names(lista_d1d2)<- c("D1", "D2")


Lista_TL_names<-lapply(lista_TL, function(x){
  y<- lapply(x, function(r) {
    r<- as.data.frame(r)
    names(r)<- c("uncorrected", "1","2", "3","4","5","6","7",
                 "svm1","svm2", "svm3","svm4","svm5","svm6","svm7")
    return(r)
  })
  names(y)<- c("D1", "D2")
  return(y)
})
names(Lista_TL_names)<- names(cut_predict)

y<- lapply(Lista_TL_names, function(x){
  x<- lapply(x, function(r) {bind_rows(r)})
  return(x)
  })

lapply(y, function(x){
  bind_rows(x, .id="id")
})


bind_rows(y)
y[[1]]
