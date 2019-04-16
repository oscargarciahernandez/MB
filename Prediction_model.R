library(here)
source(here::here('libraries.R'))

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


x<- lapply(Lista_TL_names, function(x){bind_rows(as.data.frame(x))})
y<- bind_rows(x, .id="id")

cor_table<- as.data.frame(matrix(ncol = 3, nrow = 43))
colnames(cor_table)<- c("id","corr", "name")

for (i in 2:length(y[1,])) {
  f<- as.data.frame(cbind(y[which.max(y[,i]), c(1,i)], names( y[which.max(y[,i]), c(1,i)])[2]))
  colnames(f)<- c("id","corr", "name")
  cor_table<- rbind(cor_table,f )
}

cor_table<- cor_table[complete.cases(cor_table),]
table(cor_table$id)


punto_Belesar<- c(42.628577,-7.713948)
extract_lat_lon<- lapply(str_split(cor_table$id,"__"), function(x){
  return(as.data.frame(cbind(x[[1]],x[[2]])))
})

extract_lat_lon<- as.data.frame(bind_rows(extract_lat_lon))


cor_table<- as.data.frame(cbind(extract_lat_lon, cor_table[,2:3]))

colnames(cor_table)<- c("LON","LAT", "Corr", "Method")



library(geosphere)
vec_dist<- vector()
for (i in 1:length(cor_table[,1])) {
  vec_dist[i]<- distm(c(-7.713948, 42.628577), c(as.numeric(cor_table[i,1]), 
                                                 as.numeric(cor_table[i,2])), 
        fun = distHaversine)
  
}

vec_dist<- round(vec_dist/1000, digits = 1)

cor_table<- as.data.frame(cbind(cor_table, vec_dist))


# Aportacion --------------------------------------------------------------


#Vamos a trabajar en la correlacion de la aportacion a partir de aquí

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

extract_WRF<- lapply(clean_data, function(x){
  x1<- lapply(x, function(y){
    y<- as.data.frame(y[,1:7])
    y$Date<-y$Date-hms("24:00:00") 
    return(y)
  })
  return(x1)
})

extract_DHI<- lapply(clean_data, function(x){
  x1<- lapply(x, function(y){
    y<- as.data.frame(y[,c(1,8:14)])
    return(y)
  })
  return(x1)
})

lista_desfase_1d<- list()
for (i in 1:length(extract_WRF)) {
  list_d1_d2<- list()
  for (j in 1:2) {
    df1<- extract_WRF[[i]][[j]]
    df2<- extract_DHI[[i]][[j]]
    Merge_dplyr<- left_join(df1, df2, by=c("Date"))
    Merge_dplyr<- Merge_dplyr[complete.cases(Merge_dplyr),]
    list_d1_d2[[j]]<- Merge_dplyr
    
    
  }
  names(list_d1_d2)<- c("D1", "D2")
  lista_desfase_1d[[i]]<- list_d1_d2
  
  
}
names(lista_desfase_1d)<- names(extract_WRF)


cut_train<- lapply(lista_desfase_1d, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2018/10/01")
    fecha_end<- ymd("2019/01/14")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_predict<- lapply(lista_desfase_1d, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/01/15")
    fecha_end<- ymd("2019/02/15")
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
    
    fit_1  <- lm(`Aportacion[m³/s]`~ prep_hourly, data = data_predict)
    fit_2  <- lm(`Aportacion[m³/s]`~ prep_hourly + T02_MEAN, data = data_predict)
    fit_3  <- lm(`Aportacion[m³/s]`~ prep_hourly + PSFC, data = data_predict)
    fit_4  <- lm(`Aportacion[m³/s]`~ prep_hourly + WS_MAX, data = data_predict)
    fit_5  <- lm(`Aportacion[m³/s]`~ prep_hourly * T02_MEAN, data = data_predict)
    fit_6  <- lm(`Aportacion[m³/s]`~ prep_hourly * PSFC, data = data_predict)
    fit_7  <- lm(`Aportacion[m³/s]`~ prep_hourly * WS_MAX, data = data_predict)
    
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
    
    observed_rain<-SMA(data_predict2$`Aportacion[m³/s]`,10)
    
    
    
    tabla_cor<-cbind(cor(uncorrected, observed_rain),
                     cor(prediction_rain, observed_rain),
                     cor(prediction_rain2, observed_rain),
                     cor(prediction_rain3, observed_rain),
                     cor(prediction_rain4, observed_rain),
                     cor(prediction_rain5, observed_rain),
                     cor(prediction_rain6, observed_rain),
                     cor(prediction_rain7, observed_rain))
    
    fit_1  <- svm(`Aportacion[m³/s]`~ prep_hourly, data = data_predict)
    fit_2  <- svm(`Aportacion[m³/s]`~ prep_hourly + T02_MEAN, data = data_predict)
    fit_3  <- svm(`Aportacion[m³/s]`~ prep_hourly + PSFC, data = data_predict)
    fit_4  <- svm(`Aportacion[m³/s]`~ prep_hourly + WS_MAX, data = data_predict)
    fit_5  <- svm(`Aportacion[m³/s]`~ prep_hourly * T02_MEAN, data = data_predict)
    fit_6  <- svm(`Aportacion[m³/s]`~ prep_hourly * PSFC, data = data_predict)
    fit_7  <- svm(`Aportacion[m³/s]`~ prep_hourly * WS_MAX, data = data_predict)
    
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
    
    observed_rain<-SMA(data_predict2$`Aportacion[m³/s]`, 10)
    
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


x<- lapply(Lista_TL_names, function(x){bind_rows(as.data.frame(x))})
y<- bind_rows(x, .id="id")

cor_table<- as.data.frame(matrix(ncol = 3, nrow = 43))
colnames(cor_table)<- c("id","corr", "name")

for (i in 2:length(y[1,])) {
  f<- as.data.frame(cbind(y[which.max(y[,i]), c(1,i)], names( y[which.max(y[,i]), c(1,i)])[2]))
  colnames(f)<- c("id","corr", "name")
  cor_table<- rbind(cor_table,f )
}

cor_table<- cor_table[complete.cases(cor_table),]
table(cor_table$id)


punto_Belesar<- c(42.628577,-7.713948)
extract_lat_lon<- lapply(str_split(cor_table$id,"__"), function(x){
  return(as.data.frame(cbind(x[[1]],x[[2]])))
})

extract_lat_lon<- as.data.frame(bind_rows(extract_lat_lon))


cor_table<- as.data.frame(cbind(extract_lat_lon, cor_table[,2:3]))

colnames(cor_table)<- c("LON","LAT", "Corr", "Method")



library(geosphere)
vec_dist<- vector()
for (i in 1:length(cor_table[,1])) {
  vec_dist[i]<- distm(c(-7.713948, 42.628577), c(as.numeric(cor_table[i,1]), 
                                                 as.numeric(cor_table[i,2])), 
                      fun = distHaversine)
  
}

vec_dist<- round(vec_dist/1000, digits = 1)

cor_table<- as.data.frame(cbind(cor_table, vec_dist))




# DHI histórico para hacer un daily average.  -----------------------------

DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar_Todas_Variables.RDS'))
View(DHI)

DHI_2018<- DHI[which(year(DHI$Date)==2018), ]
V<- yday(DHI_2018$Date)

Dayli_averages_2018<- as.data.frame(cbind(tapply(DHI_2018[,2], V,mean),
                    tapply(DHI_2018[,3], V,mean),
                    tapply(DHI_2018[,4], V,mean),
                    tapply(DHI_2018[,5], V,mean),
                    tapply(DHI_2018[,6], V,mean),
                    tapply(DHI_2018[,7], V,mean),
                    tapply(DHI_2018[,8], V,mean)))
names(Dayli_averages_2018)<- colnames(DHI_2018)[2:length(colnames(DHI))]





DHI_2019<- DHI[which(year(DHI$Date)==2019), ]
V<- yday(DHI_2019$Date)

Dayli_averages_2019<- as.data.frame(cbind(tapply(DHI_2019[,2], V,mean),
                                          tapply(DHI_2019[,3], V,mean),
                                          tapply(DHI_2019[,4], V,mean),
                                          tapply(DHI_2019[,5], V,mean),
                                          tapply(DHI_2019[,6], V,mean),
                                          tapply(DHI_2019[,7], V,mean),
                                          tapply(DHI_2019[,8], V,mean)))
names(Dayli_averages_2019)<- colnames(DHI_2019)[2:length(colnames(DHI))]

Daily_avg_DHI<- cbind(seq(range(DHI$Date)[1], range(DHI$Date)[2], by="day"),
      rbind(Dayli_averages_2018,Dayli_averages_2019))

names(Daily_avg_DHI)<- colnames(DHI_2018)
Daily_avg_DHI$Date<- ymd(Daily_avg_DHI$Date)



# WRF dayli avgs ----------------------------------------------------------
Historico_Belesar<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_Merged_TodasVariales.RDS'))

WRF<- Historico_Belesar[[1]]$D1[complete.cases(Historico_Belesar[[1]]$D1),]
WRF_2018<- WRF[which(year(WRF$Date)==2018), ]
V<- yday(WRF_2018$Date)

Dayli_averages_2018<- as.data.frame(cbind(tapply(WRF_2018[,1], V,mean),
                                          tapply(WRF_2018[,4], V,mean),
                                          tapply(WRF_2018[,5], V,mean),
                                          tapply(WRF_2018[,6], V,mean),
                                          tapply(WRF_2018[,7], V,mean)))

names(Dayli_averages_2018)<- colnames(WRF_2018)[c(1,4:7)]

WRF_2019<- WRF[which(year(WRF$Date)==2019), ]
V<- yday(WRF_2019$Date)

Dayli_averages_2019<- as.data.frame(cbind(tapply(WRF_2019[,1], V,mean),
                                          tapply(WRF_2019[,4], V,mean),
                                          tapply(WRF_2019[,5], V,mean),
                                          tapply(WRF_2019[,6], V,mean),
                                          tapply(WRF_2019[,7], V,mean)))
names(Dayli_averages_2019)<- colnames(WRF_2019)[c(1,4:7)]

Daily_avg_WRF<- rbind(Dayli_averages_2018,Dayli_averages_2019)
Daily_avg_WRF$Date<- as_datetime(Daily_avg_WRF$Date)
Daily_avg_WRF$Date<- ymd(format(Daily_avg_WRF$Date, format="%Y-%m-%d"))
names(Daily_avg_WRF)<- colnames(WRF_2018)


df1<- Daily_avg_WRF
df2<- Daily_avg_DHI


Merge_dplyr<- left_join(df1, df2, by=c("Date"))
Merge_dplyr_avgs<- Merge_dplyr[complete.cases(Merge_dplyr),]

#path_to_file<- here::here('Data/Parques/Belesar/Historico/DHI_WRF_Dayli_AVGS.RDS')
#saveRDS(Merge_dplyr_avgs, path_to_file)


# Lista_dayli avg ---------------------------------------------------------


Historico_Belesar<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_Merged_TodasVariales.RDS'))
DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar_Todas_Variables.RDS'))

lista_Daily_avg<- list()
for (j in 1:length(Historico_Belesar)) {
  lista_d1d2<- list()
  for (i in 1:2) {
    WRF<- Historico_Belesar[[j]][[i]][complete.cases(Historico_Belesar[[j]][[i]]),]
    WRF_2018<- WRF[which(year(WRF$Date)==2018), ]
    V<- yday(WRF_2018$Date)
    
    Dayli_averages_2018<- as.data.frame(cbind(tapply(WRF_2018[,1], V,mean),
                                              tapply(WRF_2018[,4], V,mean),
                                              tapply(WRF_2018[,5], V,mean),
                                              tapply(WRF_2018[,6], V,mean),
                                              tapply(WRF_2018[,7], V,mean)))
    
    names(Dayli_averages_2018)<- colnames(WRF_2018)[c(1,4:7)]
    
    WRF_2019<- WRF[which(year(WRF$Date)==2019), ]
    V<- yday(WRF_2019$Date)
    
    Dayli_averages_2019<- as.data.frame(cbind(tapply(WRF_2019[,1], V,mean),
                                              tapply(WRF_2019[,4], V,mean),
                                              tapply(WRF_2019[,5], V,mean),
                                              tapply(WRF_2019[,6], V,mean),
                                              tapply(WRF_2019[,7], V,mean)))
    names(Dayli_averages_2019)<- colnames(WRF_2019)[c(1,4:7)]
    
    Daily_avg_WRF<- rbind(Dayli_averages_2018,Dayli_averages_2019)
    Daily_avg_WRF$Date<- as_datetime(Daily_avg_WRF$Date)
    Daily_avg_WRF$Date<- ymd(format(Daily_avg_WRF$Date, format="%Y-%m-%d"))
    names(Daily_avg_WRF)<- colnames(Dayli_averages_2019)
    

    DHI_2018<- DHI[which(year(DHI$Date)==2018), ]
    V<- yday(DHI_2018$Date)
    
    Dayli_averages_2018<- as.data.frame(cbind(tapply(DHI_2018[,2], V,mean),
                                              tapply(DHI_2018[,3], V,mean),
                                              tapply(DHI_2018[,4], V,mean),
                                              tapply(DHI_2018[,5], V,mean),
                                              tapply(DHI_2018[,6], V,mean),
                                              tapply(DHI_2018[,7], V,mean),
                                              tapply(DHI_2018[,8], V,mean)))
    names(Dayli_averages_2018)<- colnames(DHI_2018)[2:length(colnames(DHI))]
    
    
    
    
    
    DHI_2019<- DHI[which(year(DHI$Date)==2019), ]
    V<- yday(DHI_2019$Date)
    
    Dayli_averages_2019<- as.data.frame(cbind(tapply(DHI_2019[,2], V,mean),
                                              tapply(DHI_2019[,3], V,mean),
                                              tapply(DHI_2019[,4], V,mean),
                                              tapply(DHI_2019[,5], V,mean),
                                              tapply(DHI_2019[,6], V,mean),
                                              tapply(DHI_2019[,7], V,mean),
                                              tapply(DHI_2019[,8], V,mean)))
    names(Dayli_averages_2019)<- colnames(DHI_2019)[2:length(colnames(DHI))]
    
    Daily_avg_DHI<- cbind(seq(range(DHI$Date)[1], range(DHI$Date)[2], by="day"),
                          rbind(Dayli_averages_2018,Dayli_averages_2019))
    
    names(Daily_avg_DHI)<- colnames(DHI_2018)
    Daily_avg_DHI$Date<- ymd(Daily_avg_DHI$Date)
    
    
    
    
    df1<- Daily_avg_WRF
    df2<- Daily_avg_DHI
    
    
    Merge_dplyr<- left_join(df1, df2, by=c("Date"))
    Merge_dplyr_avgs<- Merge_dplyr[complete.cases(Merge_dplyr),]
    lista_d1d2[[i]]<- Merge_dplyr_avgs
  }
  names(lista_d1d2)<- c("D1", "D2")
  lista_Daily_avg[[j]]<- lista_d1d2
}
names(lista_Daily_avg)<- names(Historico_Belesar)
path_to_file<- here::here('Data/Parques/Belesar/Historico/DHI_WRF_Dayli_AVGS.RDS')
saveRDS(lista_Daily_avg, path_to_file)




# Ploteos_Daily_AVGS ------------------------------------------------------
path_to_file<- here::here('Data/Parques/Belesar/Historico/DHI_WRF_Dayli_AVGS.RDS')
Daily_avgs<- readRDS(path_to_file)[[1]][[1]]
Daily_avgs<- Daily_avgs[Daily_avgs$Date>ymd("2018/10/01"),] %>% .[complete.cases(.),]

plot(x=Daily_avgs$Date,y=Daily_avgs$`Nivel[msnm]`, ylim = c(0,500), type = "l")
lines(x=Daily_avgs$Date,Daily_avgs$`Aportacion[m³/s]`, col="red")
lines(x=Daily_avgs$Date,Daily_avgs$`Rainfall[mm]`*200, col="blue")
lines(x=Daily_avgs$Date,Daily_avgs$prep_hourly*200, col="green")



# Aportacion --------------------------------------------------------------
Daily_avgs2<- Daily_avgs
x<- 2
Daily_avgs2$Aportminus1<- c(Daily_avgs$`Aportacion[m³/s]`[x:(length(Daily_avgs$Date))],rep(NA, x-1))
Daily_avgs2$aport<- Daily_avgs2$`Aportacion[m³/s]`
data_predict<- Daily_avgs2[Daily_avgs2$Date<ymd("2019/01/01"),]  
data_predict2<- Daily_avgs2[Daily_avgs2$Date>ymd("2019/01/01"),]  
data_predict2<- data_predict2[complete.cases(data_predict2),]
#data_predict3<- data_predict2[data_predict2$Date<ymd("2019/01/06"),]
data_predict3<- data_predict2



# Aportacion con datos horarios -------------------------------------------


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
    fecha_end<- ymd("2019/01/01")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_predict<- lapply(clean_data, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/01/01")
    fecha_end<- ymd("2019/02/20")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})

pred1<- cut_train$`-8.02328491210938__42.1343421936035`$D1
pred2<- cut_predict$`-8.02328491210938__42.1343421936035`$D1


#### construyo entrenamiento.... 
date_antes<- "2018/12/26"
aport<- pred1$`Aportacion[m³/s]`[pred1$Date< ymd(date_antes)]
prep<- pred1$prep_hourly[abs(length(aport)-length(pred1$prep_hourly)-1):length(pred1$Date)]
aport_actual<- pred1$`Aportacion[m³/s]`[abs(length(aport)-length(pred1$prep_hourly)-1):length(pred1$Date)]

data_predict_cut<- as.data.frame(cbind(aport, prep, aport_actual))
###### construir el de test
range(pred2$Date)
aport_test<- pred1$`Aportacion[m³/s]`[pred1$Date> ymd(date_antes)]
prep_test<- pred2$prep_hourly[1:length(aport_test)]
aport_actual_test<- pred1$`Aportacion[m³/s]`[1:length(aport_test)]

data_test_cut<- as.data.frame(cbind(aport_test, prep_test, aport_actual_test))


fit_1  <- lm(aport_actual~ prep * aport , data = data_predict_cut)
fit_2  <- svm(aport_actual~ prep * aport , data = data_predict_cut)

prediction_rain_lm<- predict(fit_1, data.frame(prep=data_test_cut$prep_test,
                                               aport=data_test_cut$aport_test))
prediction_rain_svm<- predict(fit_2, data.frame(prep =data_test_cut$prep_test,
                                               aport=data_test_cut$aport_test))


cor(data_test_cut$aport_actual_test, prediction_rain_lm)
cor(data_test_cut$aport_actual_test, prediction_rain_svm)

plot(data_test_cut$aport_actual_test, type = "l", ylim = c(0,120))
lines(prediction_rain_lm, col="red")
lines(data_test_cut$prep_test, col="green")
lines(prediction_rain_svm, col="blue")



# NLS ---------------------------------------------------------------------
y<- data_predict$`Aportacion[m³/s]`
x<- data_predict$prep_hourly
fit_3  <- nls(y~a*exp(-b*x))
prediction_rain_nls1<- predict(fit_3, data.frame(prep_hourly =data_predict3$prep_hourly))
fit_3  <- nls(y~a*exp(-b*x))
prediction_rain_nls1<- predict(fit_3, data.frame(prep_hourly =data_predict3$prep_hourly))
fit_3  <- nls(y~a*exp(-b*x))
prediction_rain_nls1<- predict(fit_3, data.frame(prep_hourly =data_predict3$prep_hourly))




plot(data_predict$`Aportacion[m³/s]`, type = "l")
lines(prediction_rain_nls1, col="red")
lines(data_predict$prep_hourly*150, col="green")




# Otra seccion más, me eestoy volviendo loko ------------------------------
DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar_Todas_Variables.RDS'))

DHI1<- DHI[complete.cases(DHI),]

DHI_predict1<- DHI1[DHI1$Date<ymd("2018/07/01"),]
DHI_predict2<- DHI1[DHI1$Date>ymd("2018/07/01"),]
DHI_predict2<- DHI_predict2[which(DHI_predict2$`Aportacion[m³/s]`>0),]

DHI_predict1$Lluvia<- DHI_predict1$`Rainfall[mm]`
DHI_predict2$Lluvia<- DHI_predict2$`Rainfall[mm]`


fit_dhi1  <- lm(`Aportacion[m³/s]` ~ SMA(Lluvia,10), data = DHI_predict1)
fit_dhi2 <- svm(`Aportacion[m³/s]` ~ SMA(Lluvia,10), data = DHI_predict1)


prediction_aport1<- predict(fit_dhi1, data.frame(Lluvia=DHI_predict2$Lluvia[10:length(DHI_predict2$Date)]))
prediction_aport2<- predict(fit_dhi2, data.frame(Lluvia=DHI_predict2$Lluvia[10:length(DHI_predict2$Date)]))

plot(SMA(DHI_predict2$`Aportacion[m³/s]`, 48), type = "l")
lines(prediction_aport1+DHI_predict2$`Rainfall[mm]`, col="red")




plot(SMA(DHI_predict2$`Aportacion[m³/s]`, 48), type = "l")
lines(prediction_aport1, col="red")
lines(prediction_aport2, col="green")
lines(DHI_predict2$`Rainfall[mm]`*15, col="blue")
