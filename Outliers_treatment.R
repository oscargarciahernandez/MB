library(here)
source(here::here('libraries.R'))

library(outliers)
library(imputeTS)

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mar = rep(2, 4))
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  message("Outliers successfully removed", "\n")
  return(invisible(dt))
  
}





DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar.RDS'))


DHI$`APORTACION (m3/s)`[which(DHI$`APORTACION (m3/s)`<0)]<- NA 
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)
outlierKD(DHI,`APORTACION (m3/s)`)


plot(DHI$`APORTACION (m3/s)`)
DHI$`APORTACION (m3/s)`<-  na.interpolation(DHI$`APORTACION (m3/s)`)



library(TTR)

for(n in seq(48,192,length.out = 4)){
  Mavg<- SMA(DHI$`APORTACION (m3/s)`, n) 
  
  plot(DHI$DATE, Mavg,
       xlab = "Date",
       ylab= "Aportacion [m³/s]", type = "l")
}


length(Mavg)

outlierKD(DHI,`NIVEL EMBALSE (msnm)`)
outlierKD(DHI,`NIVEL EMBALSE (msnm)`)
outlierKD(DHI,`NIVEL EMBALSE (msnm)`)
DHI$`NIVEL EMBALSE (msnm)`<-  na.interpolation(DHI$`NIVEL EMBALSE (msnm)`)


x<- 96
aportacion_SMA<- SMA(DHI$`APORTACION (m3/s)`,x)
nivel_SMA<- SMA(c(0,diff(DHI$`NIVEL EMBALSE (msnm)`)*8000+150), x)

aportacion_SMA<- aportacion_SMA[!is.na(aportacion_SMA)]
nivel_SMA<- nivel_SMA[!is.na(nivel_SMA)]

cor(aportacion_SMA, nivel_SMA)

plot(aportacion_SMA,type = "l" )

lines(nivel_SMA, col = "red")


#Cross-correlation y otras maneras de hacer analisis superways
ccf_belesar<- ccf(aportacion_SMA, nivel_SMA, lag.max = 5000)
#Máxima correlación
max(ccf_belesar$acf)
#Con cuanto desfase se produce la máxima correlación. 
ccf_belesar$lag[which.max(ccf_belesar$acf)]



##Pasamos a tratar la Lluvia
#ojito con la utilidad del comando lag y el comando lead

DHI$`LLUVIA ACUMULADA DÍA (l/m2)`<- lead(DHI$`LLUVIA ACUMULADA DÃA (l/m2)`)

Prueba_desacumular<- DHI %>% group_by(yday(DATE), year(DATE)) %>% mutate(desacumulada= c(0,diff(`LLUVIA ACUMULADA DÍA (l/m2)`)),
                                                                         lluvia=na.interpolation(ifelse(desacumulada>=0, desacumulada, NA)))



#outlierKD(Prueba_desacumular, lluvia)

View(DHI)
acum_diaria<- seq(range(DHI$DATE)[1],range(DHI$DATE)[2],
                  by="hour")

Prueba_desacumular$`yday(DATE)`<- NULL
Prueba_desacumular$`year(DATE)`<- NULL

prueba<- as.data.frame(Prueba_desacumular) %>% group_by(hour(DATE), yday(DATE), year(DATE))  %>% 
  summarize(., Acum_horaria=sum(lluvia, na.rm = T),
            aport_mean=mean(`APORTACION (m3/s)`, na.rm = T),
            nivel_mean=mean(`NIVEL EMBALSE (msnm)`, na.rm = T))


Lluvia_acum_horaria<- as.data.frame(cbind(as.character(acum_diaria[2:length(acum_diaria)]), 
                                          prueba[,4:6]))

Lluvia_acum_horaria$`as.character(acum_diaria[2:length(acum_diaria)])`<- ymd_hms(Lluvia_acum_horaria$`as.character(acum_diaria[2:length(acum_diaria)])`)


colnames(Lluvia_acum_horaria)<- c("Date", "Lluvia_mm", "aport_mean", "nivel_mean")


#Añadimos la aportación y el resto de variables
x<- 96
aportacion_SMA<- SMA(DHI$`APORTACION (m3/s)`,x)
nivel_SMA<- SMA(DHI$`NIVEL EMBALSE (msnm)`, x)

aportacion_horaria<- aportacion_SMA[DHI$DATE%in%acum_diaria]
nivel_horario<- nivel_SMA[DHI$DATE%in%acum_diaria]

y<- 24*5
aportacion_mean_SMA<- SMA(prueba$aport_mean,y)
nivel_mean_SMA<- SMA(prueba$nivel_mean,y)
plot(aportacion_mean_SMA)



Tabla_DHI<- as.data.frame(cbind(Lluvia_acum_horaria, 
                                aportacion_horaria[2:length(aportacion_horaria)],
                                nivel_horario[2:length(nivel_horario)], 
                                aportacion_mean_SMA,
                                nivel_mean_SMA))



colnames(Tabla_DHI)<- c(names(Lluvia_acum_horaria), "aport_SMA", "nivel_SMA", "aport_mean_SMA",  "nivel_mean_SMA")
Tabla_DHI<- Tabla_DHI[complete.cases(Tabla_DHI),]

rango<- 3000:5000
plot(Tabla_DHI$aport_mean[rango], type = "l")
lines(Tabla_DHI$aport_SMA[rango], col= "red")

plot(Tabla_DHI$nivel_mean[rango], type = "l")
lines(Tabla_DHI$nivel_SMA[rango], col= "red")


plot(Tabla_DHI$aport_mean_SMA[rango], type = "l", ylim= c(50,500))
lines(Tabla_DHI$nivel_mean_SMA[rango], col= "red")

plot(Tabla_DHI$aport_mean[rango], type = "l", ylim= c(50,500))
lines(Tabla_DHI$nivel_mean[rango], col= "red")

plot(Tabla_DHI$aport_SMA[rango], type = "l", ylim= c(50,500))
lines(Tabla_DHI$nivel_SMA[rango], col= "red")


cor(Tabla_DHI$aport_mean_SMA[rango],
    Tabla_DHI$nivel_mean_SMA[rango])



ccf_aport_mean<- ccf(Tabla_DHI$aport_mean_SMA[rango],
    Tabla_DHI$nivel_mean_SMA[rango], lag.max = 1000)

ccf_aport_mean$lag[which.max(ccf_aport_mean$acf)]


cor(Tabla_DHI$aport_SMA[rango],
    Tabla_DHI$nivel_SMA[rango])



ccf_aport_mean<- ccf(Tabla_DHI$aport_SMA,
                     Tabla_DHI$nivel_SMA, lag.max = 10000)

max(ccf_aport_mean$acf)
ccf_aport_mean$lag[which.max(ccf_aport_mean$acf)]


# Compromar cual es la mejor SMA La conclusion que se saca es que para 5 dias de SMA se consigue la mejor Corr
------------------------------------------

for (n in 1:14 ) {
  y<- 24*n
  aportacion_mean_SMA<- SMA(prueba$aport_mean,y)
  nivel_mean_SMA<- SMA(prueba$nivel_mean,y)
  
  
  
  Tabla_DHI<- as.data.frame(cbind(Lluvia_acum_horaria, 
                                  aportacion_horaria[2:length(aportacion_horaria)],
                                  nivel_horario[2:length(nivel_horario)], 
                                  aportacion_mean_SMA,
                                  nivel_mean_SMA))
  
  
  
  colnames(Tabla_DHI)<- c(names(Lluvia_acum_horaria), "aport_SMA", "nivel_SMA", "aport_mean_SMA",  "nivel_mean_SMA")
  Tabla_DHI<- Tabla_DHI[complete.cases(Tabla_DHI),]
  

  ccf_aport_mean<- ccf(Tabla_DHI$aport_mean_SMA[rango],
                       Tabla_DHI$nivel_mean_SMA[rango], lag.max = 1000)
  
  print(paste0("Máxima correlacion de ", 
               round(max(ccf_aport_mean$acf), digits = 3),
               " .Para un desfase de: ",
        round(ccf_aport_mean$lag[which.max(ccf_aport_mean$acf)]/24, digits = 2),
        " días. Con n= ", n))
 
}

