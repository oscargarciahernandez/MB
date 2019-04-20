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

DHI$`LLUVIA ACUMULADA DÍA (l/m2)`<- lead(DHI$`LLUVIA ACUMULADA DÍA (l/m2)`)

Prueba_desacumular<- DHI %>% group_by(yday(DATE), year(DATE)) %>% summarize(oye= sum(diff(`LLUVIA ACUMULADA DÍA (l/m2)`)))



View(DHI)
acum_diaria<- seq(range(DHI$DATE)[1],range(DHI$DATE)[2],
                  by="hour")
prueba<- DHI %>% group_by(hour(DATE), yday(DATE), year(DATE))  %>% summarize(., sum(desacumulada, na.rm = T))


Lluvia_acum_horaria<- as.data.frame(cbind(as.character(acum_diaria[2:length(acum_diaria)]), 
                                          prueba$`sum(desacumulada, na.rm = T)`))
Lluvia_acum_horaria$V1<- ymd_hms(Lluvia_acum_horaria$V1)


colnames(Lluvia_acum_horaria)<- c("Date", "Lluvia_mm")


#Añadimos la aportación y el resto de variables
x<- 96
aportacion_SMA<- SMA(DHI$`APORTACION (m3/s)`,x)
nivel_SMA<- SMA(DHI$`NIVEL EMBALSE (msnm)`, x)

aportacion_horaria<- aportacion_SMA[DHI$DATE%in%acum_diaria]
nivel_horario<- nivel_SMA[DHI$DATE%in%acum_diaria]


Tabla_DHI<- as.data.frame(cbind(Lluvia_acum_horaria, 
                                aportacion_horaria[2:length(aportacion_horaria)],
                                nivel_horario[2:length(nivel_horario)]))



colnames(Tabla_DHI)<- c("Date", "Lluvia_mm", "Aportacion_m3_s", "Nivel_msnm")


View(Tabla_DHI)

