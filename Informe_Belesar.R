library(here)
source(here::here('libraries.R'))


DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar.RDS'))

head(DHI)

plot(DHI$`APORTACION (m3/s)`,
     xlab = "Date",
     ylab= "Aportacion [m³/s]")

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


plot(DHI$`APORTACION (m3/s)`,
     xlab = "Date",
     ylab= "Aportacion [m³/s]",
     type = "l")


sum(is.na(DHI$`APORTACION (m3/s)`))


library(imputeTS)

#Rellenamos Huecos
DHI$`APORTACION (m3/s)`<-  na.interpolation(DHI$`APORTACION (m3/s)`)


#Comprobamos
sum(is.na(DHI$`APORTACION (m3/s)`))



library(TTR)

for(n in c(1,seq(48,192,length.out = 4))){
  Mavg<- SMA(DHI$`APORTACION (m3/s)`, n) 
  
  plot(DHI$DATE, Mavg,
       xlab = "Date",
       ylab= "Aportacion [m³/s]", 
       type = "l",
       main = paste0("Aportación en Belesar. MA= ", n))
}


outlierKD(DHI,`NIVEL EMBALSE (msnm)`)
DHI$`NIVEL EMBALSE (msnm)`<-  na.interpolation(DHI$`NIVEL EMBALSE (msnm)`)


x<- 96
aportacion_SMA<- SMA(DHI$`APORTACION (m3/s)`,x)
nivel_SMA<- SMA(c(0,diff(DHI$`NIVEL EMBALSE (msnm)`)*8000+150), x)

aportacion_SMA<- aportacion_SMA[!is.na(aportacion_SMA)]
nivel_SMA<- nivel_SMA[!is.na(nivel_SMA)]


plot(aportacion_SMA,type = "l",
     xlab =" " ,ylab = "",xaxt= 'n',yaxt='n',
     main=paste0("Variacion del nivel y aportacion\n Correlación de: ", 
                 round(cor(aportacion_SMA, nivel_SMA), 3)))
lines(nivel_SMA, col = "red")


ccf_belesar<- ccf(aportacion_SMA, nivel_SMA, lag.max = 5000)


#Máxima correlación
max(ccf_belesar$acf)


#Con cuanto desfase se produce la máxima correlación. 
ccf_belesar$lag[which.max(ccf_belesar$acf)]



#Retrasamos hacia detrás todos los datos de lluvia porque la acumulada de toda la hora la suma en la hora siguiente....
DHI$`LLUVIA ACUMULADA DÍA (l/m2)`<- lead(DHI$`LLUVIA ACUMULADA DÍA (l/m2)`)

Desacumular_lluvia_2018<- DHI[which(year(DHI$DATE)==2018),] %>% group_by(yday(DATE)) %>% mutate(desacumulada= c(diff(`LLUVIA ACUMULADA DÍA (l/m2)`),0),
                                                                         lluvia=ifelse(desacumulada>=0, desacumulada, 0))


Desacumular_lluvia_2019<- DHI[which(year(DHI$DATE)==2019),] %>% group_by(yday(DATE)) %>% mutate(desacumulada= c(diff(`LLUVIA ACUMULADA DÍA (l/m2)`),0),
                                                                              lluvia=ifelse(desacumulada>=0, desacumulada, 0))
Desacumular_lluvia<- rbind(Desacumular_lluvia_2018, Desacumular_lluvia_2019)


Medias_horarias_2018<- as.data.frame(Desacumular_lluvia[year(Desacumular_lluvia$DATE)==2018,]) %>% group_by(yday(DATE), hour(DATE))  %>% 
  summarize(., Acum_horaria=sum(lluvia, na.rm = T),
            aport_mean=mean(`APORTACION (m3/s)`, na.rm = T),
            nivel_mean=mean(`NIVEL EMBALSE (msnm)`, na.rm = T))

Medias_horarias_2019<- as.data.frame(Desacumular_lluvia[year(Desacumular_lluvia$DATE)==2019,]) %>% group_by(yday(DATE),hour(DATE))  %>% 
  summarize(., Acum_horaria=sum(lluvia, na.rm = T),
            aport_mean=mean(`APORTACION (m3/s)`, na.rm = T),
            nivel_mean=mean(`NIVEL EMBALSE (msnm)`, na.rm = T))

Medias_horarias<- rbind(Medias_horarias_2018, Medias_horarias_2019)
vector_Date<- seq(range(DHI$DATE)[1],range(DHI$DATE)[2],
                  by="hour")

Lluvia_acum_horaria<- as.data.frame(cbind(as.character(vector_Date[2:length(vector_Date)]), 
                                          Medias_horarias[,3:5]))



Lluvia_acum_horaria$`as.character(vector_Date[2:length(vector_Date)])`<- ymd_hms(Lluvia_acum_horaria$`as.character(vector_Date[2:length(vector_Date)])`)


colnames(Lluvia_acum_horaria)<- c("Date", "Lluvia_mm", "aport_mean", "nivel_mean")





for (n in seq(6,24*3, by=6)) {
  y<- n
  aportacion_mean_SMA<- SMA(Lluvia_acum_horaria$aport_mean,y)
  nivel_mean_SMA<- SMA(Lluvia_acum_horaria$nivel_mean,y)
  
  
  
  Tabla_DHI<- as.data.frame(cbind(Lluvia_acum_horaria, 
                                  
                                  aportacion_mean_SMA,
                                  nivel_mean_SMA))
  
  
  
  colnames(Tabla_DHI)<- c(names(Lluvia_acum_horaria), "aport_mean_SMA",  "nivel_mean_SMA")
  Tabla_DHI<- Tabla_DHI[complete.cases(Tabla_DHI),]
  
  
  
  ccf_aport_mean<- ccf(Tabla_DHI$aport_mean_SMA,
                       Tabla_DHI$nivel_mean_SMA, 
                       lag.max = 1000, 
                       plot = T)
  
  print(paste0("Máxima correlacion de ",
               round(max(ccf_aport_mean$acf), digits = 3) ,
               " .Para un desfase de: ",
               round(ccf_aport_mean$lag[which.max(ccf_aport_mean$acf)]/24, digits = 2), 
               " días. Con un SMA de :", 
               n, 
               " horas"))
  
}







#se selecciona 96 porque es la cantidad de datos que corresponden a 1 día. 
x<- 96
aportacion_SMA<- SMA(DHI$`APORTACION (m3/s)`,x)
nivel_SMA<- SMA(DHI$`NIVEL EMBALSE (msnm)`, x)

aportacion_horaria<- aportacion_SMA[DHI$DATE%in%vector_Date]
nivel_horario<- nivel_SMA[DHI$DATE%in%vector_Date]

#Se ponen 142 horas porque  se obtienen las mejoras correlaciones de esta manera
y<- 24
aportacion_mean_SMA<- SMA(Lluvia_acum_horaria$aport_mean,y)
nivel_mean_SMA<- SMA(Lluvia_acum_horaria$nivel_mean,y)





Tabla_DHI<- as.data.frame(cbind(Lluvia_acum_horaria, 
                                aportacion_horaria[2:length(aportacion_horaria)],
                                nivel_horario[2:length(nivel_horario)], 
                                aportacion_mean_SMA,
                                nivel_mean_SMA))



colnames(Tabla_DHI)<- c(names(Lluvia_acum_horaria), "aport_SMA", "nivel_SMA", 
                        "aport_mean_SMA",  "nivel_mean_SMA")

Tabla_DHI<- Tabla_DHI[complete.cases(Tabla_DHI),]






k<- max(Tabla_DHI$nivel_mean_SMA)/max(Tabla_DHI$aport_mean_SMA)

ggplot(data=Tabla_DHI, aes(x=Date))+
  geom_line(aes(y=aport_mean_SMA))+
  xlab("Date")+ylab("Aportación [m³/s]")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())+
  geom_line(aes(y=nivel_mean_SMA/k),group=1, color="red") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k), name = "Nivel [msnm]", 
                                         breaks = seq(min(Tabla_DHI$nivel_mean_SMA),
                                                      max(Tabla_DHI$nivel_mean_SMA),
                                                      by=10)),
                     breaks = seq(min(Tabla_DHI$aport_mean_SMA),
                                  max(Tabla_DHI$aport_mean_SMA),
                                  by=20)) + 
  ggtitle("Nivel y aportación. SMA (144 horas) de la media horaria")

ccf_DHI<- ccf(Tabla_DHI$aport_mean_SMA, 
              Tabla_DHI$nivel_mean_SMA, 
              lag.max = 1000)

print(paste0("Máxima correlacion de ",
             round(max(ccf_DHI$acf), digits = 3) ,
             " .Para un desfase de: ",
             round(ccf_DHI$lag[which.max(ccf_DHI$acf)]/24, digits = 2), 
             " días"))





k<- max(Tabla_DHI$nivel_SMA)/max(Tabla_DHI$aport_SMA)

ggplot(data=Tabla_DHI, aes(x=Date))+
  geom_line(aes(y=aport_SMA))+
  xlab("Date")+ylab("Aportación [m³/s]")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())+
  geom_line(aes(y=nivel_SMA/k),group=1, color="red") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k), name = "Nivel [msnm]", 
                                         breaks = seq(min(Tabla_DHI$nivel_SMA),
                                                      max(Tabla_DHI$nivel_SMA),
                                                      by=10)),
                     breaks = seq(min(Tabla_DHI$aport_SMA),
                                  max(Tabla_DHI$aport_SMA),
                                  by=20)) + 
  ggtitle("Nivel y aportación. SMA (96 datos = 1 día) de los datos 15-minutales")

ccf_DHI<- ccf(Tabla_DHI$aport_SMA, 
              Tabla_DHI$nivel_SMA, 
              lag.max = 4000)

print(paste0("Máxima correlacion de ",
             round(max(ccf_DHI$acf), digits = 3) ,
             " .Para un desfase de: ",
             round(ccf_DHI$lag[which.max(ccf_DHI$acf)]/24, digits = 2), 
             " días"))


k<- max(Tabla_DHI$nivel_mean)/max(Tabla_DHI$aport_mean)

ggplot(data=Tabla_DHI, aes(x=Date))+
  geom_line(aes(y=aport_mean))+
  xlab("Date")+ylab("Aportación [m³/s]")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())+
  geom_line(aes(y=nivel_mean/k),group=1, color="red") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k), name = "Nivel [msnm]", 
                                         breaks = seq(min(Tabla_DHI$nivel_mean),
                                                      max(Tabla_DHI$nivel_mean),
                                                      by=10)),
                     breaks = seq(min(Tabla_DHI$aport_mean),
                                  max(Tabla_DHI$aport_mean),
                                  by=20)) + 
  ggtitle("Nivel y aportación. Medias horarias")

ccf_DHI<- ccf(Tabla_DHI$aport_mean, 
              Tabla_DHI$nivel_mean, 
              lag.max = 1000)

print(paste0("Máxima correlacion de ",
             round(max(ccf_DHI$acf), digits = 3) ,
             " .Para un desfase de: ",
             round(ccf_DHI$lag[which.max(ccf_DHI$acf)]/24, digits = 2), 
             " días"))





k<- max(Tabla_DHI$Lluvia_mm)/max(Tabla_DHI$aport_mean_SMA)

ggplot(data=Tabla_DHI, aes(x=Date))+
  geom_line(aes(y=aport_mean_SMA))+
  xlab("Date")+ylab("Aportación [m³/s]")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())+
  geom_line(aes(y=Lluvia_mm/k),group=1, color="red") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k), name = "Nivel [msnm]", 
                                         breaks = seq(min(Tabla_DHI$Lluvia_mm),
                                                      max(Tabla_DHI$Lluvia_mm),
                                                      by=10)),
                     breaks = seq(min(Tabla_DHI$aport_mean_SMA),
                                  max(Tabla_DHI$aport_mean_SMA),
                                  by=20)) + 
  ggtitle("Lluvia y aportacion")

ccf_DHI<- ccf(Tabla_DHI$aport_mean_SMA, 
              Tabla_DHI$Lluvia_mm, 
              lag.max = 1000)

print(paste0("Máxima correlacion de ",
             round(max(ccf_DHI$acf), digits = 3) ,
             " .Para un desfase de: ",
             round(ccf_DHI$lag[which.max(ccf_DHI$acf)]/24, digits = 2), 
             " días"))



Tabla_DHI_cut<- Tabla_DHI[which(month(Tabla_DHI$Date)==12), ]
k<- max(Tabla_DHI_cut$Lluvia_mm)/max(Tabla_DHI_cut$aport_mean_SMA)

ggplot(data=Tabla_DHI_cut, aes(x=Date))+
  geom_line(aes(y=aport_mean_SMA))+
  xlab("Date")+ylab("Aportación [m³/s]")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())+
  geom_line(aes(y=Lluvia_mm/k),group=1, color="red") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k), name = "Nivel [msnm]", 
                                         breaks = seq(min(Tabla_DHI_cut$Lluvia_mm),
                                                      max(Tabla_DHI_cut$Lluvia_mm),
                                                      by=10)),
                     breaks = seq(min(Tabla_DHI_cut$aport_mean_SMA),
                                  max(Tabla_DHI_cut$aport_mean_SMA),
                                  by=20)) + 
  ggtitle("Lluvia y aportacion. Diciembre")

ccf_DHI<- ccf(Tabla_DHI_cut$aport_mean_SMA, 
              Tabla_DHI_cut$Lluvia_mm, 
              lag.max = 1000)

print(paste0("Máxima correlacion de ",
             round(max(ccf_DHI$acf), digits = 3) ,
             " .Para un desfase de: ",
             round(ccf_DHI$lag[which.max(ccf_DHI$acf)]/24, digits = 2), 
             " días"))




path_dataDHI <- here::here('Data/Parques/Belesar/Historico/DHI_historico_afinado.RDS')

#sRDS(Tabla_DHI,path_dataDHI)




#Listamos archivos dentro de la carpeta de Belesar
All_files_Belesar<- list.files(here::here('Data/Parques/Belesar/'),
                               recursive = F, full.names = T)


#Detectamos cuales son RDS
RDS_Belesar<- All_files_Belesar[str_detect(All_files_Belesar, ".RDS")]

#Eliminamos los RDS que no son de WRF
RDS_Belesar1<- RDS_Belesar[!str_detect(RDS_Belesar, "E001")]

#Construimos Lista para cada instante de tiempo
Lista_localizacion<- list() 
for (i in 1:length(RDS_Belesar1)) {
  Belesar_data<- readRDS(RDS_Belesar1[i])
  Belesar_lolat<- lon_lat_df_ls(Belesar_data)
  Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
  Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data2)
  Lista_localizacion[[i]]<- Belesar_rain
}

#Nombramos la lista
names_fechas<- sapply(RDS_Belesar1, function(x){
  r<- str_split(x, "/")
  return(str_remove(str_remove(r[[1]][length(r[[1]])], ".RDS"), "Belesar_"))
})
names(Lista_localizacion)<- names_fechas


#Creamos una lista por localización
Lista_localizacion2<- list()
for (i in 1:length(Lista_localizacion[[1]])) {
  Lista_localizacion2[[i]]<- lapply(Lista_localizacion, 
                                    function(x) return(x[[i]]))
}

#Nombramos la lista
names(Lista_localizacion2)<- names(Lista_localizacion[[1]])

#Guardamos la lista
path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_WRF_Belesar_Variables.RDS')
#sRDS(Lista_localizacion2, nombre_lista)

#Cargamos lista
path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_WRF_Belesar_Variables.RDS')
Lista_total1<- readRDS(nombre_lista)

#Juntamos todos los Dataframes
Lista_total_MF<- lapply(Lista_total1, function(x) bind_rows(x))

#creamos dos data.frames... uno para D1 y otro para D2
Lista_d1_d2_loc<- list()
for (i in 1:length(Lista_total_MF)) {
  p<- Lista_total_MF[[i]]
  d1<- p[duplicated(p$fechas),]
  d1<-d1[!duplicated(d1$fechas),]
  d2<- p[!duplicated(p$fechas),]
  
  d2_qneed1<-d2[!(d2$fechas%in%d1$fechas),]
  
  
  d1_2<-bind_rows(d1,d2_qneed1)
  d1_2<-d1_2[order(d1_2$fechas),]
  
  d2<-d2[order(d2$fechas),]
  
  d1_2$pre_acum<- NULL
  d2$pre_acum<- NULL
  
  colnames(d1_2)<- c("Date", "LON", "LAT", "RAINC", "RAINNC","RAINSH", "T02_MEAN","PSFC","WS_MAX", "prep_hourly")
  
  colnames(d2)<-  c("Date", "LON", "LAT", "RAINC", "RAINNC","RAINSH", "T02_MEAN","PSFC","WS_MAX", "prep_hourly")
  
  lista_loc_d12<- list(d1_2,d2)
  names(lista_loc_d12)<- c("D1", "D2")
  
  Lista_d1_d2_loc[[i]]<- lista_loc_d12
}

#nombramos la lsta
names(Lista_d1_d2_loc)<- names(Lista_total_MF)

Tabla_periodo1<- Return_periodo_Belesar()
colnames(Tabla_periodo1)<- c("Date", "LON", "LAT", "RAINC", "RAINNC","RAINSH", "T02_MEAN","PSFC","WS_MAX", "prep_hourly")

Lista_d1_d2_loc2<- list()
for (j in 1:length(Lista_d1_d2_loc)) {
  prueba_list<- Lista_d1_d2_loc[[j]]
  lista_retorno<- list()
  for(i in 1:2){
    prueba<- prueba_list[[i]]
    Tabla_periodo<- Tabla_periodo1
    Tabla_periodo$LON[match(prueba$Date,Tabla_periodo$Date)] <- prueba$LON
    Tabla_periodo$LAT[match(prueba$Date,Tabla_periodo$Date)] <- prueba$LAT
    Tabla_periodo$RAINC[match(prueba$Date,Tabla_periodo$Date)] <- prueba$RAINC
    Tabla_periodo$RAINNC[match(prueba$Date,Tabla_periodo$Date)] <- prueba$RAINNC
    Tabla_periodo$RAINSH[match(prueba$Date,Tabla_periodo$Date)] <- prueba$RAINSH
    Tabla_periodo$T02_MEAN[match(prueba$Date,Tabla_periodo$Date)] <- prueba$T02_MEAN
    Tabla_periodo$PSFC[match(prueba$Date,Tabla_periodo$Date)] <- prueba$PSFC
    Tabla_periodo$WS_MAX[match(prueba$Date,Tabla_periodo$Date)] <- prueba$WS_MAX
    Tabla_periodo$prep_hourly[match(prueba$Date,Tabla_periodo$Date)] <- prueba$prep_hourly
    lista_retorno[[i]]<- Tabla_periodo
  }
  names(lista_retorno)<- c("D1", "D2")
  Lista_d1_d2_loc2[[j]]<- lista_retorno 
  
  
}
names(Lista_d1_d2_loc2)<- names(Lista_d1_d2_loc)

#Creamos lista con las variables afinadas
Lista_d1_d2_loc3<- lapply(Lista_d1_d2_loc2, function(x){
  x[[1]]$RAINC<- NULL
  x[[1]]$RAINNC<- NULL
  x[[1]]$RAINSH<- NULL
  x[[2]]$RAINC<- NULL
  x[[2]]$RAINNC<- NULL
  x[[2]]$RAINSH<- NULL
  
  return(x)})

names(Lista_d1_d2_loc3)<- names(Lista_d1_d2_loc2)

#Guardamos
path_hist_WRF<- here::here('Data/Parques/Belesar/Historico/Historico_WRF_Belesar_Variables_D1D2.RDS')
#saveRDS(Lista_d1_d2_loc3,path_hist_WRF)




#Usamos dplyr para juntar ambos datasets

Belesar_WRF<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_WRF_Belesar_Variables_D1D2.RDS'))
Belesar_DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/DHI_historico_afinado.RDS'))

df2<- Belesar_DHI

Belesar_Merge<- list()
for (j in 1:length(Belesar_WRF)) {
  lista_retorno<- list()
  for(i in 1:2){
    df1<-  Belesar_WRF[[j]][[i]]
    Merge_table<- left_join(df1, df2, by=c("Date"))
    lista_retorno[[i]]<- Merge_table
  }
  names(lista_retorno)<- c("D1", "D2")
  Belesar_Merge[[j]]<- lista_retorno 
}
names(Belesar_Merge)<- names(Belesar_WRF)





#Belesar merge completecases
Belesar_Merge_cc<- list()
for (j in 1:length(Belesar_Merge)) {
  lista_retorno<- list()
  for(i in 1:2){
    df1<-  Belesar_Merge[[j]][[i]]
    df1$Acumulated<- NULL
    Table_fine<- df1[complete.cases(df1),]
    lista_retorno[[i]]<- Table_fine
  }
  names(lista_retorno)<- c("D1", "D2")
  Belesar_Merge_cc[[j]]<- lista_retorno 
}
names(Belesar_Merge_cc)<- names(Belesar_Merge)



#Guardamos
path_hist_WRF<- here::here('Data/Parques/Belesar/Historico/Hist_D1D2_DHI_MERGED.RDS')
#saveRDS(Belesar_Merge_cc,path_hist_WRF)




## Probamos diferentes regresiones lineales



#importar
path_hist_WRF<- here::here('Data/Parques/Belesar/Historico/Hist_D1D2_DHI_MERGED.RDS')
clean_data<- readRDS(path_hist_WRF)


#cortar en entrenamiento y predicción
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
    
    fit_1  <- lm(Lluvia_mm  ~ prep_hourly, data = data_predict)
    fit_2  <- lm(Lluvia_mm  ~ prep_hourly + T02_MEAN, data = data_predict)
    fit_3  <- lm(Lluvia_mm  ~ prep_hourly + PSFC, data = data_predict)
    fit_4  <- lm(Lluvia_mm  ~ prep_hourly + WS_MAX, data = data_predict)
    fit_5  <- lm(Lluvia_mm  ~ prep_hourly * T02_MEAN, data = data_predict)
    fit_6  <- lm(Lluvia_mm  ~ prep_hourly * PSFC, data = data_predict)
    fit_7  <- lm(Lluvia_mm  ~ prep_hourly * WS_MAX, data = data_predict)
    
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
    
    observed_rain<-data_predict2$Lluvia_mm 
    
    
    
    tabla_cor<-cbind(cor(uncorrected, observed_rain),
                     cor(prediction_rain, observed_rain),
                     cor(prediction_rain2, observed_rain),
                     cor(prediction_rain3, observed_rain),
                     cor(prediction_rain4, observed_rain),
                     cor(prediction_rain5, observed_rain),
                     cor(prediction_rain6, observed_rain),
                     cor(prediction_rain7, observed_rain))
    
    fit_1  <- svm(Lluvia_mm  ~ prep_hourly, data = data_predict)
    fit_2  <- svm(Lluvia_mm  ~ prep_hourly + T02_MEAN, data = data_predict)
    fit_3  <- svm(Lluvia_mm  ~ prep_hourly + PSFC, data = data_predict)
    fit_4  <- svm(Lluvia_mm  ~ prep_hourly + WS_MAX, data = data_predict)
    fit_5  <- svm(Lluvia_mm  ~ prep_hourly * T02_MEAN, data = data_predict)
    fit_6  <- svm(Lluvia_mm  ~ prep_hourly * PSFC, data = data_predict)
    fit_7  <- svm(Lluvia_mm  ~ prep_hourly * WS_MAX, data = data_predict)
    
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
    
    observed_rain<-data_predict2$Lluvia_mm 
    
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

cor_table$LON<-round(as.numeric(cor_table$LON), digits = 2)
cor_table$LAT<-round(as.numeric(cor_table$LAT), digits = 2)
cor_table$Corr<-round(as.numeric(cor_table$Corr), digits = 3)

cor_table

clean_data_oct<-lapply(clean_data, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2018/10/01")),])
})



#Plot_rain2 necesita columna Date y columna rain
plot_rain3<- function(data,data2, titulo){
  ggplot(data=data, aes(x=Date))+
    geom_line(aes(y=rain), stat="identity")+
    xlab("Date")+ylab("Lluvia por hora [mm/h]")+theme(panel.background = element_blank(), 
                                                      panel.grid = element_blank()) +
    geom_line(data= data2, aes(x=Date, y=rain), color="red", alpha=0.5)+
    geom_text(aes(as.POSIXct(ymd("2019/02/01")),10, label=paste0("Cor: ",round(cor(data$rain,data2$rain), 
                                                                               digits = 2)))) +
    ggtitle(titulo)
  
}


for (i in 1:length(clean_data_oct)) {
  data1<- as.data.frame(cbind(as.character(clean_data_oct[[i]][[1]]$Date),
                              clean_data_oct[[i]][[1]]$prep_hourly))
  
  names(data1)<- c("Date", "rain")
  data1$Date<- ymd_hms(data1$Date)
  data1$rain<- as.numeric(as.character(data1$rain))
  
  
  
  data2<- as.data.frame(cbind(as.character(clean_data_oct[[i]][[1]]$Date),
                              clean_data_oct[[i]][[1]]$Lluvia_mm))
  
  names(data2)<- c("Date", "rain")
  data2$Date<- ymd_hms(data2$Date)
  data2$rain<- as.numeric(as.character(data2$rain))
  
  
  plot_rain3(data = data1 ,
             data2 = data2,
             titulo = paste(str_split(names(clean_data_oct)[i], "_")[[1]], collapse = " "))
  
  
  
  
  
}

Cor_rain_place<- data.frame(matrix(ncol = 4))
colnames(Cor_rain_place)<- c("LON", "LAT", "Corr", "Dist")
for (i in 1:length(clean_data_oct)) {
  Corr<- cor(clean_data_oct[[i]][[1]]$prep_hourly, 
      clean_data_oct[[i]][[1]]$Lluvia_mm)
  LON<- as.numeric(str_split(names(clean_data_oct)[i], "_")[[1]][1])
  LAT<- as.numeric(str_split(names(clean_data_oct)[i], "_")[[1]][3])
  Dist<- distm(c(-7.713948, 42.628577), 
                      c(as.numeric(LON),
                        as.numeric(LAT)),
                      fun = distHaversine)
  Cor_rain_place[i,]<- as.data.frame(cbind(LON, LAT, Corr, Dist))
  
  
}

Cor_order<- Cor_rain_place[order(Cor_rain_place$Corr, decreasing = T),]



clean_data_jan<-lapply(clean_data_oct, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2019/01/01")),])
})

for (i in 1:length(clean_data_jan)) {
  data1<- as.data.frame(cbind(as.character(clean_data_jan[[i]][[1]]$Date),
                              clean_data_jan[[i]][[1]]$prep_hourly))
  
  names(data1)<- c("Date", "rain")
  data1$Date<- ymd_hms(data1$Date)
  data1$rain<- as.numeric(as.character(data1$rain))
  
  
  
  data2<- as.data.frame(cbind(as.character(clean_data_jan[[i]][[1]]$Date),
                              clean_data_jan[[i]][[1]]$Lluvia_mm))
  
  names(data2)<- c("Date", "rain")
  data2$Date<- ymd_hms(data2$Date)
  data2$rain<- as.numeric(as.character(data2$rain))
  
  
  x<- plot_rain3(data = data1 ,
             data2 = data2,
             titulo = paste(str_split(names(clean_data_jan)[i], "_")[[1]], collapse = " "))
  
  print(x)
  
  
  
}


clean_data_jan<-lapply(clean_data_oct, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2019/01/01")),])
})
Cor_rain_place<- data.frame(matrix(ncol = 4))
colnames(Cor_rain_place)<- c("LON", "LAT", "Corr", "Dist")
for (i in 1:length(clean_data_jan)) {
  Corr<- cor(clean_data_jan[[i]][[1]]$prep_hourly, 
             clean_data_jan[[i]][[1]]$Lluvia_mm)
  LON<- as.numeric(str_split(names(clean_data_jan)[i], "_")[[1]][1])
  LAT<- as.numeric(str_split(names(clean_data_jan)[i], "_")[[1]][3])
  Dist<- distm(c(-7.713948, 42.628577), 
               c(as.numeric(LON),
                 as.numeric(LAT)),
               fun = distHaversine)
  Cor_rain_place[i,]<- as.data.frame(cbind(LON, LAT, Corr, Dist))
  
  
}

Cor_order<- Cor_rain_place[order(Cor_rain_place$Corr, decreasing = T),]


### Correlación data desde diciembre
clean_data_dec<-lapply(clean_data, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2018/12/01")),])
})
Daiyli_avgs<- lapply(clean_data_dec, function(x){
  x[[1]] %>% group_by(year(Date), yday(Date)) %>%  
    summarize(rWRF_acum= sum(prep_hourly),
              rDHI_acum= sum(Lluvia_mm),
              rWRF_mean= sum(prep_hourly),
              rDHI_mean= sum(Lluvia_mm),
              aport_SMA_diario=mean(aport_SMA),
              aport_mean_diario=mean(aport_mean),
              aport_mean_SMA_diario=mean(aport_mean_SMA),
              nivel_SMA_diario=mean(nivel_SMA),
              nivel_mean_diario=mean(nivel_mean),
              nivel_mean_SMA_diario=mean(nivel_mean_SMA))

  
  })

Daiyli_avgs2<- lapply(Daiyli_avgs, function(x){
  
  x$Date<- ymd(ifelse(x$`year(Date)`==2018,
                      as.character(as.Date(x$`yday(Date)`, origin= "2018-01-01")),
                      as.character(as.Date(x$`yday(Date)`, origin= "2019-01-01"))))
  x$`year(Date)`<- NULL
  x$`yday(Date)`<- NULL
  return(x)
})

Cor_rain_place<- data.frame(matrix(ncol = 11))
for (i in 1:length(Daiyli_avgs2)) {
  Corr1<- cor(Daiyli_avgs2[[i]]$rWRF_acum, 
             Daiyli_avgs2[[i]]$aport_SMA_diario)
  Corr2<- cor(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_mean_diario)
  Corr3<- cor(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_mean_SMA_diario)
  Corr4<- cor(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$rDHI_acum)
  
  Corr5<- cor(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_SMA_diario)
  Corr6<- cor(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_mean_diario)
  Corr7<- cor(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_mean_SMA_diario)
  Corr8<- cor(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$rDHI_mean)
  
  LON<- as.numeric(str_split(names(Daiyli_avgs2)[i], "_")[[1]][1])
  LAT<- as.numeric(str_split(names(Daiyli_avgs2)[i], "_")[[1]][3])
  Dist<- distm(c(-7.713948, 42.628577), 
               c(as.numeric(LON),
                 as.numeric(LAT)),
               fun = distHaversine)
  Cor_rain_place[i,]<- as.data.frame(cbind(LON, LAT, Corr1,
                                           Corr2,Corr3,Corr4,Corr5,
                                           Corr6,Corr7,Corr8, Dist))
  
  
}

colnames(Cor_rain_place)<- c("LON","LAT","AWRF_aport1","AWRF_aport2", "AWRF_aport3","AWRF_ADHI",
         "MWRF_aport1","MWRF_aport2", "MWRF_aport3","MWRF_MDHI","Dist")



Cor_rain_place_ccf<- data.frame(matrix(ncol = 11))
for (i in 1:length(Daiyli_avgs2)) {
  ccfr1<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_SMA_diario)
  ccfr2<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_mean_diario)
  ccfr3<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_mean_SMA_diario)
  ccfr4<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$rDHI_acum)
  
  ccfr5<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_SMA_diario)
  ccfr6<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_mean_diario)
  ccfr7<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_mean_SMA_diario)
  ccfr8<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$rDHI_mean)
  
  LON<- as.numeric(str_split(names(Daiyli_avgs2)[i], "_")[[1]][1])
  LAT<- as.numeric(str_split(names(Daiyli_avgs2)[i], "_")[[1]][3])
  Dist<- distm(c(-7.713948, 42.628577), 
               c(as.numeric(LON),
                 as.numeric(LAT)),
               fun = distHaversine)
  Cor_rain_place_ccf[i,]<- as.data.frame(cbind(LON, LAT, max(ccfr1$acf),
                                           max(ccfr2$acf),max(ccfr3$acf),max(ccfr4$acf),max(ccfr5$acf),
                                           max(ccfr6$acf),max(ccfr7$acf),max(ccfr8$acf), Dist))
  
  print(paste("Mejor corr para un retardo de: ", 
              ccfr1$lag[which.max(ccfr1$acf)],
              ccfr2$lag[which.max(ccfr2$acf)],
              ccfr3$lag[which.max(ccfr3$acf)],
              ccfr4$lag[which.max(ccfr4$acf)],
              ccfr5$lag[which.max(ccfr5$acf)],
              ccfr6$lag[which.max(ccfr6$acf)],
              ccfr7$lag[which.max(ccfr7$acf)],
              ccfr8$lag[which.max(ccfr8$acf)], sep = " "))
  
}

colnames(Cor_rain_place_ccf)<-  c("LON","LAT","AWRF_aport1","AWRF_aport2", "AWRF_aport3","AWRF_ADHI",
                                  "MWRF_aport1","MWRF_aport2", "MWRF_aport3","MWRF_MDHI","Dist")


View(Cor_rain_place_ccf[order(as.numeric(Cor_rain_place_ccf$Dist)),])




Cor_rain_place_ccf<- data.frame(matrix(ncol = 11))
for (i in 1:length(Daiyli_avgs2)) {
  ccfr1<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_SMA_diario, plot = FALSE)
  ccfr2<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_mean_diario, plot = FALSE)
  ccfr3<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$aport_mean_SMA_diario, plot = FALSE)
  ccfr4<- ccf(Daiyli_avgs2[[i]]$rWRF_acum, 
              Daiyli_avgs2[[i]]$rDHI_acum, plot = FALSE)
  
  ccfr5<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_SMA_diario, plot = FALSE)
  ccfr6<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_mean_diario, plot = FALSE)
  ccfr7<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$aport_mean_SMA_diario, plot = FALSE)
  ccfr8<- ccf(Daiyli_avgs2[[i]]$rWRF_mean, 
              Daiyli_avgs2[[i]]$rDHI_mean, plot = FALSE)
  
  LON<- as.numeric(str_split(names(Daiyli_avgs2)[i], "_")[[1]][1])
  LAT<- as.numeric(str_split(names(Daiyli_avgs2)[i], "_")[[1]][3])
  Dist<- distm(c(-7.713948, 42.628577), 
               c(as.numeric(LON),
                 as.numeric(LAT)),
               fun = distHaversine)
  
  
  ccf1<- paste(round(max(ccfr1$acf), digits = 2), ccfr1$lag[which.max(ccfr1$acf)], sep = " ")
  ccf2<- paste(round(max(ccfr2$acf), digits = 2), ccfr1$lag[which.max(ccfr2$acf)], sep = " ")
  ccf3<- paste(round(max(ccfr3$acf), digits = 2), ccfr1$lag[which.max(ccfr3$acf)], sep = " ")
  ccf4<- paste(round(max(ccfr4$acf), digits = 2), ccfr1$lag[which.max(ccfr4$acf)], sep = " ")
  ccf5<- paste(round(max(ccfr5$acf), digits = 2), ccfr1$lag[which.max(ccfr5$acf)], sep = " ")
  ccf6<- paste(round(max(ccfr6$acf), digits = 2), ccfr1$lag[which.max(ccfr6$acf)], sep = " ")
  ccf7<- paste(round(max(ccfr7$acf), digits = 2), ccfr1$lag[which.max(ccfr7$acf)], sep = " ")
  ccf8<- paste(round(max(ccfr8$acf), digits = 2), ccfr1$lag[which.max(ccfr8$acf)], sep = " ")
  
  Cor_rain_place_ccf[i,]<- as.data.frame(cbind(LON, LAT, ccf1,ccf2, 
                                               ccf3,ccf4,ccf5,ccf6,
                                               ccf7,ccf8, Dist))
  
  
}

colnames(Cor_rain_place_ccf)<-  c("LON","LAT","AWRF_aport1",
                                  "AWRF_aport2", "AWRF_aport3",
                                  "AWRF_ADHI","MWRF_aport1",
                                  "MWRF_aport2","MWRF_aport3",
                                  "MWRF_MDHI","Dist")


Corr_mean<- colMeans(Cor_rain_place_ccf[3:(length(Cor_rain_place_ccf)-1)])
colMax <- function(data) sapply(data, max, na.rm = TRUE)

colMax(Cor_rain_place_ccf)[3:(length(Cor_rain_place_ccf)-1)]
# Prediction aportacion  --------------------------------------------------
#cortar en entrenamiento y predicción
clean_data_dec<-lapply(clean_data, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2018/12/01")),])
})

SMA_n<- 48

cut_train<- lapply(clean_data_dec, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2018/12/01")
    fecha_end<- ymd("2019/01/25")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_train<- lapply(cut_train, function(x){
  y<- lapply(x, function(r){
    
    r$aport_SMA1<- lead(r$aport_SMA, SMA_n)
    r<- r[complete.cases(r),]
    return(r)
  }
  
  
  )
})

cut_predict<- lapply(clean_data_dec, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/01/26")
    fecha_end<- ymd("2019/02/20")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_predict<- lapply(cut_predict, function(x){
  y<- lapply(x, function(r){
   
    r$aport_SMA1<- lead(r$aport_SMA, SMA_n)
    r<- r[complete.cases(r),]
    r<- r[1:SMA_n, ]
    return(r)
  }
    
    
    )
})


lista_TL<- list()
for (i in 1:length(cut_train)) {
  lista_d1d2<- list()
  for (j in 1:2) {
    data_predict<- cut_train[[i]][[j]]
    data_predict2<- cut_predict[[i]][[j]]
    
    fit_1  <- svm(aport_SMA  ~ prep_hourly , data = data_predict)
    fit_2  <- svm(aport_SMA  ~ prep_hourly + aport_SMA1, data = data_predict)
    fit_3  <- svm(aport_SMA  ~ prep_hourly * aport_SMA1, data = data_predict)
    
    
    uncorrected<-data_predict2$prep_hourly
    prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
    prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                 aport_SMA1 =data_predict2$aport_SMA1))
    
    prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                 aport_SMA1 =data_predict2$aport_SMA1))
  
    
    observed_rain<-data_predict2$aport_SMA 
    
    
    
    tabla_cor<-cbind(cor(uncorrected, observed_rain,use="complete.obs"),
                     cor(prediction_rain, observed_rain,use="complete.obs"),
                     cor(prediction_rain2, observed_rain,use="complete.obs"),
                     cor(prediction_rain3, observed_rain,use="complete.obs"))
    
   
    
    colnames(tabla_cor)<- c("uncorrected", "1","2", "3")
    lista_d1d2[[j]]<- tabla_cor
  }
  
  names(lista_d1d2)<- c("D1", "D2")
  lista_TL[[i]]<- lista_d1d2
}





Lista_TL_names<-lapply(lista_TL, function(x){
  y<- lapply(x, function(r) {
    r<- as.data.frame(r)
    names(r)<- c("uncorrected", "1","2", "3")
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

punto_Belesar<- c(42.628577,-7.713948)
extract_lat_lon<- lapply(str_split(cor_table$id,"__"), function(x){
  return(as.data.frame(cbind(x[[1]],x[[2]])))
})

extract_lat_lon<- as.data.frame(bind_rows(extract_lat_lon))


cor_table<- as.data.frame(cbind(extract_lat_lon, cor_table[,2:3]))
colnames(cor_table)<- c("LON","LAT", "Corr", "Method")

LON<- cor_table$LON[which.max(cor_table$Corr)]
LAT<- cor_table$LAT[which.max(cor_table$Corr)]
method_max<- cor_table$Method[which.max(cor_table$Corr)] %>% str_detect(.,"D1") 

print(paste0("MAX Corr: ", max(cor_table$Corr)))

name_max<- paste0(LON,"__",LAT)



cut_predict<- lapply(clean_data_dec, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/01/26")
    fecha_end<- ymd("2019/02/20")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})


cut_predict<- lapply(cut_predict, function(x){
  y<- lapply(x, function(r){
    r$aport_SMA1<- lead(r$aport_SMA, SMA_n)
    r<- r[complete.cases(r),]
    print(range(r$Date))
    return(r)
  })
})

if(method_max==TRUE){
  data_predict<- cut_train[[which(names(cut_train)==name_max)]]$D1
  data_predict2<- cut_predict[[which(names(cut_train)==name_max)]]$D1
}else{
  data_predict<- cut_train[[which(names(cut_train)==name_max)]]$D2
  data_predict2<- cut_predict[[which(names(cut_train)==name_max)]]$D2
}


fit_1  <- svm(aport_mean_SMA  ~ prep_hourly , data = data_predict)
fit_2  <- lm(aport_mean_SMA  ~ a*(prep_hourly + b*aport_SMA1), data = data_predict, )
fit_3  <- svm(aport_mean_SMA  ~ prep_hourly * aport_SMA1, data = data_predict)


uncorrected<-data_predict2$prep_hourly
prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             aport_SMA1 =data_predict2$aport_SMA1))

prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             aport_SMA1 =data_predict2$aport_SMA1))

observed_rain<-data_predict2$aport_mean_SMA 


tabla_plot<-as.data.frame( cbind(data_predict2$Date,
                                 observed_rain, uncorrected, 
                                 prediction_rain,
                                 prediction_rain2,
                                 prediction_rain3))
colnames(tabla_plot)<- c("Date", "Observed", "Rain", 
                         "Rain_aport1",
                         "Rain_aport2",
                         "Rain_aport3")

ggplot(data=tabla_plot, aes(x=Date))+
  geom_line(aes(y=c(Observed)), colour="red")+
  geom_line(aes(y=c(Rain*50)), colour="blue")+
  geom_line(aes(y=c(Rain_aport1)), colour="green")+
  geom_line(aes(y=c(Rain_aport2)), colour="yellow")+
  geom_line(aes(y=c(Rain_aport3)), colour="orange")








# Prediccion nivel --------------------------------------------------------



clean_data_dec<-lapply(clean_data, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2018/12/01")),])
})

SMA_n<- 48

cut_train<- lapply(clean_data_dec, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2018/12/01")
    fecha_end<- ymd("2019/01/25")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_train<- lapply(cut_train, function(x){
  y<- lapply(x, function(r){
    
    r$nivel_mean_SMA1<- lead(r$nivel_mean_SMA, SMA_n)
    r<- r[complete.cases(r),]
    return(r)
  }
  
  
  )
})

cut_predict<- lapply(clean_data_dec, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/01/26")
    fecha_end<- ymd("2019/02/20")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_predict<- lapply(cut_predict, function(x){
  y<- lapply(x, function(r){
    
    r$nivel_mean_SMA1<- lead(r$nivel_mean_SMA, SMA_n)
    r<- r[complete.cases(r),]
    r<- r[1:SMA_n, ]
    return(r)
  }
  
  
  )
})


lista_TL<- list()
for (i in 1:length(cut_train)) {
  lista_d1d2<- list()
  for (j in 1:2) {
    data_predict<- cut_train[[i]][[j]]
    data_predict2<- cut_predict[[i]][[j]]
    
    fit_1  <- svm(nivel_mean_SMA  ~ prep_hourly , data = data_predict)
    fit_2  <- svm(nivel_mean_SMA  ~ prep_hourly + nivel_mean_SMA1, data = data_predict)
    fit_3  <- svm(nivel_mean_SMA  ~ prep_hourly * nivel_mean_SMA1, data = data_predict)
    
    
    uncorrected<-data_predict2$prep_hourly
    prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
    prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                 nivel_mean_SMA1 =data_predict2$nivel_mean_SMA1))
    
    prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                                 nivel_mean_SMA1 =data_predict2$nivel_mean_SMA1))
    
    
    observed_rain<-data_predict2$nivel_mean_SMA 
    
    
    
    tabla_cor<-cbind(cor(uncorrected, observed_rain,use="complete.obs"),
                     cor(prediction_rain, observed_rain,use="complete.obs"),
                     cor(prediction_rain2, observed_rain,use="complete.obs"),
                     cor(prediction_rain3, observed_rain,use="complete.obs"))
    
    
    
    colnames(tabla_cor)<- c("uncorrected", "1","2", "3")
    lista_d1d2[[j]]<- tabla_cor
  }
  
  names(lista_d1d2)<- c("D1", "D2")
  lista_TL[[i]]<- lista_d1d2
}





Lista_TL_names<-lapply(lista_TL, function(x){
  y<- lapply(x, function(r) {
    r<- as.data.frame(r)
    names(r)<- c("uncorrected", "1","2", "3")
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

punto_Belesar<- c(42.628577,-7.713948)
extract_lat_lon<- lapply(str_split(cor_table$id,"__"), function(x){
  return(as.data.frame(cbind(x[[1]],x[[2]])))
})

extract_lat_lon<- as.data.frame(bind_rows(extract_lat_lon))


cor_table<- as.data.frame(cbind(extract_lat_lon, cor_table[,2:3]))
colnames(cor_table)<- c("LON","LAT", "Corr", "Method")

LON<- cor_table$LON[which.max(cor_table$Corr)]
LAT<- cor_table$LAT[which.max(cor_table$Corr)]
method_max<- cor_table$Method[which.max(cor_table$Corr)] %>% str_detect(.,"D1") 

print(paste0("MAX Corr: ", max(cor_table$Corr)))

name_max<- paste0(LON,"__",LAT)





SMA_n<- 48

cut_train<- lapply(clean_data_dec, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2018/12/01")
    fecha_end<- ymd("2019/01/25")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})
cut_train<- lapply(cut_train, function(x){
  y<- lapply(x, function(r){
    
    r$nivel_mean_SMA1<- lead(r$nivel_mean_SMA, SMA_n)
    r$prep_hourly1<- lead(r$prep_hourly, SMA_n/2)
    
    r<- r[complete.cases(r),]
    return(r)
  })
})
  
cut_predict<- lapply(clean_data_dec, function(x){
  y<- lapply(x, function(r){
    
    fecha_ini<- ymd("2019/01/26")
    fecha_end<- ymd("2019/02/25")
    Jan_data<- r[which(r$Date<fecha_end & r$Date>fecha_ini),]
    return(Jan_data)
    
  })
  return(y)
})

cut_predict<- lapply(cut_predict, function(x){
  y<- lapply(x, function(r){
    r$nivel_mean_SMA1<- lead(r$nivel_mean_SMA, SMA_n)
    r$prep_hourly1<- lead(r$prep_hourly, SMA_n/2)
    r<- r[complete.cases(r),]
    print(range(r$Date))
    return(r)
  })
})

if(method_max==TRUE){
  data_predict<- cut_train[[which(names(cut_train)==name_max)]]$D1
  data_predict2<- cut_predict[[which(names(cut_train)==name_max)]]$D1
}else{
  data_predict<- cut_train[[which(names(cut_train)==name_max)]]$D2
  data_predict2<- cut_predict[[which(names(cut_train)==name_max)]]$D2
}


fit_1  <- svm(nivel_mean_SMA  ~ prep_hourly , data = data_predict)
fit_2  <- svm(nivel_mean_SMA  ~ prep_hourly + nivel_mean_SMA1 + prep_hourly1, data = data_predict)
fit_3  <- svm(nivel_mean_SMA  ~ prep_hourly * nivel_mean_SMA1 + prep_hourly1, data = data_predict)


uncorrected<-data_predict2$prep_hourly
prediction_rain<- predict(fit_1, data.frame(prep_hourly =data_predict2$prep_hourly))
prediction_rain2<- predict(fit_2, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             nivel_mean_SMA1 =data_predict2$nivel_mean_SMA1,
                                             prep_hourly1=data_predict2$prep_hourly1))

prediction_rain3<- predict(fit_3, data.frame(prep_hourly =data_predict2$prep_hourly,
                                             nivel_mean_SMA1 =data_predict2$nivel_mean_SMA1,
                                             prep_hourly1=data_predict2$prep_hourly1))

observed_rain<-data_predict2$nivel_mean_SMA


tabla_plot<-as.data.frame( cbind(data_predict2$Date,
                                 observed_rain, uncorrected, 
                                 prediction_rain,
                                 prediction_rain2,
                                 prediction_rain3))
colnames(tabla_plot)<- c("Date", "Observed", "Rain", 
                         "Rain_aport1",
                         "Rain_aport2",
                         "Rain_aport3")

ggplot(data=tabla_plot, aes(x=Date))+
  geom_line(aes(y=c(Observed)), colour="red")+
  geom_line(aes(y=c(Rain*50)), colour="blue")+
  geom_line(aes(y=c(Rain_aport1)), colour="green")+
  geom_line(aes(y=c(Rain_aport2)), colour="yellow")+
  geom_line(aes(y=c(Rain_aport3)), colour="orange")





fit_3$coefficients




























# Parte sin incluir -------------------------------------------------------


data_predict<- cut_train$`-8.02328491210938__42.1343421936035`$D1
data_predict2<- cut_predict$`-8.02328491210938__42.1343421936035`$D1

fit_1  <- lm( ~ prep_hourly, data = data_predict)
fit_2  <- lm(Lluvia_mm  ~ prep_hourly + T02_MEAN, data = data_predict)
fit_3  <- lm(Lluvia_mm  ~ prep_hourly + PSFC, data = data_predict)
fit_4  <- lm(Lluvia_mm  ~ prep_hourly + WS_MAX, data = data_predict)
fit_5  <- lm(Lluvia_mm  ~ prep_hourly * T02_MEAN, data = data_predict)
fit_6  <- lm(Lluvia_mm  ~ prep_hourly * PSFC, data = data_predict)
fit_7  <- lm(Lluvia_mm  ~ prep_hourly * WS_MAX, data = data_predict)


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

observed_rain<-data_predict2$Lluvia_mm 


plot(prediction_rain, type = "l", ylim = c(0,5))
lines(observed_rain, col="red")
lines(uncorrected, col="green")
lines(prediction_rain2, col="blue")
lines(prediction_rain3, col="purple")
lines(prediction_rain4, col="yellow")

lines(prediction_rain5, col="blue", lty="dotted")
lines(prediction_rain6, col="purple",lty="dotted")
lines(prediction_rain7, col="yellow",lty="dotted")





svm_tune <- tune(svm, Lluvia_mm ~ prep_hourly,data = data_predict,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
)


best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, data_predict2) 

plot(best_mod_pred, type = "l")
lines(data_predict2$Lluvia_mm,
      col="red")












#Plot_rain2 necesita columna Date y columna rain
plot_rain2<- function(data,data2){
  ggplot(data=data, aes(x=Date))+
    geom_line(aes(y=rain), stat="identity")+
    xlab("Date")+ylab("Lluvia por hora [mm/h]")+theme(panel.background = element_blank(), 
                                                      panel.grid = element_blank()) +
    geom_line(data= data2, aes(x=Date, y=rain), color="red", alpha=0.5)+
    geom_text(aes(as.POSIXct(ymd("2019/02/01")),10, label=paste0("Cor: ",round(cor(data$rain,data2$rain), 
                                                                               digits = 2))))
  
}

data_predict<- cut_train$`-8.02328491210938__42.1343421936035`$D1
data_predict2<- cut_predict$`-8.02328491210938__42.1343421936035`$D1

data1<- as.data.frame(cbind(as.character(data_predict$Date),
                            data_predict$prep_hourly))

names(data1)<- c("Date", "rain")
data1$Date<- ymd_hms(data1$Date)
data1$rain<- as.numeric(as.character(data1$rain))
  


data2<- as.data.frame(cbind(as.character(data_predict$Date),
                            data_predict$Lluvia_mm))


names(data2)<- c("Date", "rain")
data2$Date<- ymd_hms(data2$Date)
data2$rain<- as.numeric(as.character(data2$rain))



plot_rain2(data = data1 ,
           data2 = data2)



for (i in 1:length(Belesar_Merge_cc)) {
  prueba<- Belesar_Merge_cc[[i]]$D1
  prueba_acum<- prueba %>% mutate(WRF_Acum= cumsum(prep_hourly),
                                  Observada_Acum=cumsum(Lluvia_mm))
  prueba_acum<- prueba_acum[prueba_acum$Date> ymd("2018/10/1"),]
  wrf<- prueba_acum[,c("Date","WRF_Acum", "prep_hourly")]
  obs<- prueba_acum[,c("Date","Observada_Acum", "Lluvia_mm")]
  
  colnames(wrf)<- c("Date","acum","rain")
  colnames(obs)<- c("Date","acum","rain")
  path_belesar<- here::here('graph/Belesar/')
  
  plot_rain2(wrf,obs)
  ggsave(filename =paste0(path_belesar,"/",names(Belesar_Merge_cc)[i],"_D1.png"),
         device = "png",
         dpi=200,
         width = 7,
         height = 7,
         units = "in")
}






# Prediccion 24/04 --------------------------------------------------------

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


x<- cut_train[[1]]
x$aport1<- lag(x$aport_mean_SMA, 24)
x$aport2<- lag(x$aport_mean_SMA, 48)
x$aport3<- lag(x$aport_mean_SMA, 72)
x$aport4<- lag(x$aport_mean_SMA, 96)
x$aport5<- lag(x$aport_mean_SMA, 120)

x$prep_hourly1<- lead(x$prep_hourly, 24)
x$prep_hourlySMA12<- SMA(x$prep_hourly, 12)
x$prep_hourlySMA24<- SMA(x$prep_hourly, 24)
x$prep_hourlySMA36<- SMA(x$prep_hourly, 36)
x$prep_hourlySMA48<- SMA(x$prep_hourly, 48)
x$prep_hourlySMA48_lag<- lag(x$prep_hourlySMA48, 24)

x<- x[complete.cases(x),]


y<- cut_predict[[1]]
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

#y<- y[which(y$Date> fecha_ini & y$Date< fecha_end), ]




fit_1  <- svm(aport_mean_SMA ~ prep_hourlySMA48_lag * prep_hourlySMA48 , data = x)
fit_2  <- svm(aport_mean_SMA  ~ prep_hourlySMA48_lag + aport3 +  prep_hourlySMA48, data = x)
fit_3  <- svm(aport_mean_SMA  ~ prep_hourlySMA48_lag * aport3 * prep_hourlySMA48, data = x)

fit_4  <- lm(aport_mean_SMA ~ prep_hourlySMA48_lag * prep_hourlySMA48, data = x)
fit_5  <- lm(aport_mean_SMA  ~ prep_hourlySMA48_lag + aport3 + prep_hourlySMA48, data = x)
fit_6  <- lm(aport_mean_SMA  ~ prep_hourlySMA48_lag * aport3 * prep_hourlySMA48, data = x)


uncorrected<-y$aport_mean_SMA
pred_aport1<- predict(fit_1, data.frame(prep_hourlySMA48_lag =y$prep_hourlySMA48_lag,
                                        prep_hourlySMA48 =y$prep_hourlySMA48))
pred_aport2<- predict(fit_2, data.frame(prep_hourlySMA48_lag =y$prep_hourlySMA48_lag,
                                        aport3= y$aport3,
                                        prep_hourlySMA48 =y$prep_hourlySMA48))
pred_aport3<- predict(fit_3, data.frame(prep_hourlySMA48_lag =y$prep_hourlySMA48_lag,
                                        aport3= y$aport3,
                                        prep_hourlySMA48 =y$prep_hourlySMA48))

pred_aport4<- predict(fit_4, data.frame(prep_hourlySMA48_lag =y$prep_hourlySMA48_lag,
                                        prep_hourlySMA48 =y$prep_hourlySMA48))
pred_aport5<- predict(fit_5, data.frame(prep_hourlySMA48_lag =y$prep_hourlySMA48_lag,
                                        aport3= y$aport3,
                                        prep_hourlySMA48 =y$prep_hourlySMA48))
pred_aport6<- predict(fit_6, data.frame(prep_hourlySMA48_lag =y$prep_hourlySMA48_lag,
                                        aport3= y$aport3,
                                        prep_hourlySMA48 =y$prep_hourlySMA48))


plot(uncorrected, x=y$Date, type = "l",
     ylim = c(0,400), 
     xlab = paste0(range(y$Date)),
     ylab = "Aportación [m³/s]")
lines(pred_aport1,x=y$Date, col="red")
lines(pred_aport2,x=y$Date, col="green")
lines(pred_aport3,x=y$Date,col="blue")
lines(pred_aport4,x=y$Date, col="red", lty=2)
lines(pred_aport5,x=y$Date, col="green", lty=2)
lines(pred_aport6,x=y$Date,col="blue", lty=2)




lines(SMA(pred_aport4,12),x=y$Date, col="red", lty=5)
lines(SMA(pred_aport5,12),x=y$Date, col="green", lty=5)
lines(SMA(pred_aport6,12),x=y$Date,col="blue", lty=5)










plot(x$aport_mean_SMA, type = "l", ylim = c(0, 200))
lines(x$prep_hourlySMA48*50, col="red")


x$aport_mean_SMA

  
xx<- x[which(x$Date> ymd("2018/12/10")),]  
  
plot(diff(xx$nivel_mean_SMA)*20,  ylim = c(-1.5,2), type = "l")
lines(xx$aport_mean_SMA/100-1, col="red")
