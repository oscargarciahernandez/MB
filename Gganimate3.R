library(gganimate)

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


clean_data_oct<-lapply(clean_data, function(x){
  y<- lapply(x, function(r) {
    r<- r[which(r$Date > ymd("2018/10/01")),]
    r$prep_hourly<- ifelse(r$prep_hourly < 0,0,r$prep_hourly) 
    return(r)
  })
})



#Plot_rain3 necesita columna Date y columna rain
plot_rain3<- function(data,data2, titulo){
  ggplot(data=data, aes(x=Date))+
    geom_line(aes(y=rain), stat="identity")+
    xlab("Date")+ylab("Hourly rainfall [mm/h]")+
    theme(panel.background = element_blank(),
          panel.grid = element_blank()) +
    geom_line(data= data2, 
              aes(x=Date, y=rain),
              color="red", 
              alpha=0.5)+
    geom_text(aes(as.POSIXct(ymd("2019/02/01")),10, 
                  label=paste0("Cor: ",
                               round(cor(data$rain,
                                         data2$rain),
                                     digits = 2)))) +
    ggtitle(titulo) + 
    theme(plot.title = element_text(hjust = 0.5))+ 
    transition_reveal(along = Date)
  
}

clean_data_jan<-lapply(clean_data_oct, function(x){
  y<- lapply(x, function(r) r[which(r$Date > ymd("2019/01/15") & r$Date < ymd("2019/02/10")),])
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

#Ploteamos los 5 puntos con mejor correlacion
i<- 1
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
               titulo ="Observed rain (red) vs WRF estimated rain (black)")


x + 


print(x)

titulo<- "gganimate :)"
  
ggplot(data=data1, aes(x=Date))+
  geom_line(aes(y=rain), stat="identity")+
  xlab("Date")+ylab("Hourly rainfall [mm/h]")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  geom_line(data= data2, 
            aes(x=Date, y=rain),
            color="red", 
            alpha=0.5)+
  geom_text(aes(as.POSIXct(ymd("2019/02/01")),10, 
                label=paste0("Cor: ",
                             round(cor(data1$rain,
                                       data2$rain),
                                   digits = 2)))) +
  ggtitle(titulo) + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  transition_reveal(along = Date)
