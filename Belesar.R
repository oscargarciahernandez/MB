library(here)
source(here::here('libraries.R'))






# Generar CSV's de todas las localizacones y puntos concretos -------------



#Para generar todas los CSV's
#lapply(Belesar_files2, Belesar_CSV_Generator)

##Generar CSV Belesar
Belesar_CSV_Generator(Belesar_ultimo)



#Para generar todas los CSV's
#lapply(Belesar_files2, Belesar_CSV_Generator_Point,point = 19)
#Geenrar CSV_Belesar
Belesar_CSV_Generator_Point<- function(Belesar_ultimo, point){
  Belesar_data<- readRDS(Belesar_ultimo)
  Belesar_lolat<- lon_lat_df_ls(Belesar_data)
  Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
  Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)
  
  nombres<- names(Belesar_rain)
  precipitacion_horaria<- lapply(Belesar_rain, function(x) x$prep_hourly)
  tabla_precip<- data.frame(matrix(nrow = length(precipitacion_horaria$`-8.02328491210938__42.1343421936035`)))
  for (i in 1:length(precipitacion_horaria)) {
    tabla_precip<- as.data.frame(cbind(tabla_precip,precipitacion_horaria[[i]]))
  }
  
  tabla_precip[,1]<- Belesar_rain[[1]]$fechas
  
  colnames(tabla_precip)<- c("Date", nombres)
  path<- here::here('Data/Parques/Belesar/')
  nombre1<- str_split(Belesar_ultimo, "/")
  nombre2<- nombre1[[1]][length(nombre1[[1]])]
  nombre3<- str_remove(nombre2 ,".RDS")
  
  path_nombre<-paste0(path,nombre3,"_",point,".CSV")
  
  tabla_precipita_localizacion<- tabla_precip[,c(1,point)]
  
  
  write.table(tabla_precipita_localizacion, path_nombre, 
              sep = ";",
              dec = ".", 
              row.names = F,
              quote = F)
  
}





# Descargar Data_ Internet ------------------------------------------------


# Down_E001_Belesar()



# Leer historico DHI ------------------------------------------------------



historico<- read.csv(here::here('DHI_Datos Belesar.csv'), sep = ";")
historico1<- historico[3:length(historico$X),]

col__names<- as.vector(unlist(lapply(historico[1,], as.character)))
col__names[1]<- "DATE"

colnames(historico1)<- col__names

historico1$DATE<- dmy_hms(historico1$DATE)
historico2<-historico1[complete.cases(historico1),]

indx <- sapply(historico2, is.factor)
historico2[indx] <- lapply(historico2[indx], function(x) as.numeric(str_replace(as.character(x),",",".")))


path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_DHI_Belesar.RDS')
saveRDS(historico2, nombre_lista)



# Historico WRF -----------------------------------------------------------


solo_fechas<- grepl("^[[:digit:]]+$",
                    list.dirs("/media/asus/Elements",
                              recursive = F, 
                              full.names = F))
dirs_wrf<- list.dirs("/media/asus/Elements", recursive = F)[solo_fechas]

for (dirs in 1:length(dirs_wrf)) {
  
  netcdf_files<- list.files(dirs_wrf[dirs], full.names = T)
  netcdf_files_d1<- netcdf_files[str_detect(netcdf_files, "wrfout_d01" )]
  netcdf_files_d2<- netcdf_files[str_detect(netcdf_files, "wrfout_d02" )]
  
  folder_name<- first_date(list.files(dirs_wrf[dirs]))
  folder_spain<- str_remove_all(as.character(folder_name),"-")
  
  path_check<- paste0(here::here('Data/Espana/'),folder_spain,"/")
  if(dir.exists(path_check)){
    print(paste0(("Archivo ya almacenado: "),path_check ))
    
  }else{
    if(length(netcdf_files_d2)==0 && length(netcdf_files_d1)==0){}else{
      
      if(length(netcdf_files_d2)==0){
        netcdf_files2<- netcdf_files_d1
        path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/d01/")
        
        if(!dir.exists(path_espana)){
          dir.create(paste0(here::here('Data/Espana/'),folder_spain, "/"))
          dir.create(path_espana)}
        
        list_espana<- create_list_from_netcdf(netcdf_files = netcdf_files2)
        
        saveRDS(list_espana, file = paste0(path_espana,"Espana_",folder_spain,".RDS"))
        
        
      }else{
        netcdf_files2<- list(netcdf_files_d1, netcdf_files_d2)
        names(netcdf_files2)<- c("dom1","dom2")
        
        for (dom in 1:2) {
          
          if(dom==1){path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/d01/")
          }else{path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/d02/")}
          
          if(!dir.exists(paste0(here::here('Data/Espana/'),folder_spain, "/"))){dir.create(paste0(here::here('Data/Espana/'),folder_spain, "/"))}
          if(!dir.exists(path_espana)){dir.create(path_espana)}
          
          list_espana<- create_list_from_netcdf(netcdf_files = netcdf_files2[[dom]])
          
          saveRDS(list_espana, file = paste0(path_espana,"Espana_",folder_spain,".RDS"))
        }
        
      }
    }
    
    
    
  }
  
  
  
  
  

  
  
}


Actualizar_Historico_WRF_desdeElements()


x<- list.dirs(here::here('Data/Espana/'), recursive = T, full.names = T)
unique(x)
y<- list.files(here::here('Data/Espana/'), recursive = T, full.names = T)
y1<- str_split(y,"/") %>% lapply(., function(x) x[1:(length(x)-1)]) %>%  sapply(., function(x) paste(x, collapse = "/"))
y2<- unique(y1)
carpetas_vacias<- x[x%in%y2]



# Historico WRF de parques ------------------------------------------------
All_files_Spain<- list.files(here::here('Data/Espana/'),
                             recursive = T, 
                             full.names = T)

d01_files<- All_files_Spain[!str_detect(All_files_Spain, "/d02/")]
RDS_files<- d01_files[str_detect(d01_files, ".RDS")]
RDS_files1<- RDS_files[!str_detect(RDS_files, "/NA/")]





Actualizar_Data_Parques_2(RDS_files1)



# Belesar. Construir historico --------------------------------------------
All_files_Belesar<- list.files(here::here('Data/Parques/Belesar/'), 
                               recursive = F, full.names = T)
RDS_Belesar<- All_files_Belesar[str_detect(All_files_Belesar, ".RDS")]

RDS_Belesar1<- RDS_Belesar[!str_detect(RDS_Belesar, "E001")]

Lista_total<- list()
for (j in 1:length(Belesar_rain)) {
  Lista_localizacion<- list() 
  for (i in 1:length(RDS_Belesar)) {
    Belesar_data<- readRDS(RDS_Belesar1[i])
    Belesar_lolat<- lon_lat_df_ls(Belesar_data)
    Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
    Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)
    Lista_localizacion[[i]]<- Belesar_rain[[j]]
  }
  Lista_total[[j]]<- Lista_localizacion
}


names_fechas<- sapply(RDS_Belesar, function(x){
  r<- str_split(x, "/")
  return(r[length(r)])
})

names_loc<- names(Belesar_rain)

Lista_total2<- lapply(Lista_total, function(x) names(x)<- names_fechas)
names(Lista_total2)<- names_loc

names(Lista_total2)<- names_loc

path_lista_total<- here::here('Data/Parques/Belesar/Historico/')
nombre_lista<- paste0(path_lista_total, 'Historico_WRF_Belesar_BRUTO.RDS')
nombre_lista2<- paste0(path_lista_total, 'Historico_WRF_Belesar_rdata_BRUTO.Rdata')
saveRDS(Lista_total2, nombre_lista)
save(Lista_total2, file=nombre_lista2)




# Tratar Belesar ----------------------------------------------------------
nombre_lista<- paste0(path_lista_total, 'Historico_WRF_Belesar.RDS')
Lista_total<- readRDS(nombre_lista)
Lista_total_MF<- lapply(Lista_total, function(x) bind_rows(x))

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
  
  colnames(d1_2)<- c("Date", "LON", "LAT", "Rainfall[mm]")
  colnames(d2)<- c("Date", "LON", "LAT", "Rainfall[mm]")
  
  lista_loc_d12<- list(d1_2,d2)
  names(lista_loc_d12)<- c("D1", "D2")
  
  Lista_d1_d2_loc[[i]]<- lista_loc_d12
}

names(Lista_d1_d2_loc)<- names(Lista_total_MF)


Tabla_periodo1<- Return_periodo_Belesar()
colnames(Tabla_periodo1)<- c("Date", "LON", "LAT", "Rainfall[mm]", "Acumulated")

Lista_d1_d2_loc2<- list()
for (j in 1:length(Lista_d1_d2_loc)) {
  prueba_list<- Lista_d1_d2_loc[[j]]
  lista_retorno<- list()
  for(i in 1:2){
    prueba<- prueba_list[[i]]
    Tabla_periodo<- Tabla_periodo1
    Tabla_periodo$LON[match(prueba$Date,Tabla_periodo$Date)] <- prueba$LON
    Tabla_periodo$LAT[match(prueba$Date,Tabla_periodo$Date)] <- prueba$LAT
    Tabla_periodo$`Rainfall[mm]`[match(prueba$Date,Tabla_periodo$Date)] <- prueba$`Rainfall[mm]`
    lista_retorno[[i]]<- Tabla_periodo
  }
  names(lista_retorno)<- c("D1", "D2")
  Lista_d1_d2_loc2[[j]]<- lista_retorno 
  
  
}
names(Lista_d1_d2_loc2)<- names(Lista_d1_d2_loc)

Lista_d1_d2_loc3<- list()
for (i in 1:length(Lista_d1_d2_loc2)) {
  
  x_lista<- Lista_d1_d2_loc2[[i]]
  
  datos_erroneos<- which(x[[2]]$`Rainfall[mm]`<0)
  if(length(datos_erroneos)==0){
    x[[2]]<- x[[2]]%>%
      mutate(., Acumulated=cumsum(`Rainfall[mm]`))
    }else{
    x_lista[[2]]<- x_lista[[2]][-which(x_lista[[2]]$`Rainfall[mm]`<0),]%>%
      mutate(., Acumulated=cumsum(`Rainfall[mm]`))
    }
  
  datos_erroneos<- which(x_lista[[1]]$`Rainfall[mm]`<0)
  if(length(datos_erroneos)==0){
    x_lista[[1]]<- x_lista[[1]]%>%
      mutate(., Acumulated=cumsum(`Rainfall[mm]`))
  }else{
    x_lista[[1]]<- x_lista[[1]][-which(x_lista[[1]]$`Rainfall[mm]`<0),]%>%
      mutate(., Acumulated=cumsum(`Rainfall[mm]`))
  }
  
  Lista_d1_d2_loc3[[i]]<-  x_lista
  
  
}
  
names(Lista_d1_d2_loc3)<- names(Lista_d1_d2_loc2)


path_hist_WRF<- here::here('Data/Parques/Belesar/Historico/Historico_WRF_Belesar.RDS')
saveRDS(Lista_d1_d2_loc3,path_hist_WRF)




# DHI historico -----------------------------------------------------------


path_to_DHI<- here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar.RDS')
Hist_DHI<- readRDS(path_to_DHI)

head(Hist_DHI)


acumulada<- Hist_DHI$`LLUVIA ACUMULADA DÍA (l/m2)`
Hist_DHI$`LLUVIA ACUMULADA DÍA (l/m2)`<- c(acumulada[2:length(acumulada)],0)

V<- yday(Hist_DHI$DATE)
diff_Day<- c(1,1+which(diff(V)!=0))

for (i in 1:length(Hist_DHI$DATE)) {
  
  if(sum(i==diff_Day)>0){
    Hist_DHI$DESACUMULADA[i]<- Hist_DHI[i,3] 
  }else{Hist_DHI$DESACUMULADA[i]<- Hist_DHI[i,3]- Hist_DHI[i-1,3]}
  }

Hist_DHI$Qnet<- Hist_DHI$`APORTACION (m3/s)`-(Hist_DHI$`CAUDAL TURBINADO (m3/s)`+Hist_DHI$`Q. TURB. BCE. (m3/s)`)

Acumulada_horaria<- Hist_DHI %>% group_by(round_date(DATE, "hour")) %>% 
  summarise(hourly_rainfall =sum(DESACUMULADA))

Acum_hor<- Acumulada_horaria$hourly_rainfall


hourly<- seq(min(Hist_DHI$DATE), max(Hist_DHI$DATE), by="hour")

LLuvia_DHI<- as.data.frame(cbind(as.character(hourly), Acum_hor))
LLuvia_DHI$V1<- ymd_hms(as.character(LLuvia_DHI$V1))
LLuvia_DHI$Acum_hor<-as.numeric(as.character(LLuvia_DHI$Acum_hor))
colnames(LLuvia_DHI)<- c("Date", "Rainfall[mm]")

path_hist_LLuvia<- here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar_Lluvia.RDS')
saveRDS(LLuvia_DHI,path_hist_LLuvia)

# Juntar DHI y Belesar ----------------------------------------------------

Belesar_WRF<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_WRF_Belesar.RDS'))
Belesar_DHI<- readRDS(here::here('Data/Parques/Belesar/Historico/Historico_DHI_Belesar_Lluvia.RDS'))


### Merge por dates... 2 metodos
df1<- prueba
df2<- Belesar_DHI
colnames(df2)<- c("Date", "Real_RF")

#data.table
setDT(df1)
setDT(df2)
Merge_datatable<- df2[df1, on = c('Date')]

#dplyr
Merge_dplyr<- left_join(df1, df2, by=c("Date"))

###usamos dplyr
df2<- Belesar_DHI
colnames(df2)<- c("Date", "Real_RF")

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
    
    colnames(Table_fine)<- c("Date", "LON","LAT", "WRF","Observada")
    lista_retorno[[i]]<- Table_fine
  }
  names(lista_retorno)<- c("D1", "D2")
  Belesar_Merge_cc[[j]]<- lista_retorno 
}
names(Belesar_Merge_cc)<- names(Belesar_Merge)







# Ploteando cositas para sacar pistas -------------------------------------


prueba<- Belesar_Merge_cc[[19]]$D2
prueba_acum<- prueba %>% mutate(WRF_Acum= cumsum(WRF),
                  Observada_Acum=cumsum(Observada))

prueba_acum<- prueba_acum[year(prueba_acum$Date)==2019, ]



wrf<- prueba_acum[,c("Date","WRF_Acum", "WRF")]
obs<- prueba_acum[,c("Date","Observada_Acum", "Observada")]

colnames(wrf)<- c("Date","acum","rain")
colnames(obs)<- c("Date","acum","rain")


data<-wrf
data2<-obs

plot_rain2(data,data2)

for (i in 1:length(Belesar_Merge_cc)) {
  prueba<- Belesar_Merge_cc[[i]]$D2
  prueba_acum<- prueba %>% mutate(WRF_Acum= cumsum(WRF),
                                  Observada_Acum=cumsum(Observada))
  prueba_acum<- prueba_acum[prueba_acum$Date> ymd("2018/10/1"),]
  wrf<- prueba_acum[,c("Date","WRF_Acum", "WRF")]
  obs<- prueba_acum[,c("Date","Observada_Acum", "Observada")]
  
  colnames(wrf)<- c("Date","acum","rain")
  colnames(obs)<- c("Date","acum","rain")
  path_belesar<- here::here('graph/Belesar/')
  
  plot_rain2(wrf,obs)
  ggsave(filename =paste0(path_belesar,"/",names(Belesar_Merge_cc)[i],"_D2.png"),
         device = "png",
         dpi=200,
         width = 7,
         height = 7,
         units = "in")
}






#Esta funcion necesita que data contenga acum, rain y Date
plot_rain<- function(data){
  k<- max(data$acum)/max(data$rain)
  ggplot(data=data, aes(x=Date))+
    geom_bar(aes(y=rain), stat="identity")+
    xlab("Date")+ylab("Lluvia por hora [mm/h]")+theme(panel.background = element_blank(), 
                                                      panel.grid = element_blank())+
    geom_line(aes(y = acum/k), group = 1, col="red") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / (1/k), name = " LLuvia acumulada [mm]", 
                                           breaks = seq(min(data$acum),
                                                        max(data$acum),
                                                        by=50)),
                       breaks = seq(min(data$rain),
                                    max(data$rain),
                                    by=1))
  
  
}

plot_rain2<- function(data,data2){
  ggplot(data=data, aes(x=Date))+
    geom_line(aes(y=rain), stat="identity")+
    xlab("Date")+ylab("Lluvia por hora [mm/h]")+theme(panel.background = element_blank(), 
                                                      panel.grid = element_blank()) +
    geom_line(data= data2, aes(x=Date, y=rain), color="red", alpha=0.5)+
    geom_text(aes(as.POSIXct(ymd("2019/02/01")),10, label=paste0("Cor: ",round(cor(data$rain,data2$rain), 
                                                                        digits = 2))))

}



plot_rain(wrf)
plot_rain(obs)




















# Investigando aportacion  ------------------------------------------------


prueba<-Hist_DHI$`APORTACION (m3/s)`[-which(Hist_DHI$`APORTACION (m3/s)`>1500 
                                            | Hist_DHI$`APORTACION (m3/s)`< 0)]

summary(prueba)

plot(prueba, type = "l", ylim = c(0,400))

for (i in 96) {
  plot(SMA(prueba, i), type = "l", ylim = c(0,400))
  
}


plot(SMA(prueba, 2), type = "l", ylim = c(0,400))
plot(SMA(prueba, 5), type = "l", ylim = c(0,400))
plot(SMA(prueba, 10), type = "l",ylim = c(0,400))
plot(SMA(prueba, 20), type = "l",ylim = c(0,400))
plot(SMA(prueba, 50), type = "l", ylim = c(0,400))
plot(SMA(prueba, 70), type = "l", ylim = c(0,400))
plot(SMA(prueba,100), type = "l", ylim = c(0,400))
plot(SMA(prueba,150), type = "l",ylim = c(0,400))