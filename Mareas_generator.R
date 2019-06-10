library(here)
source(here::here('libraries.R'))



# IMPORTAR INFORMACION DE TABLAS DE MAREAS --------------------------------
Tabla_mareas1<- read.csv(here::here('Mareas/tabla_mareas_2019.csv'), sep = ",")
Tabla_mareas<- read.csv(here::here('Mareas/tabla_mareas_2019.csv'), sep = ",")
for (i in 1:length(Tabla_mareas)) {
  Tabla_mareas[,i]<- ifelse(str_detect(Tabla_mareas[,i], "[[:digit:]]"),as.character(Tabla_mareas[,i]),NA )  
}

Tabla_mareas<- Tabla_mareas[complete.cases(Tabla_mareas$EGUNAK.DÍAS),]


prueba<- c(0,which(diff(as.numeric(Tabla_mareas$EGUNAK.DÍAS))<0))
lista_p<- list()
for (i in 1:length(prueba)) {
  lista_p[[i]]<- rep(i, c(diff(as.numeric(prueba)),31)[i])
  
}

vect_split<- unlist(lista_p)
Tablas_mareas2<- split(Tabla_mareas, vect_split)
Tablas_pleamar<- Tablas_mareas2[seq(1,23,2)]
names(Tablas_pleamar)<- c("Ene","Feb","Mar", "Apr", "MAy", "jun", "jul", "Agos",
                          "Sept", "Oct","Nov","Dic")
Tablas_pleamar<- lapply(Tablas_pleamar, function(x){
  x<- x[,c(1,3,4,7,8)]
  colnames(x)<- c("Dia", "Hora1", "H1", "Hora2", "H2")
  x$Dia<- as.numeric(as.character(x$Dia))
  x$Hora1<- x$Hora1 %>% str_replace(" ",":") %>% hm()
  x$Hora2<- x$Hora2 %>% str_replace(" ",":") %>% hm()
  x$H1<- x$H1 %>% str_replace(",",".") %>% as.numeric()
  x$H2<- x$H2 %>% str_replace(",",".") %>% as.numeric()
  return(as.data.frame(x))
})
Lista_actualizada_pleamar<- list()
for (i in 1:length(Tablas_pleamar)) {
  x<- Tablas_pleamar[[i]]
  fecha_hora1<- paste("2019",i,x$Dia,"-",x$Hora1, sep = "/") %>% ymd_hms()
  fecha_hora2<- paste("2019",i,x$Dia,"-",x$Hora2, sep = "/") %>% ymd_hms()
  
  tabla1<- cbind(as.character(fecha_hora1), x$H1) %>% as.data.frame()
  colnames(tabla1)<- c("Date", "Altura")
  tabla2<- cbind(as.character(fecha_hora2), x$H2) %>% as.data.frame()
  colnames(tabla2)<- c("Date", "Altura")
  
  tabla_merged<- rbind(tabla1,tabla2) %>% as.data.frame() %>% .[order(ymd_hms(.$Date)), ]
  tabla_merged$Date<- ymd_hms(tabla_merged$Date) %>%  + hm("2:00")
  Lista_actualizada_pleamar[[i]]<- tabla_merged
}


Tabla_pleamar<- lapply(Lista_actualizada_pleamar, function(x) x[complete.cases(x), ]) %>% 
  bind_rows()









Tablas_bajamar<- Tablas_mareas2[seq(2,24,2)]
names(Tablas_bajamar)<- c("Ene","Feb","Mar", "Apr", "MAy", "jun", "jul", "Agos",
                          "Sept", "Oct","Nov","Dic")
Tablas_bajamar<- lapply(Tablas_bajamar, function(x){
  x<- x[,c(1,3,4,7,8)]
  colnames(x)<- c("Dia", "Hora1", "H1", "Hora2", "H2")
  x$Dia<- as.numeric(as.character(x$Dia))
  x$Hora1<- x$Hora1 %>% str_replace(" ",":") 
  x$Hora2<- x$Hora2 %>% str_replace(" ",":")
  x$H1<- x$H1 %>% str_replace(",",".") %>% as.numeric()
  x$H2<- x$H2 %>% str_replace(",",".") %>% as.numeric()
  return(as.data.frame(x))
})



Lista_actualizada_bajamar<- list()
for (i in 1:length(Tablas_bajamar)) {
  x<- Tablas_bajamar[[i]]
  fecha_hora1<- paste("2019",i,x$Dia,"-",x$Hora1, sep = "/") %>% ymd_hm()
  fecha_hora2<- paste("2019",i,x$Dia,"-",x$Hora2, sep = "/") %>% ymd_hm()
  
  tabla1<- cbind(as.character(fecha_hora1), x$H1) %>% as.data.frame()
  colnames(tabla1)<- c("Date", "Altura")
  tabla2<- cbind(as.character(fecha_hora2), x$H2) %>% as.data.frame()
  colnames(tabla2)<- c("Date", "Altura")
  
  tabla_merged<- rbind(tabla1,tabla2) %>% as.data.frame() %>% .[order(ymd_hms(.$Date)), ]
  tabla_merged$Date<- ymd_hms(tabla_merged$Date) %>%  + hm("2:00")
  Lista_actualizada_bajamar[[i]]<- tabla_merged
}


Tabla_bajamar<- lapply(Lista_actualizada_bajamar, function(x) x[complete.cases(x), ]) %>% 
  bind_rows()


#PONER EN ORDEN LAS VARIABLES DE LAS TABLAS
Tabla_bajamar$Date<- ymd_hms(Tabla_bajamar$Date)
Tabla_pleamar$Date<- ymd_hms(Tabla_pleamar$Date)
Tabla_bajamar$Altura<-Tabla_bajamar$Altura %>% as.numeric()
Tabla_pleamar$Altura<-Tabla_pleamar$Altura %>% as.numeric()

#SEPARAR POR MESES OS DATOS DE LA TABLA
#QUIERO HACER LAS TABLAS DE MAYO EN ADELANTE

Lista_bajamar<- Tabla_bajamar %>% group_split(month(Date)) %>% lapply(., function(x) x[,1:2]) %>% .[5:12]
Lista_pleamar<- Tabla_pleamar %>% group_split(month(Date)) %>% lapply(., function(x) x[,1:2]) %>% .[5:12]

names(Lista_bajamar)<- c("MAY","JUN", "JUL", "AGOS", "SEPT", "OCT", "NOV", "DEC")
names(Lista_pleamar)<- c("MAY","JUN", "JUL", "AGOS", "SEPT", "OCT", "NOV", "DEC")


for (i in 1:length(Lista_bajamar)) {
  Bajamar_textos<- Lista_bajamar[[i]] %>% group_split(day(Date)) %>% 
    sapply(., function(x){
    if(nrow(x)==2){
      paste(paste0(hour(x$Date),":", sprintf("%02d", minute(x$Date)), " con ", x$Altura %>% as.character() %>% str_replace("[.]", ",")), collapse = " / ")
    }else{paste0(hour(x$Date),":", sprintf("%02d", minute(x$Date)), " con ",  x$Altura %>% as.character() %>% str_replace("[.]", ","))}
    
  }) 
  Pleamar_textos<- Lista_pleamar[[i]] %>% group_split(day(Date)) %>% 
    sapply(., function(x){
    if(nrow(x)==2){
      paste(paste0(hour(x$Date),":", sprintf("%02d", minute(x$Date)), " con ",  x$Altura %>% as.character() %>% str_replace("[.]", ",")), collapse = " / ")
    }else{paste0(hour(x$Date),":", sprintf("%02d", minute(x$Date)), " con ",  x$Altura %>% as.character() %>% str_replace("[.]", ","))}
    
  }) 
  
  Tabla_junto<- cbind(seq(1, length(Bajamar_textos)), 
                      Bajamar_textos,
                      Pleamar_textos) %>% as.data.frame()
  
  colnames(Tabla_junto)<- c("Dia_del_mes",	"texto_BAJAMAR",	"texto_PLEAMAR")
  
  nombre_mareas<- paste0("Mareas_",names(Lista_bajamar)[i],".txt"  )
  
  if(!dir.exists(here::here("Mareas"))){dir.create(here::here("Mareas"))}
  write.table(Tabla_junto, 
              here::here(paste0("Mareas/bizkaia_",nombre_mareas)),
              sep = "\t",
              row.names = FALSE,
              dec = ",")
}

