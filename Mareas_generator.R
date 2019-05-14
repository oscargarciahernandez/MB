library(request)
library(XML)




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
  x$Hora1<- x$Hora1 %>% str_replace(" ",":") %>% hm()+hm("2:00")
  x$Hora2<- x$Hora2 %>% str_replace(" ",":") %>% hm()+hm("2:00")
  x$H1<- x$H1 %>% str_replace(",",".") %>% as.numeric()
  x$H2<- x$H2 %>% str_replace(",",".") %>% as.numeric()
  return(as.data.frame(x))
})


Tablas_bajamar<- Tablas_mareas2[seq(2,24,2)]
names(Tablas_bajamar)<- c("Ene","Feb","Mar", "Apr", "MAy", "jun", "jul", "Agos",
                          "Sept", "Oct","Nov","Dic")
Tablas_bajamar<- lapply(Tablas_bajamar, function(x){
  x<- x[,c(1,3,4,7,8)]
  colnames(x)<- c("Dia", "Hora1", "H1", "Hora2", "H2")
  x$Dia<- as.numeric(as.character(x$Dia))
  x$Hora1<- x$Hora1 %>% str_replace(" ",":") %>% hm()+hm("2:00")
  x$Hora2<- x$Hora2 %>% str_replace(" ",":") %>% hm()+hm("2:00")
  x$H1<- x$H1 %>% str_replace(",",".") %>% as.numeric()
  x$H2<- x$H2 %>% str_replace(",",".") %>% as.numeric()
  return(as.data.frame(x))
})
##############GENERAMOS MAREAS_TXT PARA BIZKAIA 

PLEAMAR_MAYO<- Tablas_pleamar$MAy
BAJAMAR_MAYO<- Tablas_bajamar$MAy

minutos<- as.character(minute(as.Date(BAJAMAR_MAYO$Hora1, origin="1960-10-01")))
BAJAMAR1<- paste0(hour(as.Date(BAJAMAR_MAYO$Hora1, origin="1960-10-01")),
                  ":",
                  ifelse(nchar(minutos)==1, paste0("0",minutos), minutos),
                  " con ",
                  as.character(BAJAMAR_MAYO$H1), " m")


minutos<- as.character(minute(as.Date(BAJAMAR_MAYO$Hora2, origin="1960-10-01")))
BAJAMAR2<- paste0(hour(as.Date(BAJAMAR_MAYO$Hora2, origin="1960-10-01")),
                  ":",
                  ifelse(nchar(minutos)==1, paste0("0",minutos), minutos),
                  " con ",
                  as.character(BAJAMAR_MAYO$H2), " m")



texto_bajamar<-  paste(BAJAMAR1,"/",BAJAMAR2)

minutos<- as.character(minute(as.Date(PLEAMAR_MAYO$Hora1, origin="1960-10-01")))
PLEAMAR1<- paste0(hour(as.Date(PLEAMAR_MAYO$Hora1, origin="1960-10-01")),
                  ":",
                  ifelse(nchar(minutos)==1, paste0("0",minutos), minutos),
                  " con ",
                  as.character(PLEAMAR_MAYO$H1), " m")


minutos<- as.character(minute(as.Date(PLEAMAR_MAYO$Hora2, origin="1960-10-01")))
PLEAMAR2<- paste0(hour(as.Date(PLEAMAR_MAYO$Hora2, origin="1960-10-01")),
                  ":",
                  ifelse(nchar(minutos)==1, paste0("0",minutos), minutos),
                  " con ",
                  as.character(PLEAMAR_MAYO$H2), " m")


texto_pleamar<-  paste(PLEAMAR1,"/",PLEAMAR2)


Tabla_final<- as.data.frame(cbind(1:length(texto_bajamar), texto_bajamar, texto_pleamar))
colnames(Tabla_final)<- c("Dia_del_mes", "texto_BAJAMAR", "texto_PLEAMAR")


nombre_mareas<- paste0(as.character(lubridate::month(now(), label=TRUE)),"_",
                       range(as.numeric(as.character(Tabla_final$Dia_del_mes)))[1],"-",
                       range(as.numeric(as.character(Tabla_final$Dia_del_mes)))[2], ".txt")


if(!dir.exists(here::here("Mareas"))){dir.create(here::here("Mareas"))}
write.table(Tabla_final, 
            here::here(paste0("Mareas/bizkaia_",nombre_mareas)),
            sep = "\t",
            row.names = FALSE,
            dec = ",")




##############QUEDA PENDIENTE HACER LA TABLA PARA GUIPUZKOA
