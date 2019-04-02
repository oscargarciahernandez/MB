library(here)
source(here::here('libraries.R'))

#Lo de los mapas esta a la espera de poder instalar JAVA
# Mapeos de puntos y demas ------------------------------------------------


#Seteamos el tamaño del mapa, para ello habrá que elegir 
n=max(Datos2$lat)    
s=min(Datos2$lat)    
e=max(Datos2$lon)    
w=min(Datos2$lon)    


#Fijamos incremento para hacer más grande el mapa

incr<- 0.0215


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right


download_maps(ul, lr, maptyp = "bing", res=15)
