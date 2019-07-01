library(rNOMADS)
library(rgdal)
library(here)
source('libraries.R')


### CONVERT TO NETCDF USING SYSTEM Y WGRIB2... DESCARTADO MUY LENTO MUY PESADO 

CONVERT_TO_NTCDF<- FALSE
if (CONVERT_TO_NTCDF) {
  FOLDERS<- here::here('Gribs/') %>% list.dirs(recursive = F)
  
  GRIBS<- FOLDERS %>% list.files(full.names = T) %>% paste0("wgrib2 ", .," -netcdf " , 
                                                            paste0(str_replace_all(., "[.]", ""),".nc"))
  for (i in 1:length(GRIBS)) {
    system(GRIBS[i])
  }
 }
# HAY QUE TENER EN CUENTA QUE PARA LA BASE DE DATOS DE EL MUNDO NECESITAMOS: 
# TMIN, TMAX, TPROMEDIO, 
#LLUVIA TOTAL, LLUVIA COVECTIVA
# NIEVE 
ANALIZAR_VARIABLES<- FALSE
if(ANALIZAR_VARIABLES){
  GFS_table<- "https://www.nco.ncep.noaa.gov/pmb/products/gfs/gfs.t00z.pgrb2.0p25.f006.shtml" %>% GET() %>% 
    htmlParse() %>% readHTMLTable() %>% .[[2]]
  
  info_grib<- GribInfo(here::here('Gribs/20190627/gfs.t00z.pgrb2.0p25.f050'))
  
  Grib_table<- info_grib$inventory %>% str_split(':') %>% lapply(function(x){
    r<- matrix(ncol = 2) %>% as_data_frame()
    r[1, 1:2]<- x[4:5]
    colnames(r)<- c("Var", "level")
    return(r)
  }) %>% bind_rows()
  
  
  for (i in 1:nrow(Grib_table)) {
    Grib_table$descrip[i]<- ifelse(GFS_table$Description[GFS_table$Parameter %>% as.character()==Grib_table$Var[i]] %>% unique()%>% is.null(),
                                   NA,
                                   GFS_table$Description[GFS_table$Parameter==Grib_table$Var[i]] %>% unique() %>% as.character())
    
  }
  Grib_table %>% View()
}





#LEEMOS LA TABLA DE CIUDADES DEL MUNDO  PARA PONER EL RANGO DE GRIBS QUE QUEREMOS LEER
#LEER XLS
#install.packages("gdata")
library(gdata)

Ciudades_mundo<- read.xls(here::here('Gribs/Ciudades/csvciudadesdelmundo/ciudades_mundo.xls'))
S<- range(Ciudades_mundo$latitud)[1]-0.5
N<- range(Ciudades_mundo$latitud)[2]+0.5
W<- range(Ciudades_mundo$longitud)[1]-0.5
E<- range(Ciudades_mundo$longitud)[2]+0.5



GRIB_FILES<- list.dirs(here::here('Gribs/'), recursive = FALSE) %>% .[1] %>% list.files(full.names = T) %>% .[1:97]

##OPCION BUCLE
OPCION_BUCLE<- FALSE
if(OPCION_BUCLE){
  LISTA_TEMPORAL<- list()
  for (i in 1:length(GRIB_FILES)) {
    READ_GRIB<- ReadGrib(GRIB_FILES[i],
                         variables = "TMP",
                         levels = "2 m above ground", 
                         domain = c(W, E, N, S))
    cat(paste('DESEMPAQUETANDO GRIB ', READ_GRIB$forecast.date))
    
    
    TABLE_GRIB<- cbind(READ_GRIB$forecast.date,READ_GRIB$lon,READ_GRIB$lat, READ_GRIB$value) %>% as.data.frame()
    colnames(TABLE_GRIB)<- c("Date","lon", "lat", "value")
    LISTA_TEMPORAL[[i]]<- TABLE_GRIB
    
  }
  
}

#OPCION PARALLEL
library(parallel)
#OPCIÓN DE PARALELIZAR PONIENDO UNA BARRA DE PROGRESO
#install.packages("pbmcapply")
library(pbmcapply)
BARRA_DE_PROGRESO<- TRUE


#DEFINIMOS FUNCIÓN
OBTAIN_GRIB_PARAMETERS<- function(GRIB_FILE){
  READ_GRIB<- ReadGrib(GRIB_FILE,
                       variables = "TMP",
                       levels = "2 m above ground", 
                       domain = c(W, E, N, S))
  cat(paste('DESEMPAQUETANDO GRIB ', READ_GRIB$forecast.date))
  
  
  TABLE_GRIB<- cbind(READ_GRIB$forecast.date,READ_GRIB$lon,READ_GRIB$lat, READ_GRIB$value) %>% as.data.frame()
  colnames(TABLE_GRIB)<- c("Date","lon", "lat", "value")
  return(TABLE_GRIB)
}

#EJECUTAMOS MODO PARALELO
#CUIDADITO CON ESTO... PORQUE EL PAQUETE rNOMADS HACE USO DE WGRIB2...
# QUE SE ENCARGA DE DESEMPAQUETAR GRIBS Y DE POR SI HACE UN TRABAJO MULTINÚCLEO...
# POR LO TANTO SOLO PONDRÍA 2 NUCLEOS EN LOS PROCESOS PARALELOS... 
# CREO Y SOLO CREO, QUE WGRIB2 TIENE POR DEFECTO USAR LA MITAD DEL TOTAL DE NUCLEOS DISPONIBLES


if(BARRA_DE_PROGRESO){LISTA_GRIBS<- pbmclapply(GRIB_FILES,
                           OBTAIN_GRIB_PARAMETERS,
                           mc.cores =  getOption("mc.cores",2L ))
}else{LISTA_GRIBS<- mclapply(GRIB_FILES,
                         OBTAIN_GRIB_PARAMETERS,
                         mc.cores =  getOption("mc.cores",2L ))}
GRIB_DATAFRAME<- LISTA_GRIBS %>% bind_rows()
rm(list=setdiff(ls(), "GRIB_DATAFRAME"))

#ESTAS COSAS HACEN UN USO BASTANTE JARTO DE LA MEMORIA RAM... HABRÁ QUE PENSAR ESTRATÉGIAS PARA 
# SUAVIZAR ESTE EFECTO 
# SE ME HA OCURRIDO QUE PODEMOS DIVIDIR INCLUSO MÁS LOS DOMINIOS Y ALIGERAR TODAVÍA MÁS SI CABE 
# EL PESO DE LOS GRIBS LEIDOS... HASTA AHORA ESTOY OBTENIENDO UN ELEMENTO DE 39.6 MB POR HORA...
# CON UN TOTAL DE 97 ARCHIVOS SUMAN CASI 4 Gb.... CREO QUE SERÁ DEMASIADO PARA R....
# ESTO ESTOY HABLANDO PARA UNA ÚNICA VARIABLE Y TENDREMOS QUE COJER 6.... NO SÉ MUY BIEN COMO LO VOY A HACER
# 




Ciudades_mundo<- read.xls(here::here('Gribs/Ciudades/csvciudadesdelmundo/ciudades_mundo.xls'))

LON_G<- GRIB_DATAFRAME$lon %>% unique() %>% as.character() %>% as.numeric()
LAT_G<- GRIB_DATAFRAME$lat %>% unique()%>% as.character() %>% as.numeric()

range(Ciudades_mundo$longitud)
for (i in 1:nrow(Ciudades_mundo)) {
  Ciudades_mundo$lat_g[i]<- LAT_G[which.min(abs(LAT_G - Ciudades_mundo$latitud[i]))]
  Ciudades_mundo$lon_g[i]<-LON_G[which.min(abs(LON_G - Ciudades_mundo$longitud[i]))]
}



#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

class(world)

ggplot(data = world) +
  geom_sf()+
  geom_point(data= Ciudades_mundo, aes(x= longitud, y= latitud), color= "red")
