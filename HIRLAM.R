library(here)
library(tidyr)
source('libraries.R')


PATH_CERROBLANCO<- here::here('Data/Parques/PRUEBA_EOLICOS/CERROBLANCO/HIRLAM/')
if(!dir.exists(PATH_CERROBLANCO)){dir.create(PATH_CERROBLANCO, recursive = TRUE)}

READ_CSVs= FALSE

if (READ_CSVs) {
  
  HIRLAM_FILES<- '/home/meteobit/Escritorio/Parques/EZ/' %>% list.files(full.names = T) %>% .[str_detect(., 'CONVERTED')]
  
  HIRLAM_LIST<- list()
  HIRLAM_ERRORS<- vector()
  
  k<- 1
  ke<- 1
  
  for(file in HIRLAM_FILES){
    HIRLAM_ERRORS[ke]<- tryCatch({
      HIRLAM<- read.csv(file)
      
      HIRLAM$DATE<- HIRLAM$DATE %>% paste(., '00:00')%>% ymd_hm() + as.difftime(HIRLAM$FCST_TIME %>% 
                                                                                  str_replace('fcst time', '') %>% 
                                                                                  str_replace('hrs', '') %>% as.numeric(), units = 'hours') 
      
      HIRLAM$FCST_TIME<- HIRLAM$FCST_TIME %>% str_replace('fcst time', '') %>%str_replace('hrs', '') %>% as.numeric()
      HIRLAM$LEVEL <- NULL
      
      HIRLAM_1<- HIRLAM %>% 
        group_by(VAR_NAME) %>%
        dplyr::mutate(i1 = row_number()) %>% 
        spread(VAR_NAME, VALUES) %>%
        select(-i1)
      
      
      HIRLAM_1$DATE %>% as.character()%>% range()
      HIRLAM_2<- HIRLAM_1 %>% group_by(LON,LAT,DATE) %>% mutate(WS10= sqrt(`10 metre U wind component`^2 + `10 metre V wind component`^2),
                                                                WD10= atan2(`10 metre U wind component`/WS10, `10 metre V wind component`/WS10)* 180/pi +180,
                                                                WS100= sqrt(`100 metre U wind component`^2 + `100 metre V wind component`^2),
                                                                WD100= atan2(`100 metre U wind component`/WS100, `100 metre V wind component`/WS100)* 180/pi +180,
                                                                U10= `10 metre U wind component`,
                                                                V10= `10 metre V wind component`,
                                                                U100= `100 metre U wind component`,
                                                                V100= `100 metre V wind component`)
      
      
      
      HIRLAM_3<- HIRLAM_2[which(!HIRLAM_2$FCST_TIME> 120),]
      
      HIRLAM_4<- HIRLAM_3[,c( "DATE", "FCST_TIME","LON", "LAT","WS10", "WD10", "WS100", "WD100","U10", "V10","U100", "V100")]
      
      HIRLAM_LIST[[k]]<- HIRLAM_4
      k<- k+1
      
    }, error= function(e){
        return(file)
      
    })
    ke<- ke + 1
  }
  for(error_file in HIRLAM_ERRORS[str_detect(HIRLAM_ERRORS, '/home/meteobit/')]){
    file.remove(error_file)
  }
  
}


CREAMOS_TABLA_Y_GUARDAMOS_CSV<- FALSE
if(CREAMOS_TABLA_Y_GUARDAMOS_CSV){
  
  HIRLAM_DF<- HIRLAM_LIST %>% bind_rows()
  
  write.csv(HIRLAM_DF,paste0(PATH_CERROBLANCO, 'HIRLAM_', as.Date(now()), '.csv'), col.names = TRUE, row.names = FALSE )
  
}




HIRLAM_DATA<- read.csv(PATH_CERROBLANCO %>% list.files(full.names = TRUE) %>% .[length(.)])
HIRLAM_DATA$DATE<- HIRLAM_DATA$DATE %>% ymd_hms()


buscar_huecos=function(vector_fechas,periodo){
  #Esta funcion busca que huecos tenemos en las mediciones.
  #No sobreescribe nada, solo devuelve el dataframe huecos.
  #huecos[1] muestra cual es la medicion posterior (en el tiempo) al hueco.
  #huecos[2] muestra cual es la medicion anterior (en el tiempo) al hueco.
  #Las fechas estan en numerico (segundos desde 1970-01-01)
  #Periodo especifica la distancia entre mediciones esperable en segundos. Por ejemplo, periodo=3600 para una serie horaria
  huecos=data.frame(a=as.POSIXct(character(),tz="UTC"), b=as.POSIXct(character(),tz="UTC"))  #Creamos relleno de esta forma para que cada columna este ya en el formato que queremos
  colnames(huecos)=c("despues","antes")
  cont=1
  vector_fechas=sort(vector_fechas)
  for(i in 1:(length(vector_fechas)-1)){
    difer=as.numeric(time_length((vector_fechas[i+1]-vector_fechas[i]),unit = 's'))     #La duracion del hueco en segundos, formato numeric
    if (difer<=0){
      print(paste0("ERROR! vector_fechas[",as.character(i-1),"]-vector_fechas[",as.character(i),"]=",as.character(difer/60)," minutos"))
    }
    if (difer>periodo*1.5){  #3600 por que esperamos que los datos sean horarios. 1,5 para diferenciar bien los huecos
      huecos[cont,1]=vector_fechas[i+1]
      huecos[cont,2]=vector_fechas[i]
      cont=cont+1
    }
  }
  setDT(huecos)
  huecos[,diferencia_H := time_length(despues-antes,unit = 's')/3600]
  return(huecos)
}


BUSCAR_HUECOS<- FALSE

if(BUSCAR_HUECOS){
  #PONGO 3600 * 3 PORQUE SON DATOS 3 HORARIOS
  HUECOS<- buscar_huecos(sort(HIRLAM_DATA$DATE), 3600*3)
  
  HUECOS$diferencia_Dias<- HUECOS$diferencia_H/24
  
  View(HUECOS)
  
  # HIRLAM SE PROPORCIONA CON ARCHIVOS DIARIOS, POR LO TANTO HAREMOS UNA LISTA
  # CON LOS DIAS FALTANTES
  
  
  LISTA_HUECOS<- list()
  k<- 1
  for(huecos in 1:nrow(HUECOS)){
    for (dias_faltantes in 1:(HUECOS$diferencia_Dias[huecos]-1)) {
      LISTA_HUECOS[[k]]=HUECOS$antes[huecos] + as.difftime(dias_faltantes, units = 'days') 
      k<- k+1
    }
  }
  
  LISTA_HUECOS <- do.call("c", LISTA_HUECOS)
  
  write.csv(LISTA_HUECOS, paste0(PATH_CERROBLANCO, 'HUECOS_HIRLAM_', as.Date(now()), '.csv'), 
            row.names = FALSE)
  
  
}


