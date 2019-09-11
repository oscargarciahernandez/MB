library(here)
source('libraries.R')


IMPORTAR_HISTORICO_PYTHON_Y_CREAR_CSV<- FALSE

if(IMPORTAR_HISTORICO_PYTHON_Y_CREAR_CSV){
  TABLA_METEOGALICIA<- '/home/meteobit/METEOGALICIA/' %>% list.files(full.names = T) %>% .[str_detect(., 'HISTORICO')] %>% read.csv()
  TABLA_METEOGALICIA$TIMESTAMP<- TABLA_METEOGALICIA$DATE
  
  TABLA_METEOGALICIA$DATE<-  TABLA_METEOGALICIA$DATE_UNITS %>% as.character() %>% str_remove('second since ') %>% ymd_hms() + as.difftime(TABLA_METEOGALICIA$TIMESTAMP/60, units = 'mins')
  
  
  TABLA_METEOGALICIA$TSIM<- TABLA_METEOGALICIA$TIMESTAMP/3600 
  TABLA_METEOGALICIA <- TABLA_METEOGALICIA[,c("DATE", 'TSIM',"LON", 'LAT', 'U10','V10','ULEV1','VLEV1','ULEV2','VLEV2','ULEV3','VLEV3')]
  
  
  TABLA_METEOGALICIA<- TABLA_METEOGALICIA %>% 
    group_by(LON,LAT,DATE) %>%
    mutate(WS10= sqrt(U10^2 + V10^2),
           WD10= atan2(U10/WS10, V10/WS10)* 180/pi +180,
           WSLEV1= sqrt(ULEV1^2 + VLEV1^2),
           WDLEV1= atan2(ULEV1/WSLEV1, VLEV1/WSLEV1)* 180/pi +180,
           WSLEV2= sqrt(ULEV2^2 + VLEV2^2),
           WDLEV2= atan2(ULEV2/WSLEV2, VLEV2/WSLEV2)* 180/pi +180,
           WSLEV3= sqrt(ULEV3^2 + VLEV3^2),
           WDLEV3= atan2(ULEV3/WSLEV3, VLEV3/WSLEV3)* 180/pi +180)
  
  
  
  PATH_CERROBLANCO<- here::here('Data/Parques/PRUEBA_EOLICOS/CERROBLANCO/METEOGALICIA/')
  if(!dir.exists(PATH_CERROBLANCO)){dir.create(PATH_CERROBLANCO, recursive = TRUE)}
  
  
  write.csv(TABLA_METEOGALICIA,
            paste0(PATH_CERROBLANCO, 'METEOGALICIA_', as.Date(now()), '.csv'), 
            col.names = TRUE, row.names = FALSE )
  
}




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

