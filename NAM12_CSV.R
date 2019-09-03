library(here)
source('libraries.R')


TAMAULIPAS_FILES<- '/media/oscar/Elements/NAM12/'  %>% list.files(full.names = T) %>% .[str_detect(., 'TAMAULIPAS_CONVERTED')]
for (i in TAMAULIPAS_FILES) {
  if(file.info(i)$size>10){
    print(i)
  }  
}

k<- 1 
LISTA_TAMAULIPAS_NAM<- list()
for(file in TAMAULIPAS_FILES){
  
  TAMAULIPAS<- read.csv(file)
  
  TAMAULIPAS$DATE<- ifelse(nchar(as.character(TAMAULIPAS$DATE))<11, 
                           paste(as.character(TAMAULIPAS$DATE), '00:00:00'), 
                           as.character(TAMAULIPAS$DATE)) %>% ymd_hms()
  
  TAMAULIPAS$FCST_TIME<- TAMAULIPAS$FCST_TIME %>% str_replace('fcst time', '') %>%str_replace('hrs', '') %>% as.numeric()
  
  TAMAULIPAS$VAR_NAME_MERGE<- paste0(str_replace_all(TAMAULIPAS$VAR_NAME, ' ',''), TAMAULIPAS$LEVEL)
  TAMAULIPAS$VAR_NAME<- NULL
  TAMAULIPAS$LEVEL<- NULL
  
  library(tidyr)
  
  TAMAULIPAS_1<- TAMAULIPAS %>% 
    group_by(VAR_NAME_MERGE) %>%
    dplyr::mutate(i1 = row_number()) %>% 
    spread(VAR_NAME_MERGE, VALUES)
  
  
  TAMAULIPAS_2<- TAMAULIPAS_1 %>% group_by(LON,LAT,DATE) %>% mutate(WS10= sqrt(`10metreVwindcomponentlevel 10 m`^2 + `10metreUwindcomponentlevel 10 m`^2),
                                                                    WD10= atan2(`10metreUwindcomponentlevel 10 m`/WS10, `10metreVwindcomponentlevel 10 m`/WS10)* 180/pi +180,
                                                                    WS80= sqrt(`Vcomponentofwindlevel 80 m`^2 + `Ucomponentofwindlevel 80 m`^2),
                                                                    WD80= atan2(`Ucomponentofwindlevel 80 m`/WS80, `Vcomponentofwindlevel 80 m`/WS80)* 180/pi +180)
  
  
  
  TAMAULIPAS_3<- TAMAULIPAS_2[,c( "DATE","FCST_TIME","LON", "LAT","WS10", "WD10", "WS80", "WD80")]
  
  LISTA_TAMAULIPAS_NAM[[k]]<- TAMAULIPAS_3
  
  k<- k + 1 
  
  
}


TABLA_TAMAULIPAS<- LISTA_TAMAULIPAS_NAM %>% bind_rows()

saveRDS(TABLA_TAMAULIPAS, here::here('Data/Parques/PRUEBA_EOLICOS/TAMAULIPAS_DATA/NAM_12_TAMAULIPAS.RDS'))
