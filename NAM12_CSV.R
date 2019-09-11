library(here)
source('libraries.R')


TATANKA_FILES<- '/media/meteobit/Elements/NAM12/'  %>% list.files(full.names = T) %>% .[str_detect(., 'TATANKA_CONVERTED')]
for (i in TATANKA_FILES) {
  if(file.info(i)$size>10){
    print(i)
  }  
}

k<- 1 
LISTA_TATANKA_NAM<- list()
for(file in TATANKA_FILES){
  
  TATANKA<- read.csv(file)
  
  TATANKA$DATE<- ifelse(nchar(as.character(TATANKA$DATE))<11, 
                           paste(as.character(TATANKA$DATE), '00:00:00'), 
                           as.character(TATANKA$DATE)) %>% ymd_hms()
  
  TATANKA$FCST_TIME<- TATANKA$FCST_TIME %>% str_replace('fcst time', '') %>%str_replace('hrs', '') %>% as.numeric()
  
  TATANKA$VAR_NAME_MERGE<- paste0(str_replace_all(TATANKA$VAR_NAME, ' ',''), TATANKA$LEVEL)
  TATANKA$VAR_NAME<- NULL
  TATANKA$LEVEL<- NULL
  
  library(tidyr)
  
  TATANKA_1<- TATANKA %>% 
    group_by(VAR_NAME_MERGE) %>%
    dplyr::mutate(i1 = row_number()) %>% 
    spread(VAR_NAME_MERGE, VALUES)
  
  
  TATANKA_2<- TATANKA_1 %>% group_by(LON,LAT,DATE) %>% mutate(WS10= sqrt(`10metreVwindcomponentlevel 10 m`^2 + `10metreUwindcomponentlevel 10 m`^2),
                                                                    WD10= atan2(`10metreUwindcomponentlevel 10 m`/WS10, `10metreVwindcomponentlevel 10 m`/WS10)* 180/pi +180,
                                                                    WS80= sqrt(`Vcomponentofwindlevel 80 m`^2 + `Ucomponentofwindlevel 80 m`^2),
                                                                    WD80= atan2(`Ucomponentofwindlevel 80 m`/WS80, `Vcomponentofwindlevel 80 m`/WS80)* 180/pi +180)
  
  
  
  TATANKA_3<- TATANKA_2[,c( "DATE","FCST_TIME","LON", "LAT","WS10", "WD10", "WS80", "WD80")]
  
  LISTA_TATANKA_NAM[[k]]<- TATANKA_3
  
  k<- k + 1 
  
  
}


TABLA_TATANKA<- LISTA_TATANKA_NAM %>% bind_rows()

if(!dir.exists( here::here('Data/Parques/PRUEBA_EOLICOS/TATANKA_DATA'))){dir.create(here::here('Data/Parques/PRUEBA_EOLICOS/TATANKA_DATA'), recursive = TRUE)}

saveRDS(TABLA_TATANKA, here::here('Data/Parques/PRUEBA_EOLICOS/TATANKA_DATA/NAM_12_TATANKA.RDS'))





# AÑADIMOS LOS DATOS DE PRODUCCION A LOS DATOS DEL NAM12 ------------------

ADD_PROD<- FALSE

if (ADD_PROD) {
  TATANKA_DATA<- here::here('Data/Parques/PRUEBA_EOLICOS/TATANKA_DATA/NAM_12_TATANKA.RDS') %>% readRDS()
  INFO_PARQUES<- readRDS(here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS'))
  INFO_TATANKA<- INFO_PARQUES %>% filter(PARQUE=='P.E.Tatanka')
  
  MERGE_TATANKA<- left_join(TATANKA_DATA, INFO_TATANKA, by= 'DATE')
  
  write.csv(MERGE_TATANKA, 
            file = here::here('Data/Parques/PRUEBA_EOLICOS/TATANKA_DATA/NAM_12_TATANKA_WITH_PRODUCTION.csv'),
            sep = ';')
}


