library(here)
source('libraries.R')


TAMAULIPAS_FILES<- '/media/meteobit/Elements/NAM12/'  %>% list.files(full.names = T) %>% .[str_detect(., 'TAMAULIPAS_CONVERTED')]
for (i in TAMAULIPAS_FILES) {
  if(file.info(i)$size>10){
    print(i)
  }  
}



TAMAULIPAS<- read.csv("/media/meteobit/Elements/NAM12//nam_218_20190103_0000_010.grb2_TAMAULIPAS_CONVERTED.csv")

TAMAULIPAS$DATE<- TAMAULIPAS$DATE %>% ymd_hms()

TAMAULIPAS$FCST_TIME<- TAMAULIPAS$FCST_TIME %>% str_replace('fcst time', '') %>%str_replace('hrs', '') %>% as.numeric()

TAMAULIPAS$VAR_NAME_MERGE<- paste0(str_replace_all(TAMAULIPAS$VAR_NAME, ' ',''), TAMAULIPAS$LEVEL)
TAMAULIPAS$VAR_NAME<- NULL
TAMAULIPAS$LEVEL<- NULL

library(tidyr)

TAMAULIPAS_1<- TAMAULIPAS %>% 
  group_by(VAR_NAME_MERGE) %>%
  dplyr::mutate(i1 = row_number()) %>% 
  spread(VAR_NAME_MERGE, VALUES) %>%
  select(-i1)


TAMAULIPAS_2<- TAMAULIPAS_1 %>% group_by(LON,LAT,DATE) %>% mutate(WS10= sqrt(`10 metre U wind component`^2 + `10 metre V wind component`^2),
                                                          WD10= atan2(`10 metre U wind component`/WS10, `10 metre V wind component`/WS10)* 180/pi +180,
                                                          WS80= sqrt(`U component of wind`^2 + `V component of wind`^2),
                                                          WD80= atan2(`U component of wind`/WS80, `V component of wind`/WS80)* 180/pi +180)



HIRLAM_3<- HIRLAM_2[which(!HIRLAM_2$FCST_TIME> 120),]

HIRLAM_4<- HIRLAM_3[,c( "DATE","LON", "LAT","WS10", "WD10", "WS100", "WD100")]
