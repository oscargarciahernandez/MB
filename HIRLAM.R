library(here)
source('libraries.R')

HIRLAM_FILES<- '/home/meteobit/Escritorio/Parques/EZ/' %>% list.files(full.names = T) %>% .[str_detect(., 'CONVERTED')]
for (i in HIRLAM_FILES) {
  if(file.info(i)$size>10){
    print(i)
  }  
}


HIRLAM<- read.csv('/home/meteobit/Escritorio/Parques/EZ//ATHd00.20170420_CONVERTED.csv')



HIRLAM$DATE<- HIRLAM$DATE %>% paste(., '00:00')%>% ymd_hm() + as.difftime(HIRLAM$FCST_TIME %>% 
                                      str_replace('fcst time', '') %>% 
                                      str_replace('hrs', '') %>% as.numeric(), units = 'hours') 

HIRLAM$FCST_TIME<- HIRLAM$FCST_TIME %>% str_replace('fcst time', '') %>%str_replace('hrs', '') %>% as.numeric()

HIRLAM$LEVEL <- NULL


library(tidyr)

HIRLAM_1<- HIRLAM %>% 
  group_by(VAR_NAME) %>%
  dplyr::mutate(i1 = row_number()) %>% 
  spread(VAR_NAME, VALUES) %>%
  select(-i1)


HIRLAM_1$DATE %>% as.character()%>% range()
HIRLAM_2<- HIRLAM_1 %>% group_by(LON,LAT,DATE) %>% mutate(WS10= sqrt(`10 metre U wind component`^2 + `10 metre V wind component`^2),
                                               WD10= atan2(`10 metre U wind component`/WS10, `10 metre V wind component`/WS10)* 180/pi +180,
                                               WS100= sqrt(`100 metre U wind component`^2 + `100 metre V wind component`^2),
                                               WD100= atan2(`100 metre U wind component`/WS100, `100 metre V wind component`/WS100)* 180/pi +180)



HIRLAM_3<- HIRLAM_2[which(!HIRLAM_2$FCST_TIME> 120),]

HIRLAM_4<- HIRLAM_3[,c( "DATE","LON", "LAT","WS10", "WD10", "WS100", "WD100")]




