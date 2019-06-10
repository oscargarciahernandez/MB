library(here)
source(here::here('libraries.R'))

x<- here::here('SISTEMA_ELECTRICO/') %>% list.files(full.names = T) %>% .[1] %>%   
  read.csv(sep = ";", header = T)

x$geoid<- NULL
x$geoname<- NULL

DATETIME<- x$datetime %>% str_split("T|[+]") %>% lapply(function(y){
  DateUTC<- y %>%  .[1:2] %>% paste(collapse = " ") %>% ymd_hms()
  hour_diff<- y  %>%  .[3] %>% hm()
  DateUTC+hour_diff 
}) 
  
y<- here::here('SISTEMA_ELECTRICO/') %>% list.files(full.names = T) %>% .[2] %>%   
  read.csv(sep = ";", header = T)

y$geoid<- NULL
y$geoname<- NULL
