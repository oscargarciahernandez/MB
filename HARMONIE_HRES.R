library(here)
source('libraries.R')


## HARMONIE DATA
here::here('HARMONIE/ALBO2500-2019072900-B2019072900-HC.nc') %>% 
  open.nc() %>% print.nc()

HARMONIE<- here::here('HARMONIE/ALBO2500-2019072900-B2019072900-HC.nc') %>% 
  open.nc() %>% read.nc()

HARMONIE %>% View()


####HIGH RES DATA 
here::here('HARMONIE/HIGHRES3D_puertotarifa_2016100100_B2016100112H000_HC.nc') %>% 
  open.nc() %>% print.nc()


HRES<- here::here('HARMONIE/HIGHRES3D_puertotarifa_2016100100_B2016100112H000_HC.nc') %>% 
  open.nc() %>% read.nc()

HRES %>% View()
