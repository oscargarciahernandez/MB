library(here)
source(here::here('libraries.R'))


Belesar_files<- list.files(here::here('Data/Parques/Belesar'), full.names = T)
Belesar_ultimo<- Belesar_files[length(Belesar_files)] 


##Generar CSV Belesar
Belesar_CSV_Generator(Belesar_ultimo)

#Download data
Down_E001_Belesar()