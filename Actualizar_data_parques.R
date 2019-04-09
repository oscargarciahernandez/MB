library(here)
source(here::here('libraries.R'))


# Actualizar data parques eólicos ---------------------------------------------------------
#Actualizar_Data_Parques()

All_files_Spain<- list.files(here::here('Data/Espana/'),
                             recursive = T, 
                             full.names = T)

d01_files<- All_files_Spain[!str_detect(All_files_Spain, "/d02/")]
RDS_files<- d01_files[str_detect(d01_files, ".RDS")]
RDS_files1<- RDS_files[!str_detect(RDS_files, "/NA/")]





Actualizar_Data_Parques_2(RDS_files1)
