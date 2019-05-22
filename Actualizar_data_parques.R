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

'
Historico<- 148:154
RDS_historico<- RDS_files1[Historico]
'
### PARA REALIZAR SOLAMENTE LOS DE HOY
RDS_hoy<- now() %>% as.Date() %>% as.character() %>% str_replace_all("-","")%>% str_detect(RDS_files1, . )%>% RDS_files[.]



### PARA REALIZAR LOS DE LA ULTIMA SEMANA 
RDS_last7days<- RDS_files1 %>% .[(length(.)-7):length(.)]

tryCatch({Actualizar_Data_Parques_2(RDS_hoy)}, error=function(e){
  cat("NO HAY ARCHIVOS DE HOY, PASAMOS A REVISAR LOS FICHEROS DE LA ULTIMA SEMANA")
  Actualizar_Data_Parques_2(RDS_last7days)
})
