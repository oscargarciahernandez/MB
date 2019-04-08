library(here)
source(here::here('libraries.R'))


Belesar_files<- list.files(here::here('Data/Parques/Belesar'), full.names = T)
Belesar_files1<- Belesar_files[!str_detect(Belesar_files, "E001")]
Belesar_files2<- Belesar_files1[str_detect(Belesar_files1,".RDS")]
Belesar_ultimo<- Belesar_files2[length(Belesar_files2)] 





#Para generar todas los CSV's
#lapply(Belesar_files2, Belesar_CSV_Generator)

##Generar CSV Belesar
Belesar_CSV_Generator(Belesar_ultimo)



#Para generar todas los CSV's
#lapply(Belesar_files2, Belesar_CSV_Generator_Point,point = 19)
#Geenrar CSV_Belesar
Belesar_CSV_Generator_Point<- function(Belesar_ultimo, point){
  Belesar_data<- readRDS(Belesar_ultimo)
  Belesar_lolat<- lon_lat_df_ls(Belesar_data)
  Belesar_lolat1<- lapply(Belesar_lolat, uv_transformation)
  Belesar_rain<- lapply(Belesar_lolat1, extract_rain_data)
  
  nombres<- names(Belesar_rain)
  precipitacion_horaria<- lapply(Belesar_rain, function(x) x$prep_hourly)
  tabla_precip<- data.frame(matrix(nrow = length(precipitacion_horaria$`-8.02328491210938__42.1343421936035`)))
  for (i in 1:length(precipitacion_horaria)) {
    tabla_precip<- as.data.frame(cbind(tabla_precip,precipitacion_horaria[[i]]))
  }
  
  tabla_precip[,1]<- Belesar_rain[[1]]$fechas
  
  colnames(tabla_precip)<- c("Date", nombres)
  path<- here::here('Data/Parques/Belesar/')
  nombre1<- str_split(Belesar_ultimo, "/")
  nombre2<- nombre1[[1]][length(nombre1[[1]])]
  nombre3<- str_remove(nombre2 ,".RDS")
  
  path_nombre<-paste0(path,nombre3,"_",point,".CSV")
  
  tabla_precipita_localizacion<- tabla_precip[,c(1,point)]
  
  
  write.table(tabla_precipita_localizacion, path_nombre, 
              sep = ";",
              dec = ".", 
              row.names = F,
              quote = F)
  
}




#Download data
Down_E001_Belesar()