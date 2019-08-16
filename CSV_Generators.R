library(here)
source(here::here('libraries.R'), echo=FALSE)

source(here::here('/Spain_CSV_Generator.R'), echo=FALSE)
source(here::here('/Actualizar_data_parques.R'), echo=FALSE)
source(here::here('/CSV_Generator_VientoMAX.R'), echo=FALSE)


tryCatch({source(here::here("/Belesar_Inflow_prediction_CSV_Generator.R"), echo=FALSE)}, 
         error= function(e){
           cat("Ejecutando de nuevo Belesar inflow prediction")
           source(here::here("/Belesar_Inflow_prediction_CSV_Generator.R"), echo=FALSE)})


source(here::here("/COMPROBACION_BELESAR.R"))

source(here::here('/FTP_UPLOAD.R'), echo=FALSE)





####LIMPIAR ENVIROMENT, EXCEPTO LAS FUNCIONES
rm(list = setdiff(ls(), lsf.str()))



