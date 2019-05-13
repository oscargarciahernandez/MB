source('/home/asus/MB/Spain_CSV_Generator.R', echo=TRUE)
source('/home/asus/MB/Actualizar_data_parques.R', echo=TRUE)
source('/home/asus/MB/CSV_Generator_VientoMAX.R', echo=TRUE)

tryCatch({source('/home/asus/MB/Belesar_Inflow_prediction_CSV_Generator.R', echo=TRUE)}, 
         error= function(){
           cat("Ejecutando de nuevo Belesar inflow prediction")
           source('/home/asus/MB/Belesar_Inflow_prediction_CSV_Generator.R', echo=TRUE)})


source('/home/asus/MB/FTP_UPLOAD.R', echo=TRUE)





####LIMPIAR ENVIROMENT, EXCEPTO LAS FUNCIONES
rm(list = setdiff(ls(), lsf.str()))



