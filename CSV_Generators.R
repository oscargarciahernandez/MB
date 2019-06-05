source('/home/asus/MB/Spain_CSV_Generator.R', echo=FALSE)
source('/home/asus/MB/Actualizar_data_parques.R', echo=FALSE)
source('/home/asus/MB/CSV_Generator_VientoMAX.R', echo=FALSE)

tryCatch({source('/home/asus/MB/Belesar_Inflow_prediction_CSV_Generator.R', echo=FALSE)}, 
         error= function(e){
           cat("Ejecutando de nuevo Belesar inflow prediction")
           source('/home/asus/MB/Belesar_Inflow_prediction_CSV_Generator.R', echo=FALSE)})


source('/home/asus/MB/FTP_UPLOAD.R', echo=FALSE)
source('/home/asus/MB/COMPROBACION_BELESAR.R')




####LIMPIAR ENVIROMENT, EXCEPTO LAS FUNCIONES
rm(list = setdiff(ls(), lsf.str()))



