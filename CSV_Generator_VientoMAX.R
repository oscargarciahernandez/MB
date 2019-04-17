library(here)
source(here::here('libraries.R'))





CSV_Generator_VientoMAX<- function(RDS_parque){
  parq_data<- readRDS(RDS_parque)
  parq_lolat<- lon_lat_df_ls(parq_data)
  parq_lolat1<- lapply(parq_lolat, uv_transformation)
  parq_gust<- lapply(parq_lolat1, function(x){
    table<- as.data.frame(cbind(x$fechas,x$lon,x$lat,x$G10_MAX))
    colnames(table)<- c('Date', "LON", "LAT", "GUST")
    return(table)
  })
  
  table_gust<- data.frame(matrix(ncol=length(parq_gust)+1, nrow = length(parq_gust[[1]]$Date)+2))
  
  for (i in 1:length(parq_gust)) {
    table_gust[1,i+1]<- unique(parq_gust[[i]]$LON)
    table_gust[2,i+1]<- unique(parq_gust[[i]]$LAT)
    table_gust[3:(length(parq_gust[[i]]$Date)+2),i+1]<- parq_gust[[i]]$GUST
  }
  
  
  table_gust[3:(length(parq_gust[[i]]$Date)+2),1]<- as.character(as.POSIXct(parq_gust[[1]]$Date
                                                                            ,origin = "1970-01-01",tz = "UTC"))
  
  table_gust[1,1]<- "LON"
  table_gust[2,1]<- "LAT"
  
  
  round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  Tabla_final<- round_df(table_gust, 4)
  
  
  
  path2<- RDS_parque %>% str_split('/') %>% unlist
  path1<-path2[1:length(path2)-1] 
  path<- paste(path1, collapse = "/")
  
  pathcsv<- paste0(path,"CSV/")
  
  if(!dir.exists(pathcsv)){dir.create(pathcsv)}
  
  nombre<- paste0(str_remove_all(path2[length(path2)],".RDS"), ".CSV")
  
  path_nombre<- paste0(pathcsv,nombre)
  
  
  write.table(Tabla_final, path_nombre, 
              sep = ";",
              dec = ".", 
              row.names = F,
              col.names = F,
              quote = F)

}

ElCerro<- list.files(here::here('Data/Parques/ElCerro/'), full.names = T) %>% .[str_detect(., ".RDS")]
ElCerro_ultimo<- ElCerro[length(ElCerro)]
CSV_Generator_VientoMAX(ElCerro_ultimo)


Lubian<- list.files(here::here('Data/Parques/Lubian/'), full.names = T) %>% .[str_detect(., ".RDS")]
Lubian_ultimo<- Lubian[length(Lubian)]
CSV_Generator_VientoMAX(Lubian_ultimo)


LaSia<- list.files(here::here('Data/Parques/LaSia/'), full.names = T) %>% .[str_detect(., ".RDS")]
LaSia_ultimo<- ElCerro[length(LaSia)]
CSV_Generator_VientoMAX(LaSia_ultimo)

