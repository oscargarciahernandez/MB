library(here)
source(here::here('libraries.R'))


# READ CSV ----------------------------------------------------------------
csvs<- here::here('SISTEMA_ELECTRICO/') %>% list.files(full.names = T)
lista_se<- list()

for (csv in 1:length(csvs)) {
  
  x<-  csvs%>% .[csv] %>%   
    read.csv(sep = ";", header = T)
  
  x$geoid<- NULL
  x$geoname<- NULL
  
  x_list<- x %>% group_split(name)
  names(x_list)<- x_list %>%  sapply(function(y) y$name %>% unique) %>% as.character()
  
  lista_se[[csv]]<- x_list
  
}



# DEMANDA -----------------------------------------------------------------
#TRATAMIENTO DEMANDA 
tabla_demanda<- lista_se[[1]] %>% .[1:3] %>% bind_rows() %>%  
  group_by(name) %>% do(.[!duplicated(.$datetime), ]) %>% ungroup()


tabla_demanda2<- tabla_demanda %>% group_split(name) %>% bind_cols()

names_val<- c(tabla_demanda2$name %>% unique() %>% as.character(),
  tabla_demanda2$name1 %>% unique() %>% as.character(),
  tabla_demanda2$name2 %>% unique() %>% as.character(),
  "Date") 

tabla_demanda2[,c("id1", "id", "id2", 
                  "datetime","datetime1",
                  "name","name1","name2")]<- NULL

colnames(tabla_demanda2)<- names_val

DATETIME<- tabla_demanda2$Date %>% str_split("T|[+]") %>% lapply(function(y){
  DateUTC<- y %>%  .[1:2] %>% paste(collapse = " ") %>% ymd_hms()
  hour_diff<- y  %>%  .[3] %>% hm()
  DateUTC+hour_diff 
}) 


tabla_demanda2$Date<- DATETIME %>%  do.call("c", . )

saveRDS(tabla_demanda2, here::here('SISTEMA_ELECTRICO/TABLA_DEMANDAS.RDS'))


# GENERACION EÓLICA, CICLO COMBINADO Y SOLAR ------------------------------
#TRATAMIENTO VARIABLES PREVISION SOLAR, EOLICA Y CICLO COMBINADO
#NOTA: LOS DATOS DE EOLICA Y SOLAR ESTAN EN MWH Y LOS DATOS DE CICLO COMBINADO ESTÁN 
# EN MW
tabla_variables<- lista_se[c(2:7,9)]  %>% lapply(function(x) x[[1]])  %>%    bind_rows() %>% 
  group_by(name) %>% do(.[!duplicated(.$datetime), ]) %>% ungroup()


tabla_variables2<- tabla_variables %>% group_split(name) 

#PREVISIONES SOLAR Y EÓLICA
Previsiones<- tabla_variables2[c(1,7)] %>% bind_cols()
names_val<- c(Previsiones$name %>% unique() %>% as.character(),
              Previsiones$name1 %>% unique() %>% as.character(),
              "Date") 

Previsiones[,c("id1", "id",
                  "datetime",
                  "name","name1")]<- NULL

colnames(Previsiones)<- names_val

DATETIME<- Previsiones$Date %>% str_split("T|[+]") %>% lapply(function(y){
  DateUTC<- y %>%  .[1:2] %>% paste(collapse = " ") %>% ymd_hms()
  hour_diff<- y  %>%  .[3] %>% hm()
  DateUTC+hour_diff 
}) 
Previsiones$Date<- DATETIME %>%  do.call("c", . )

saveRDS(Previsiones, here::here('SISTEMA_ELECTRICO/PREVISIONES_EOLICASOLAR.RDS'))

#MEDICIONES REALES
Treal<- tabla_variables2[c(2,3,4,5,6)] %>% bind_cols()

names_val<- c(Treal$name %>% unique() %>% as.character(),
              Treal$name1 %>% unique() %>% as.character(),
              Treal$name2 %>% unique() %>% as.character(),
              Treal$name3 %>% unique() %>% as.character(),
              Treal$name4 %>% unique() %>% as.character(),
              "Date") 

Treal[,c("id1", "id","id2", "id3", "id4",
               "datetime","datetime1","datetime2", "datetime3",
               "name","name1", "name2", "name3", "name4")]<- NULL

colnames(Treal)<- names_val

DATETIME<- Treal$Date %>% str_split("T|[+]") %>% lapply(function(y){
  DateUTC<- y %>%  .[1:2] %>% paste(collapse = " ") %>% ymd_hms()
  hour_diff<- y  %>%  .[3] %>% hm()
  DateUTC+hour_diff 
}) 
Treal$Date<- DATETIME %>%  do.call("c", . )

saveRDS(Treal, here::here('SISTEMA_ELECTRICO/MEDICIONESTREAL.RDS'))



# CREACMOS TABLA PRECIO DIARIO E INTRADIARIO  -----------------------------
tabla_spot<- lista_se[[4]]  %>% bind_rows() %>% 
  group_by(name) %>% do(.[!duplicated(.$datetime), ]) %>% ungroup()


tabla_diaria<- tabla_spot %>% group_split(name) %>% .[[1]]
tabla_intradiario<- diaria<- tabla_spot %>% group_split(name) %>% .[[2]]
tabla_spot2<- left_join(tabla_diaria, tabla_intradiario, by="datetime")

names_val<- c(tabla_spot2$name.x %>% unique() %>% as.character(),"Date",
              tabla_spot2$name.y %>% unique() %>% as.character()) %>% .[1:3]

tabla_spot2[,c("id.x","id.y",
               "name.x","name.y")]<- NULL

colnames(tabla_spot2)<- names_val

DATETIME<- tabla_spot2$Date %>% str_split("T|[+]") %>% lapply(function(y){
  DateUTC<- y %>%  .[1:2] %>% paste(collapse = " ") %>% ymd_hms()
  hour_diff<- y  %>%  .[3] %>% hm()
  DateUTC+hour_diff 
}) 



tabla_spot2$Date<- DATETIME %>%  do.call("c", . )

saveRDS(tabla_spot2, here::here('SISTEMA_ELECTRICO/PRECIOS.RDS'))



rm(list = setdiff(ls(), lsf.str()))

# JUNTAMOS TODOS LOS DATOS -----------------------------------
RDS_files<- here::here('SISTEMA_ELECTRICO/') %>% list.files(full.names = T) %>% 
  .[str_detect(., ".RDS")]

MEDICIONES_REALES<- RDS_files[1] %>% readRDS()
PRECIOS<- RDS_files[2] %>% readRDS()
PREVISIONES_EOLICASOLAR<- RDS_files[3] %>% readRDS()
DEMANDAS<- RDS_files[4] %>% readRDS()

JOINING<- left_join(DEMANDAS, MEDICIONES_REALES, by="Date")
MEDIAS_HORARIAS<- JOINING %>% group_by(year(Date),yday(Date),hour(Date)) %>% 
  summarise_all(funs(if(is.numeric(.)) mean(.)  else .[length(.)]))

MEDIAS_HORARIAS[,1:3]<- NULL
MEDIAS_HORARIAS$Date<- MEDIAS_HORARIAS$Date %>% round_date(unit = "hour")

View(MEDIAS_HORARIAS)
JOINING2<- left_join(MEDIAS_HORARIAS, PRECIOS, by="Date") 



JOINING3<- left_join(JOINING2, PREVISIONES_EOLICASOLAR, by="Date") %>% .[complete.cases(.), ]

colnames(JOINING3)<- colnames(JOINING3) %>% str_replace_all(" ","_") %>% 
  str_replace_all("ó","o") %>% str_replace_all("[.]","") %>% 
  str_replace_all("á","a") %>% str_replace_all("Generacion","Gen") %>% 
  str_replace_all("Prevision","prev") %>% str_replace_all("[.]","")

saveRDS(JOINING3, here::here('SISTEMA_ELECTRICO/TODAS_VARIABLES.RDS'))




# AHORA A INVENTAR --------------------------------------------------------

JOINING3 %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=prev_de_la_produccion_eolica_nacional_peninsular)) +
  geom_line(aes(y=Gen_TReal_eolica), color= "red", alpha= 0.4)+
  theme_light()



