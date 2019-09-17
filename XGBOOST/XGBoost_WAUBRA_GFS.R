library(here)
source(here::here('libraries.R'))
library(caret)
library(dplyr)
library(ggplot2)
library(glue)
library(ModelMetrics)
#library(OpenMPController) # for Kaggle backend
library(readr)
library(vtreat)
library(xgboost)
library(DescTools)

' 
PARAMERTROS PARA AJUSTAR 

LO QUE SE HARA SERÁ USAR UN LEARNING RATE ALTO PARA AJUSTAR LOS HYPERPARAMETROS 
LO CUAL ES COMPUTACIONALMENTE COSTOSO Y LUEGO CON LOS HIPERPARAMETROS AJUSTADOS 
USAMOS UN LEARNING RATE MAS BAJO

nrounds: Number of trees, default: 100
max_depth: Maximum tree depth, default: 6
eta: Learning rate, default: 0.3
gamma: Used for tuning of Regularization, default: 0
colsample_bytree: Column sampling, default: 1
min_child_weight: Minimum leaf weight, default: 1
subsample: Row sampling, default: 1
'


# ARREGLOS A LOS DATOS DE LA PRIMERA VEZ -------------------------------------------------------------
READ_CSV_AND_TRANSFORM<- FALSE
if(READ_CSV_AND_TRANSFORM){
  
  DATA_ALL<- here::here('Data/Parques/PRUEBA_EOLICOS/WAUBRA/HISTORICO_GFS.csv') %>% read_csv()
  
  DATA_ALL1<- DATA_ALL[,3:ncol(DATA_ALL)]
  DATA_ALL1 %>% View()
  
  LISTA_DATA<- DATA_ALL1 %>% mutate(VARIABLE= paste(VAR_NAME, LEVEL)) %>%
    group_by(VARIABLE) %>% group_split() %>% 
    lapply(function(x){
      NOMBRE_VAR<- x$VARIABLE %>% unique() %>% as.character() %>% str_replace_all(' ','')
      x[,NOMBRE_VAR]<- x$VALUES
      x[,c('VARIABLE', "VAR_NAME", "LEVEL", "VALUES")]<- NULL
      return(x)
    })
  
  library(plyr)
  TABLA_ALL<- join_all(LISTA_DATA, by=c("LON","LAT","DATE","FCST_TIME" ), type='left')
  
  library(dplyr)
  
  colnames(TABLA_ALL)<- c("LON","LAT","DATE","FCST_TIME","U10","V10","NANA",
                          "U100000","U80","U95000","U97500","V100000","V80",'V95000', "V97500" )
  
  TABLA_ALL$NANA<- NULL
  
  
  TABLA_ALL<- TABLA_ALL %>% mutate(WS10= sqrt(U10^2 + V10^2),
                                   WD10= atan2(U10/WS10, V10/WS10)* 180/pi +180,
                                   WS80= sqrt(U80^2 + V80^2),
                                   WD80= atan2(U80/WS80, V80/WS80)* 180/pi +180, 
                                   FCST_TIME= FCST_TIME %>% 
                                                 str_replace('fcst time', '') %>% 
                                                 str_replace('hrs', '') %>% as.numeric(),
                                   T_MODELO= DATE - as.difftime(FCST_TIME, units = 'hours'))
  
  PATH_WAUBRA<- here::here('Data/Parques/PRUEBA_EOLICOS/WAUBRA/')
  
  saveRDS(TABLA_ALL, paste0(PATH_WAUBRA, 'HISTORICO_TRANSFORMADO.RDS'))
  
}

ADD_PRODUCTION_DATA<- FALSE
if(ADD_PRODUCTION_DATA){
  DATA_ALL<- readRDS(paste0(PATH_WAUBRA, 'HISTORICO_TRANSFORMADO.RDS'))
  INFO_PARQUES<- readRDS(here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS'))
  INFO_WAUBRA<- INFO_PARQUES %>% filter(PARQUE=='P.E.Tatanka')
  
  DATA_ALL<- left_join(DATA_ALL, INFO_WAUBRA, by='DATE')
  
  DATA_ALL<- DATA_ALL %>% select(-c(PARQUE, CAPACIDAD, N_TURBINAS, LON.y, LAT.y))
  
  DATA_ALL<- DATA_ALL %>% dplyr::rename(LON = LON.x,
                                        LAT = LAT.x,
                                        P_MWH= PRUDUCCION_MWH,
                                        DISP= DISPONIBILIDAD)
  
  saveRDS(DATA_ALL, paste0(PATH_WAUBRA, 'HISTORICO_TRANSFORMADO_WITH_PRODUCCTION.RDS'))
}


# IMPORTAR DATOS ----------------------------------------------------------
PATH_WAUBRA<- here::here('Data/Parques/PRUEBA_EOLICOS/WAUBRA/')

DATA_ALL<- paste0(PATH_WAUBRA, 'HISTORICO_TRANSFORMADO_WITH_PRODUCCTION.RDS') %>%  readRDS()


MAX_COR_POINT<- DATA_ALL %>% group_by(LON, LAT) %>% group_split() %>% 
  sapply(function(x){
    print(cor(x$WS10,x$P_MWH, use = 'complete.obs'))
    cor(x$WS10,x$P_MWH, use = 'complete.obs')
  }) %>% which.max()

DATA_ONE_LOCATION<- DATA_ALL %>%  group_by(LON, LAT) %>% group_split() %>% .[[MAX_COR_POINT]]
DATA_ONE_LOCATION<-  DATA_ONE_LOCATION[DATA_ONE_LOCATION$FCST_TIME<24,] %>% .[order(.$DATE),]


DATA_ONE_LOCATION<- DATA_ONE_LOCATION %>% mutate(WS102= WS10^2,
                                                 WS103= WS10^3,
                                                 WS802= WS80^2,
                                                 WS803=WS80^3,
                                                 HORA= DATE %>% hour(),
                                                 COSH= cos(2*pi*HORA/24),
                                                 PLAG1= P_MWH %>% shift(1),
                                                 PLAG2= P_MWH %>% shift(2),
                                                 PLAG3= P_MWH %>% shift(3),
                                                 PLAG4= P_MWH %>% shift(4))



# SEPARAMOS ENTRENAMIENTO Y TEST ------------------------------------------
DATA_ONE_LOCATION<- DATA_ONE_LOCATION %>% .[complete.cases(.),]
TEST_TRAIN_FACTOR<- 1.1
N_DATOS<-DATA_ONE_LOCATION %>% nrow()
DATA_TRAIN<- DATA_ONE_LOCATION[1:((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)),]
DATA_TEST<- DATA_ONE_LOCATION[((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)):N_DATOS,]

# VALORES POR DEFECTO DEL XGBOOST  ----------------------------------------

grid_default <- expand.grid(
  nrounds = 500,
  max_depth = 10,
  eta = 0.4,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)


# CREAMOS LAS COMBINACIONES POSIBLES DE VARIABLES -------------------------
VECTOR_COLS<-DATA_ONE_LOCATION %>% colnames() %>% .[!(.%in%c("LON","LAT","DATE","FCST_TIME", 'P_MWH','DISP'))]
VECTOR_COLS1<- VECTOR_COLS %>% .[str_detect(., 'WS|WD')]

TABLA_VARIABLES<- expand.grid(lapply(VECTOR_COLS1, function(x) c(x, NA)))
library(stringr)


for(i in 1:nrow(TABLA_VARIABLES)){
  
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist()
  
  
  tryCatch({
    
    PATH_MODELOS<- here::here('XGBOOST/GFS_WAUBRA/')
    if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS, recursive = TRUE)}
    
    NOMBRE_BASE<- paste(VARIABLES_MODELO, collapse = '_')
    
    if(file.exists(paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))){
      print(paste('YA EXISTE', NOMBRE_BASE))
    }else{
      
      
      # CREAMOS MODELO LINEAL DE REFERENCIA -------------------------------------
      
      
      linear_base <- lm(paste0("P_MWH ~ ", paste(VARIABLES_MODELO, collapse = ' + ')),data = DATA_TRAIN)
      
      # XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------
      
      input_x<- DATA_TRAIN[,VARIABLES_MODELO]
      input_y<- DATA_TRAIN[,"P_MWH"]$P_MWH
      
      xgb_base <- caret::train(
        x = input_x,
        y = input_y,
        trControl = train_control,
        tuneGrid = grid_default,
        method = "xgbTree",
        verbose = TRUE
      )
      
      predict(xgb_base, DATA_TEST[, VARIABLES_MODELO])
      
      
      saveRDS(linear_base, paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))
      saveRDS(xgb_base, paste0(PATH_MODELOS, NOMBRE_BASE,'_xgbBase.RDS'))
      
      
      
    }
    
    
  }, error= function(e){
    print(e)
  })
}


# TEST DATA ---------------------------------------------------------------
ALL_MODELS<-here::here('XGBOOST/GFS_WAUBRA/') %>% list.files(full.names = TRUE)
LINEAR_MODELS<- ALL_MODELS %>% .[str_detect(., '_linear')]
XGBbase_MODELS<- ALL_MODELS %>% .[str_detect(., '_xgbBase')]




LISTA_VARIABLES<- list()
for(i in 1:nrow(TABLA_VARIABLES)){
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist()
  LISTA_VARIABLES[[paste(VARIABLES_MODELO, collapse = '_')]]<- VARIABLES_MODELO
}




library(forecast)


# CREAMOS TABLA ACCURACY PARA LOS MODELOS LINEALES ------------------------
TABLA_ACCURACY_LINEAR<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_LINEAR)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(LINEAR_MODELS)){
  
  MODELO_LINEAR<- LINEAR_MODELS[i] %>% readRDS()
  
  
  NOMBRE_MODELO<- LINEAR_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_linear.RDS')
  
  VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
  
  
  holdout_x <- DATA_TEST[,VARIABLES_MODELO]
  holdout_y <-  DATA_TEST[,"P_MWH"]$P_MWH
  MODELOS_CORRUPTOS<- tryCatch({
    
    TABLA_ACCURACY_LINEAR[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_LINEAR,newdata = holdout_x),
                                                         holdout_y),
                                  cor(predict(MODELO_LINEAR, newdata = holdout_x),
                                      DATA_TEST$P_MWH,use= 'complete.obs'))
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(LINEAR_MODELS[i])
  })
}


# CREAMOS TABLA ACCURACY PARA LOS XGBOOST ---------------------------------
TABLA_ACCURACY_XGBbase<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_XGBbase)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(XGBbase_MODELS)){
  
  #CARGAMOS EL MODELO
  MODELO_XGBbase<- XGBbase_MODELS[i] %>% readRDS()
  
  #SACAMOS EL NOMBRE DEL MODELO PARA INCLUIR EN LA TABLA
  NOMBRE_MODELO<- XGBbase_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_xgbBase.RDS')
  
  #OBTENEMOS LAS VARIABLES QUE PERTENECEN AL MODELO
  VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
  
  
  #TEST DATA
  holdout_x <- DATA_TEST[,VARIABLES_MODELO]
  holdout_y <-  DATA_TEST[,"P_MWH"]$P_MWH
  MODELOS_CORRUPTOS<- tryCatch({
    
    TABLA_ACCURACY_XGBbase[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_XGBbase,newdata = holdout_x),
                                                          holdout_y),
                                   cor(predict(MODELO_XGBbase, newdata = holdout_x),
                                       DATA_TEST$P_MWH,use= 'complete.obs'))
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(XGBbase_MODELS[i])
  })
}

# JUNTAMOS LAS TABLAS ACCURACY DE AMBOS MODELOS ---------------------------

TABLA_ACCURACY_LINEAR$METHOD<- 'LINEAR'
TABLA_ACCURACY_XGBbase$METHOD<- 'XGB_BASE'


#JUNTAMOS LOS DATASETS Y COMPROBAMOS RESULTADS MIRANDO MEDIA MAXIMA Y MINIMA DE LAS COLUMAS IMPORTANTES
TABLA_ACCURACY<- list(TABLA_ACCURACY_LINEAR,TABLA_ACCURACY_XGBbase) %>% bind_rows()
TABLA_ACCURACY[, c(2:4,7)] <- lapply(TABLA_ACCURACY[, c(2:4,7)], function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})

aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), mean)
aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), max)
aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), min)

TABLA_ACCURACY1<- TABLA_ACCURACY

# MIRAMOS CUALES SON LAS VARIABLES MAS REPETIDAS --------------------------
X<- 1
REPEATED_VARIABLES<- TABLA_ACCURACY1[order(TABLA_ACCURACY1$RMSE),][1:X,1] %>% 
  str_split('_') %>% unlist() %>% table()

REPEATED_VARIABLES %>% barplot() 





# SEGUNDA TANDA DE VARIABLES A PROBAR -------------------------------------
#COJEMOS LOS MEJORES PREDICTORES Y PASAMOS LA SEGUNDA RONDA DE AFINACION

VECTOR_COLS2<- VECTOR_COLS[!(VECTOR_COLS %in% VECTOR_COLS1)] %>% .[!(.%in%c('T_MODELO','P_MWH','DISP',
                                                                            'HORA','COSH','PLAG1',
                                                                            'PLAG2','PLAG3','PLAG4'))]
BEST_VARS1<- REPEATED_VARIABLES %>% names()

TABLA_VARIABLES<-  expand.grid((lapply(VECTOR_COLS2, function(x) c(x, NA)))) 

for(i in 1:nrow(TABLA_VARIABLES)){
  
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist() %>% c(BEST_VARS1)
  
  
  tryCatch({
    
    PATH_MODELOS<- here::here('XGBOOST/GFS_WAUBRA/')
    if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS, recursive = TRUE)}
    
    NOMBRE_BASE<- paste(VARIABLES_MODELO, collapse = '_')
    
    if(file.exists(paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))){
      print(paste('YA EXISTE', NOMBRE_BASE))
    }else{
      
      
      # CREAMOS MODELO LINEAL DE REFERENCIA -------------------------------------
      
      
      linear_base <- lm(paste0("P_MWH ~ ", paste(VARIABLES_MODELO, collapse = ' + ')),data = DATA_TRAIN)
      
      # XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------
      
      input_x<- DATA_TRAIN[,VARIABLES_MODELO]
      input_y<- DATA_TRAIN[,"P_MWH"]$P_MWH
      
      xgb_base <- caret::train(
        x = input_x,
        y = input_y,
        trControl = train_control,
        tuneGrid = grid_default,
        method = "xgbTree",
        verbose = TRUE
      )
      
      predict(xgb_base, DATA_TEST[, VARIABLES_MODELO])
      
      
      saveRDS(linear_base, paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))
      saveRDS(xgb_base, paste0(PATH_MODELOS, NOMBRE_BASE,'_xgbBase.RDS'))
      
      
      
    }
    
    
  }, error= function(e){
    print(e)
  })
}


# TEST DATA ---------------------------------------------------------------
ALL_MODELS<-here::here('XGBOOST/GFS_WAUBRA/') %>% list.files(full.names = TRUE)
LINEAR_MODELS<- ALL_MODELS %>% .[str_detect(., '_linear')]
XGBbase_MODELS<- ALL_MODELS %>% .[str_detect(., '_xgbBase')]




LISTA_VARIABLES<- list()
for(i in 1:nrow(TABLA_VARIABLES)){
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist() %>% c(BEST_VARS1)
  LISTA_VARIABLES[[paste(VARIABLES_MODELO, collapse = '_')]]<- VARIABLES_MODELO
}




library(forecast)


# CREAMOS TABLA ACCURACY PARA LOS MODELOS LINEALES ------------------------
TABLA_ACCURACY_LINEAR<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_LINEAR)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(LINEAR_MODELS)){
  
  MODELO_LINEAR<- LINEAR_MODELS[i] %>% readRDS()
  
  
  NOMBRE_MODELO<- LINEAR_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_linear.RDS')
  
  MODELOS_CORRUPTOS<- tryCatch({
    
    VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
    
    
    holdout_x <- DATA_TEST[,VARIABLES_MODELO]
    holdout_y <-  DATA_TEST[,"P_MWH"]$P_MWH
    TABLA_ACCURACY_LINEAR[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_LINEAR,newdata = holdout_x),
                                                         holdout_y),
                                  cor(predict(MODELO_LINEAR, newdata = holdout_x),
                                      DATA_TEST$P_MWH,use= 'complete.obs'))
    #TABLA_ACCURACY_LINEAR$MAPE[i]<- MAPE(predict(MODELO_LINEAR,newdata = holdout_x),holdout_y)
    
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(LINEAR_MODELS[i])
  })
}


# CREAMOS TABLA ACCURACY PARA LOS XGBOOST ---------------------------------
TABLA_ACCURACY_XGBbase<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_XGBbase)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(XGBbase_MODELS)){
  
  #CARGAMOS EL MODELO
  MODELO_XGBbase<- XGBbase_MODELS[i] %>% readRDS()
  
  #SACAMOS EL NOMBRE DEL MODELO PARA INCLUIR EN LA TABLA
  NOMBRE_MODELO<- XGBbase_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_xgbBase.RDS')
  
  
  MODELOS_CORRUPTOS<- tryCatch({
    #OBTENEMOS LAS VARIABLES QUE PERTENECEN AL MODELO
    VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
    
    
    #TEST DATA
    holdout_x <- DATA_TEST[,VARIABLES_MODELO]
    holdout_y <-  DATA_TEST[,"P_MWH"]$P_MWH
    TABLA_ACCURACY_XGBbase[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_XGBbase,newdata = holdout_x),
                                                          holdout_y),
                                   cor(predict(MODELO_XGBbase, newdata = holdout_x),
                                       DATA_TEST$P_MWH,use= 'complete.obs'))
    #TABLA_ACCURACY_LINEAR$MAPE[i]<- MAPE(predict(MODELO_XGBbase,newdata = holdout_x),holdout_y)
    
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(XGBbase_MODELS[i])
  })
}

# JUNTAMOS LAS TABLAS ACCURACY DE AMBOS MODELOS ---------------------------

TABLA_ACCURACY_LINEAR$METHOD<- 'LINEAR'
TABLA_ACCURACY_XGBbase$METHOD<- 'XGB_BASE'


#JUNTAMOS LOS DATASETS Y COMPROBAMOS RESULTADS MIRANDO MEDIA MAXIMA Y MINIMA DE LAS COLUMAS IMPORTANTES
TABLA_ACCURACY<- list(TABLA_ACCURACY_LINEAR,TABLA_ACCURACY_XGBbase) %>% bind_rows()
TABLA_ACCURACY<- TABLA_ACCURACY %>% .[complete.cases(.), ]
TABLA_ACCURACY[, c(2:7)] <- lapply(TABLA_ACCURACY[, c(2:7)], function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})

aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), mean)
aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), max)
aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), min)

TABLA_ACCURACY2<- TABLA_ACCURACY

# VARIABLES REPETIDAS -----------------------------------------------------
X<- 1
REPEATED_VARIABLES<- TABLA_ACCURACY2[order(TABLA_ACCURACY2$RMSE),][1:X,1] %>% 
  str_split('_') %>% unlist() %>% table()

REPEATED_VARIABLES %>% barplot() 





# TERCERA TANDA DE VARIABLES A PROBAR -------------------------------------
#COJEMOS LOS MEJORES PREDICTORES Y PASAMOS LA SEGUNDA RONDA DE AFINACION

VECTOR_COLS3<- VECTOR_COLS[!(VECTOR_COLS %in% VECTOR_COLS1)] %>% .[!.%in% VECTOR_COLS2]
BEST_VARS2<- REPEATED_VARIABLES %>% names()

TABLA_VARIABLES<-  expand.grid((lapply(VECTOR_COLS3, function(x) c(x, NA)))) 

for(i in 1:nrow(TABLA_VARIABLES)){
  
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist() %>% c(BEST_VARS2)
  
  
  tryCatch({
    
    PATH_MODELOS<- here::here('XGBOOST/GFS_WAUBRA/')
    if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS, recursive = TRUE)}
    
    NOMBRE_BASE<- paste(VARIABLES_MODELO, collapse = '_')
    
    if(file.exists(paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))){
      print(paste('YA EXISTE', NOMBRE_BASE))
    }else{
      
      
      # CREAMOS MODELO LINEAL DE REFERENCIA -------------------------------------
      
      
      linear_base <- lm(paste0("P_MWH ~ ", paste(VARIABLES_MODELO, collapse = ' + ')),data = DATA_TRAIN)
      
      # XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------
      
      input_x<- DATA_TRAIN[,VARIABLES_MODELO]
      input_y<- DATA_TRAIN[,"P_MWH"]$P_MWH
      
      xgb_base <- caret::train(
        x = input_x,
        y = input_y,
        trControl = train_control,
        tuneGrid = grid_default,
        method = "xgbTree",
        verbose = TRUE
      )
      
      saveRDS(linear_base, paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))
      saveRDS(xgb_base, paste0(PATH_MODELOS, NOMBRE_BASE,'_xgbBase.RDS'))
      
      
      
    }
    
    
  }, error= function(e){
    print(e)
  })
}


# TEST DATA ---------------------------------------------------------------
ALL_MODELS<-here::here('XGBOOST/GFS_WAUBRA/') %>% list.files(full.names = TRUE)
LINEAR_MODELS<- ALL_MODELS %>% .[str_detect(., '_linear')]
XGBbase_MODELS<- ALL_MODELS %>% .[str_detect(., '_xgbBase')]




LISTA_VARIABLES<- list()
for(i in 1:nrow(TABLA_VARIABLES)){
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist() %>% c(BEST_VARS2)
  LISTA_VARIABLES[[paste(VARIABLES_MODELO, collapse = '_')]]<- VARIABLES_MODELO
}




library(forecast)


# CREAMOS TABLA ACCURACY PARA LOS MODELOS LINEALES ------------------------
TABLA_ACCURACY_LINEAR<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_LINEAR)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(LINEAR_MODELS)){
  
  MODELO_LINEAR<- LINEAR_MODELS[i] %>% readRDS()
  
  
  NOMBRE_MODELO<- LINEAR_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_linear.RDS')

  MODELOS_CORRUPTOS<- tryCatch({
    
    VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
    
    
    holdout_x <- DATA_TEST[,VARIABLES_MODELO]
    holdout_y <-  DATA_TEST[,"P_MWH"]$P_MWH
    TABLA_ACCURACY_LINEAR[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_LINEAR,newdata = holdout_x),
                                                         holdout_y),
                                  cor(predict(MODELO_LINEAR, newdata = holdout_x),
                                      DATA_TEST$P_MWH,use= 'complete.obs'))
    
    
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(LINEAR_MODELS[i])
  })
}


# CREAMOS TABLA ACCURACY PARA LOS XGBOOST ---------------------------------
TABLA_ACCURACY_XGBbase<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_XGBbase)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(XGBbase_MODELS)){
  
  #CARGAMOS EL MODELO
  MODELO_XGBbase<- XGBbase_MODELS[i] %>% readRDS()
  
  #SACAMOS EL NOMBRE DEL MODELO PARA INCLUIR EN LA TABLA
  NOMBRE_MODELO<- XGBbase_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_xgbBase.RDS')
  

  MODELOS_CORRUPTOS<- tryCatch({
    #OBTENEMOS LAS VARIABLES QUE PERTENECEN AL MODELO
    VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
    
    
    #TEST DATA
    holdout_x <- DATA_TEST[,VARIABLES_MODELO]
    holdout_y <-  DATA_TEST[,"P_MWH"]$P_MWH
    TABLA_ACCURACY_XGBbase[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_XGBbase,newdata = holdout_x),
                                                          holdout_y),
                                   cor(predict(MODELO_XGBbase, newdata = holdout_x),
                                       DATA_TEST$P_MWH,use= 'complete.obs'))
    
    
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(XGBbase_MODELS[i])
  })
}

# JUNTAMOS LAS TABLAS ACCURACY DE AMBOS MODELOS ---------------------------

TABLA_ACCURACY_LINEAR$METHOD<- 'LINEAR'
TABLA_ACCURACY_XGBbase$METHOD<- 'XGB_BASE'


#JUNTAMOS LOS DATASETS Y COMPROBAMOS RESULTADS MIRANDO MEDIA MAXIMA Y MINIMA DE LAS COLUMAS IMPORTANTES
TABLA_ACCURACY<- list(TABLA_ACCURACY_LINEAR,TABLA_ACCURACY_XGBbase) %>% bind_rows()
TABLA_ACCURACY<- TABLA_ACCURACY %>% .[complete.cases(.),]

TABLA_ACCURACY[, c(2:7)] <- lapply(TABLA_ACCURACY[, c(2:7)], function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})

aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), mean)
aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), max)
aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), min)

TABLA_ACCURACY3<- TABLA_ACCURACY

# VARIABLES REPETIDAS -----------------------------------------------------
X<- 1
REPEATED_VARIABLES<- TABLA_ACCURACY[order(TABLA_ACCURACY$RMSE),][1:X,1] %>% 
  str_split('_') %>% unlist() %>% table()

REPEATED_VARIABLES %>% barplot() 

