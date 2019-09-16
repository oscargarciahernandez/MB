library(caret)
library(dplyr)
library(ggplot2)
library(glue)
library(ModelMetrics)
#library(OpenMPController) # for Kaggle backend
library(readr)
library(vtreat)
library(xgboost)

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


# IMPORT DATA -------------------------------------------------------------

DATA_ALL<- here::here('Data/Parques/PRUEBA_EOLICOS/CERROBLANCO/HIRLAM/HISTORICO_HIRLAM_INTERPOLATED_WITH_PRUDCTION.csv') %>% read_csv()


MAX_COR_POINT<- DATA_ALL %>% group_by(LON.x, LAT.x) %>% group_split() %>% 
  sapply(function(x){
    cor(x$WS10,x$PRUDUCCION_MWH, use = 'complete.obs')
  }) %>% which.max()

DATA_ONE_LOCATION<- DATA_ALL %>%  group_by(LON.x, LAT.x) %>% group_split() %>% .[[MAX_COR_POINT]]
DATA_ONE_LOCATION<-  DATA_ONE_LOCATION[DATA_ONE_LOCATION$FCST_TIME<24,]


DATA_ONE_LOCATION<- DATA_ONE_LOCATION %>% mutate(WS10_2= WS10^2,
                                                 WS10_3= WS10^3,
                                                 WS100_2= WS100^2,
                                                 WS100_3= WS100^3)

TEST_TRAIN_FACTOR<- 1.1
N_DATOS<-DATA_ONE_LOCATION %>% nrow()
DATA_TRAIN<- DATA_ONE_LOCATION[1:((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)),]
DATA_TEST<- DATA_ONE_LOCATION[((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)):N_DATOS,]

# VALORES POR DEFECTO DEL XGBOOST  ----------------------------------------

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
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

GV1<- c( paste('WS100', 'WD100', collapse = '_'), NA)
GV2<- c( paste('U100', 'V100', collapse = '_'), NA)

GV3<-  c( paste('WS10', 'WD10', collapse = '_'), NA)
GV4<- c( paste('U10', 'V10', collapse = '_'), NA)

GV5<- c('FCST_TIME', NA)

GV6<- c('WS10_2', NA)

GV7<- c('WS10_3', NA)

GV8<- c('WS100_2', NA)

GV9<- c('WS100_3', NA)

TABLA_VARIABLES<- expand.grid(GV1, GV2, GV3,GV4, GV5, GV6, GV7, GV8, GV9)
library(stringr)


for(i in 1:nrow(TABLA_VARIABLES)){
  
  
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist()
  
  
  tryCatch({
    
    PATH_MODELOS<- here::here('XGBOOST/HIRLAM_CERROBLANCO/')
    if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS, recursive = TRUE)}
    
    NOMBRE_BASE<- paste(VARIABLES_MODELO, collapse = '_')
    
    if(file.exists(paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))){
      print(paste('YA EXISTE', NOMBRE_BASE))
    }else{
      
      
      # CREAMOS MODELO LINEAL DE REFERENCIA -------------------------------------
      
      
      linear_base <- lm(paste0("PRUDUCCION_MWH ~ ", paste(VARIABLES_MODELO, collapse = ' + ')),data = DATA_TRAIN)
      
      # XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------
      
      input_x<- DATA_TRAIN[,VARIABLES_MODELO]
      input_y<- DATA_TRAIN[,"PRUDUCCION_MWH"]$PRUDUCCION_MWH
      
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
ALL_MODELS<-here::here('XGBOOST/HIRLAM_CERROBLANCO//') %>% list.files(full.names = TRUE)
LINEAR_MODELS<- ALL_MODELS %>% .[str_detect(., '_linear')]
XGBbase_MODELS<- ALL_MODELS %>% .[str_detect(., '_xgbBase')]




LISTA_VARIABLES<- list()
for(i in 1:nrow(TABLA_VARIABLES)){
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
    str_split(' ') %>% unlist()
  LISTA_VARIABLES[[paste(VARIABLES_MODELO, collapse = '_')]]<- VARIABLES_MODELO
}




library(forecast)

TABLA_ACCURACY_LINEAR<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_LINEAR)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(LINEAR_MODELS)){
  
  MODELO_LINEAR<- LINEAR_MODELS[i] %>% readRDS()
  
  
  NOMBRE_MODELO<- LINEAR_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_linear.RDS')
  
  VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
  
  
  holdout_x <- DATA_TEST[,VARIABLES_MODELO]
  holdout_y <-  DATA_TEST[,"PRUDUCCION_MWH"]$PRUDUCCION_MWH
  MODELOS_CORRUPTOS<- tryCatch({
    
    TABLA_ACCURACY_LINEAR[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_LINEAR,newdata = holdout_x),
                                                         holdout_y),
                                  cor(predict(MODELO_LINEAR, newdata = holdout_x),
                                      DATA_TEST$PRUDUCCION_MWH,use= 'complete.obs'))
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(LINEAR_MODELS[i])
  })
}

BEST_LINEAR<- TABLA_ACCURACY_LINEAR[order(TABLA_ACCURACY_LINEAR$RMSE, decreasing = FALSE),][2:50,]


TABLA_ACCURACY_XGBbase<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_XGBbase)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(XGBbase_MODELS)){
  
  MODELO_XGBbase<- XGBbase_MODELS[i] %>% readRDS()
  
  
  NOMBRE_MODELO<- XGBbase_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_xgbBase.RDS')
  
  VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
  
  
  holdout_x <- DATA_TEST[,VARIABLES_MODELO]
  holdout_y <-  DATA_TEST[,"PRUDUCCION_MWH"]$PRUDUCCION_MWH
  MODELOS_CORRUPTOS<- tryCatch({
    
    TABLA_ACCURACY_XGBbase[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_XGBbase,newdata = holdout_x),
                                                          holdout_y),
                                   cor(predict(MODELO_XGBbase, newdata = holdout_x),
                                       DATA_TEST$PRUDUCCION_MWH,use= 'complete.obs'))
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(XGBbase_MODELS[i])
  })
}

BEST_XGBBase<- TABLA_ACCURACY_XGBbase[order(TABLA_ACCURACY_XGBbase$RMSE, decreasing = FALSE),][2:50,]



BEST_LINEAR$METHOD<- 'LINEAR'
BEST_XGBBase$METHOD<- 'XGB_BASE'


#JUNTAMOS LOS DATASETS Y COMPROBAMOS RESULTADS MIRANDO MEDIA MAXIMA Y MINIMA DE LAS COLUMAS IMPORTANTES
TABLA_ACCURACY<- list(BEST_LINEAR, BEST_XGBBase) %>% bind_rows()
TABLA_ACCURACY[, c(2:4,7)] <- lapply(TABLA_ACCURACY[, c(2:4,7)], function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})

aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), mean)
aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), max)
aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), min)


X<- 1
REPEATED_VARIABLES<- TABLA_ACCURACY[order(TABLA_ACCURACY$RMSE),][1:X,1] %>% 
  str_split('_') %>% unlist() %>% table()

REPEATED_VARIABLES %>% barplot() 


TABLA_ACCURACY_HIRLAM<- TABLA_ACCURACY
