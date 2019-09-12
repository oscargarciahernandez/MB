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

DATA_ALL<- here::here('Data/Parques/PRUEBA_EOLICOS/CERROBLANCO/METEOGALICIA/METEOGALICIA_2019-09-11.csv') %>% read_csv()
INFO_PARQUES<- here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS') %>% readRDS()
INFO_CERROBLANCO<- INFO_PARQUES[INFO_PARQUES$PARQUE %>% str_detect('Cerroblanco'), ]

#UNIMOS LOS DATOS DEL PARQUE
DATA_ALL<- left_join(DATA_ALL, INFO_CERROBLANCO, by= 'DATE')


#SELECCIONAMOS EL PUNTO CON MEJOR CORRELACION 
MAX_COR_POINT<- DATA_ALL %>% group_by(LON.x, LAT.x) %>% group_split() %>% 
  sapply(function(x){
    cor(x$WS10,x$PRUDUCCION_MWH, use = 'complete.obs')
  }) %>% which.max()

DATA_ONE_LOCATION<- DATA_ALL %>%  group_by(LON.x, LAT.x) %>% group_split() %>% .[[MAX_COR_POINT]]


#CORAMOS LOS DATOS COJIENDO SOLO HORIZONTE 24 Y ORDENAMOS 
DATA_ONE_LOCATION<-  DATA_ONE_LOCATION[DATA_ONE_LOCATION$TSIM<24,]
DATA_ONE_LOCATION<- DATA_ONE_LOCATION[!duplicated(DATA_ONE_LOCATION), ]
DATA_ONE_LOCATION<- DATA_ONE_LOCATION[order(DATA_ONE_LOCATION$DATE),]
DATA_ONE_LOCATION<- DATA_ONE_LOCATION[complete.cases(DATA_ONE_LOCATION),]



# CREATIVE FEATURE ENGIENEERING -------------------------------------------
# DE MOMENTO POCA CREATIVIDAD PERO AQUI ES DONDE CREAREMOS LAS VARIABLES 
# PARA MEJORAR EL FUNCIONAMIENTO DE NUESTRO MODELO

DATA_ONE_LOCATION<- DATA_ONE_LOCATION %>% mutate(WS10_2= WS10^2,
                                                 WS10_3= WS10^3,
                                                 WSLEV1_2= WSLEV1^2,
                                                 WSLEV1_3= WSLEV1^3)



# SPLIT TRAIN AND TEST ----------------------------------------------------
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


# CREAMOS MILES DE MODELOS PARA TODAS LA VARIABLES DISPONIBLES ------------
GV1<- c( paste('WSLEV1', 'WDLEV1', collapse = '_'), NA)
GV2<- c( paste('ULEV1', 'VLEV1', collapse = '_'), NA)

GV3<-  c( paste('WS10', 'WD10', collapse = '_'), NA)
GV4<- c( paste('U10', 'V10', collapse = '_'), NA)

GV5<- c('TSIM', NA)

GV6<- c('WS10_2', NA)

GV7<- c('WS10_3', NA)

GV8<- c('WSLEV1_2', NA)
GV9<- c('WSLEV1_3', NA)

TABLA_VARIABLES<- expand.grid(GV1, GV2, GV3,GV4, GV5, GV6, GV7, GV8, GV9)
library(stringr)

for(i in 1:nrow(TABLA_VARIABLES)){
  
  
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% as.character() %>% paste(collapse = ' ') %>%
    str_remove_all(pattern = 'NA') %>% .[.!=''] %>% str_replace_all('  ', ' ') %>% 
    str_trim() %>% str_split(' ') %>% .[[1]] %>% .[. != ""]
  
  tryCatch({
    
    PATH_MODELOS<- here::here('XGBOOST/METEOGALICIA_CERROBLANCO/')
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
ALL_MODELS<-here::here('XGBOOST/METEOGALICIA_CERROBLANCO//') %>% list.files(full.names = TRUE)
LINEAR_MODELS<- ALL_MODELS %>% .[str_detect(., '_linear')]
XGBbase_MODELS<- ALL_MODELS %>% .[str_detect(., '_xgbBase')]
XGBTune_MODELS<- ALL_MODELS %>% .[str_detect(., '_xbbTune')]



LISTA_VARIABLES<- list()
for(i in 1:nrow(TABLA_VARIABLES)){
  VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% as.character() %>% paste(collapse = ' ') %>%
    str_remove_all(pattern = 'NA') %>% .[.!=''] %>% str_replace_all('  ', ' ') %>% 
    str_trim() %>% str_split(' ') %>% .[[1]] %>% .[. != ""]
  
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

BEST_LINEAR<- TABLA_ACCURACY_LINEAR[order(TABLA_ACCURACY_LINEAR$CORR, decreasing = TRUE),][1:50,]


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

BEST_XGBBase<- TABLA_ACCURACY_XGBbase[order(TABLA_ACCURACY_XGBbase$CORR, decreasing = TRUE),][1:50,]



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



# AJUSTAMOS EL XGBOOST PARA LOS 5 MEJORES RESULTADOS ----------------------

BEST_MODELS<- TABLA_ACCURACY %>% filter(METHOD== "XGB_BASE") %>% 
  .[order(.$CORR, decreasing = TRUE), ]%>% .[1:5,'NAME']

VARIABLES_AJUSTE<- LISTA_VARIABLES[which(names(LISTA_VARIABLES)%in%BEST_MODELS)]

for(i in 1:length(VARIABLES_AJUSTE)){
  
  
  VARIABLES_MODELO<- VARIABLES_AJUSTE[[i]]
  
  tryCatch({
    
    PATH_MODELOS<- here::here('XGBOOST/METEOGALICIA_CERROBLANCO/')
    if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS, recursive = TRUE)}
    
    NOMBRE_BASE<- paste(VARIABLES_MODELO, collapse = '_')
    
    if(file.exists(paste0(PATH_MODELOS, NOMBRE_BASE,'xgbTune.RDS'))){
      print(paste('YA EXISTE', NOMBRE_BASE))
    }else{
      input_x<- DATA_TRAIN[,VARIABLES_MODELO]
      input_y<- DATA_TRAIN[,"PRUDUCCION_MWH"]$PRUDUCCION_MWH

  nroundsmin<- 50
  nroundsmax<- 1000
  
  
  etamin<- 0.05
  etamax<- 0.5
  
  
  while (TRUE) {
    k<- 0
    VECTOR_ETA<- seq(from = etamin, to = etamax, length.out = 10) %>% round(3)
    VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 10) %>% round()
    tune_grid <- expand.grid(
      nrounds = VECTOR_NROUNDS,
      eta = VECTOR_ETA,
      max_depth = c(2, 3, 4, 5, 6),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    
    tune_control <- caret::trainControl(
      method = "cv", # cross-validation
      number = 3, # with n folds 
      #index = createFolds(tr_treated$Id_clean), # fix the folds
      verboseIter = FALSE, # no training log
      allowParallel = TRUE # FALSE for reproducible results 
    )
    
    xgb_tune <- caret::train(
      x = input_x,
      y = input_y,
      trControl = tune_control,
      tuneGrid = tune_grid,
      method = "xgbTree",
      verbose = TRUE
    )
    
    # helper function for the plots
    tuneplot <- function(x, probs = .90) {
      ggplot(x) +
        coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
        theme_bw()
    }
    
    #tuneplot(xgb_tune)
    
    
    if(xgb_tune$bestTune$nrounds==nroundsmin){
      nroundsmin= nroundsmin - nroundsmin/1.5
      nroundsmax= nroundsmax - nroundsmax/1.5
      k<- 1
    }
    if(xgb_tune$bestTune$nrounds==nroundsmax){
      nroundsmin= nroundsmin + nroundsmin/1.5
      nroundsmax= nroundsmax + nroundsmax/1.5
      k<- 1
    }
    if(xgb_tune$bestTune$eta==etamin){
      etamin= etamin - etamin/1.5
      etamax= etamax - etamax/1.5
      k<- 1
    }
    if(xgb_tune$bestTune$eta==etamax){
      etamin= etamin + etamin/1.5
      etamax= etamax + etamax/1.5
      k<- 1
    }
    
    if(k==0){
      print(xgb_tune$bestTune)
      break
    }
    
    
  }
  
  
  # AJUSTAMOS DEPTH y CHILD WEIGH ---------------------------------------------------------
  
  nroundsmin<- 50
  nroundsmax<- 1000
  
  mchildweightmin<- 1
  mchildweightmax<- 10
  
  
  while (TRUE) {
    k<- 0
    VECTOR_MCW<- seq(from = mchildweightmin, to = mchildweightmax, length.out = 10) %>% round()
    VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 10) %>% round()
    
    tune_grid2 <- expand.grid(
      nrounds = VECTOR_NROUNDS,
      eta = xgb_tune$bestTune$eta,
      max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                         c(xgb_tune$bestTune$max_depth:4),
                         xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = VECTOR_MCW,
      subsample = 1
    )
    
    xgb_tune2 <- caret::train(
      x = input_x,
      y = input_y,
      trControl = tune_control,
      tuneGrid = tune_grid2,
      method = "xgbTree",
      verbose = TRUE
    )
    
    #tuneplot(xgb_tune2)
    xgb_tune2$bestTune
    
    
    if(xgb_tune2$bestTune$nrounds==nroundsmin){
      nroundsmin= nroundsmin - nroundsmin/1.5
      nroundsmax= nroundsmax - nroundsmax/1.5
      k<- 1
    }
    if(xgb_tune2$bestTune$nrounds==nroundsmax){
      nroundsmin= nroundsmin + nroundsmin/1.5
      nroundsmax= nroundsmax + nroundsmax/1.5
      k<- 1
    }
    if(xgb_tune2$bestTune$min_child_weight==mchildweightmin){
      mchildweightmin= mchildweightmin - mchildweightmin/1.5
      mchildweightmax= mchildweightmax - mchildweightmax/1.5
      k<- 1
    }
    if(xgb_tune2$bestTune$min_child_weight==mchildweightmax){
      mchildweightmin= mchildweightmin + mchildweightmin/1.5
      mchildweightmax= mchildweightmax + mchildweightmax/1.5
      k<- 1
    }
    
    if(k==0){
      print(xgb_tune2$bestTune)
      break
    }
    
    
  }
  
  
  
  # AJUSTAMOS COLUMN AND ROW SAMPLING ---------------------------------------
  
  nroundsmin<- 50
  nroundsmax<- 1000
  
  colsamplemin<- 0.1
  colsamplemax<- 1
  
  subsamplemin<- 0.1
  subsamplemax<- 1
  
  
  while (TRUE) {
    k<- 0
    VECTOR_COLSAMPLE<- seq(from = colsamplemin, to = colsamplemax, length.out = 7) %>% round(3)
    VECTOR_SUBSAMPLE<- seq(from = subsamplemin, to= subsamplemax , length.out = 7) %>% round(3)
    VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 5) %>% round()
    
    
    tune_grid3 <- expand.grid(
      nrounds = VECTOR_NROUNDS,
      eta = xgb_tune$bestTune$eta,
      max_depth = xgb_tune2$bestTune$max_depth,
      gamma = 0,
      colsample_bytree = VECTOR_COLSAMPLE,
      min_child_weight = xgb_tune2$bestTune$min_child_weight,
      subsample = VECTOR_SUBSAMPLE
    )
    
    xgb_tune3 <- caret::train(
      x = input_x,
      y = input_y,
      trControl = tune_control,
      tuneGrid = tune_grid3,
      method = "xgbTree",
      verbose = TRUE
    )
    
    ##tuneplot(xgb_tune3, probs = .95)
    xgb_tune3$bestTune
    
    if(xgb_tune3$bestTune$colsample_bytree ==colsamplemin){
      colsamplemin= colsamplemin - colsamplemin/1.5
      colsamplemax= colsamplemax - colsamplemax/1.5
      k<- 1
    }
    if(xgb_tune3$bestTune$colsample_bytree==colsamplemax){
      colsamplemin= colsamplemin + colsamplemin/1.5
      colsamplemax= colsamplemax + colsamplemax/1.5
      k<- 1
    }
    if(xgb_tune3$bestTune$subsample==subsamplemin){
      subsamplemin= subsamplemin - subsamplemin/1.5
      subsamplemax= subsamplemax - subsamplemax/1.5
      k<- 1
    }
    if(xgb_tune3$bestTune$subsample==subsamplemax){
      subsamplemin= subsamplemin + subsamplemin/1.5
      subsamplemax= subsamplemax + subsamplemax/1.5
      k<- 1
    }
    
    if(k==0){
      print(xgb_tune3$bestTune)
      break
    }else{
      print('CAMBIANDO PARAMETROS')
    }
    
    
  }
  
  
  
  
  
  # AJUSTAMOS GAMMA ---------------------------------------------------------
  nroundsmin<- 50
  nroundsmax<- 1000
  
  gammamin<- 0.001
  gammamax<- 1
  
  while (TRUE) {
    k<- 0
    VECTOR_GAMMA<-  seq(from = gammamin, to = gammamax, length.out = 10) %>% round(3)
    VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 10) %>% round()
    
    
    tune_grid4 <- expand.grid(
      nrounds = VECTOR_NROUNDS,
      eta = xgb_tune$bestTune$eta,
      max_depth = xgb_tune2$bestTune$max_depth,
      gamma = VECTOR_GAMMA,
      colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
      min_child_weight = xgb_tune2$bestTune$min_child_weight,
      subsample = xgb_tune3$bestTune$subsample
    )
    
    xgb_tune4 <- caret::train(
      x = input_x,
      y = input_y,
      trControl = tune_control,
      tuneGrid = tune_grid4,
      method = "xgbTree",
      verbose = TRUE
    )
    
    #tuneplot(xgb_tune4)
    
    if(xgb_tune4$bestTune$gamma==gammamin){
      subsamplemin= subsamplemin - subsamplemin/1.5
      subsamplemax= subsamplemax - subsamplemax/1.5
      k<- 1
    }
    if(xgb_tune4$bestTune$gamma==gammamax){
      subsamplemin= subsamplemin + subsamplemin/1.5
      subsamplemax= subsamplemax + subsamplemax/1.5
      k<- 1
    }
    
    if(k==0){
      print(xgb_tune4$bestTune)
      break
    }
    
    
  }
  

  
  
  
  
  
  # REDUCIMOS EL LEARNING RATE ----------------------------------------------
  
  tune_grid5 <- expand.grid(
    nrounds = seq(from = 100, to = 10000, by = 100),
    eta = seq(0.01, xgb_tune$bestTune$eta, length.out = 5),
    max_depth = xgb_tune2$bestTune$max_depth,
    gamma = xgb_tune4$bestTune$gamma,
    colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
    min_child_weight = xgb_tune2$bestTune$min_child_weight,
    subsample = xgb_tune3$bestTune$subsample
  )
  
  xgb_tune5 <- caret::train(
    x = input_x,
    y = input_y,
    trControl = tune_control,
    tuneGrid = tune_grid5,
    method = "xgbTree",
    verbose = TRUE
  )
  
  #tuneplot(xgb_tune5)
  
  
  
  
  
  # fitting the model -------------------------------------------------------
  
  final_grid <- expand.grid(
    nrounds = xgb_tune5$bestTune$nrounds,
    eta = xgb_tune5$bestTune$eta,
    max_depth = xgb_tune5$bestTune$max_depth,
    gamma = xgb_tune5$bestTune$gamma,
    colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
    min_child_weight = xgb_tune5$bestTune$min_child_weight,
    subsample = xgb_tune5$bestTune$subsample
  )
  
  xgb_model <- caret::train(
    x = input_x,
    y = input_y,
    trControl = train_control,
    tuneGrid = final_grid,
    method = "xgbTree",
    verbose = TRUE
  )
  
  saveRDS(xgb_model, paste0(PATH_MODELOS, NOMBRE_BASE,'_xgbTune.RDS'))
}

  }, error= function(e){
  print('FALLO AJUSTANTO')
})
}




# SACAMOS TABLA ACCURACY PARA LOS MODELOS TUNEADOS
ALL_MODELS<-here::here('XGBOOST/METEOGALICIA_CERROBLANCO//') %>% list.files(full.names = TRUE)
XGBTune_MODELS<- ALL_MODELS %>% .[str_detect(., '_xgbTune')]

TABLA_ACCURACY_XGBTune<- data.frame(matrix(ncol = 7))
colnames(TABLA_ACCURACY_XGBTune)<- c('NAME','ME','RMSE','MAE','MPE','MAPE','CORR')
for(i in 1:length(XGBTune_MODELS)){
  
  MODELO_XGBTune<- XGBTune_MODELS[i] %>% readRDS()
  
  
  NOMBRE_MODELO<- XGBTune_MODELS[i] %>% str_split('/') %>% .[[1]] %>%
    .[length(.)] %>% str_remove('_xgbTune.RDS')
  
  VARIABLES_MODELO<- LISTA_VARIABLES[[which(names(LISTA_VARIABLES)==NOMBRE_MODELO)]]
  
  
  holdout_x <- DATA_TEST[,VARIABLES_MODELO]
  holdout_y <-  DATA_TEST[,"PRUDUCCION_MWH"]$PRUDUCCION_MWH
  MODELOS_CORRUPTOS<- tryCatch({
    
    TABLA_ACCURACY_XGBTune[i,]<- c(NOMBRE_MODELO,accuracy(predict(MODELO_XGBTune,newdata = holdout_x),
                                                          DATA_TEST$PRUDUCCION_MWH),
                                   cor(predict(MODELO_XGBTune, newdata = holdout_x),
                                       DATA_TEST$PRUDUCCION_MWH,use= 'complete.obs'))
  }, error= function(e){
    print('MODELO CORRUPTO')
    return(XGBTune_MODELS[i])
  })
}

TABLA_ACCURACY_XGBTune$METHOD<- 'XGB_Tune'




#JUNTAMOS LOS MODELOS TUNEADOS Y LOS MODELOS DE PRUEBA
TABLA_ACCURACY<- list(TABLA_ACCURACY, TABLA_ACCURACY_XGBTune) %>% bind_rows()
TABLA_ACCURACY[, c(2:4,7)] <- lapply(TABLA_ACCURACY[, c(2:4,7)], function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})

aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), mean)
aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), max)
aggregate(TABLA_ACCURACY[, c(2:4,7)], list(TABLA_ACCURACY$METHOD), min)




