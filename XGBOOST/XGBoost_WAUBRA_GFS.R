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
library(stringr)


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
  
  colnames(TABLA_ALL)<- colnames(TABLA_ALL) %>% str_remove_all('componentofwindlevel|windcomponentlevel')
  
  TABLA_ALL$NANA<- NULL
  
  
  TABLA_ALL<- TABLA_ALL %>% mutate(WS10= sqrt(U10^2 + V10^2),
                                   WD10= atan2(U10/WS10, V10/WS10)* 180/pi +180,
                                   WS80= sqrt(U80^2 + V80^2),
                                   WD80= atan2(U80/WS80, V80/WS80)* 180/pi +180,
                                   WS100000= sqrt(U100000^2 + V100000^2),
                                   WD100000= atan2(U100000/WS100000, V100000/WS100000)* 180/pi +180,
                                   WS97500= sqrt(U97500^2 + V97500^2),
                                   WD97500= atan2(U97500/WS97500, V97500/WS97500)* 180/pi +180, 
                                   WS95000= sqrt(U95000^2 + V95000^2),
                                   WD95000= atan2(U95000/WS95000, V95000/WS95000)* 180/pi +180, 
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
  INFO_WAUBRA<- INFO_PARQUES %>% filter(PARQUE=='P.E. Waubra')
  
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


#COJEMOS EL PUNTO CON MEJOR CORRELACION Y EL MODELO MAS FRESCO PAR QUE NO SE PISE
MAX_COR_POINT<- DATA_ALL %>% group_by(LON, LAT) %>% group_split() %>% 
  sapply(function(x){
    print(cor(x$WS10,x$P_MWH, use = 'complete.obs'))
    cor(x$WS10,x$P_MWH, use = 'complete.obs')
  }) %>% which.max()

DATA_ONE_LOCATION<- DATA_ALL %>%  group_by(LON, LAT) %>% group_split() %>% .[[MAX_COR_POINT]]
DATA_ONE_LOCATION<-  DATA_ONE_LOCATION[DATA_ONE_LOCATION$FCST_TIME<24,] %>% .[order(.$DATE),]


# IMPORTAMOS LA CURVA DE POTENCIA -----------------------------------------

CURVA_POTECIA1<- here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/CURVA_WAUBRA51.RDS') %>% readRDS()
CURVA_POTECIA2<- here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/CURVA_WAUBRA77.RDS') %>% readRDS()

#CREAMOS LA CURVA DE POTENCIA TENIENDO EN CUENTA DE QUE EN WAUBRA TENEMOS 2 TIPOS DE MOLINOS

LISTA_POTENCIAS<- list()
for(WS in (DATA_ONE_LOCATION %>% colnames() %>% .[str_detect(., 'WS')])){
  
  #CURVA PARA 97500
  P_ANALITIC1= DATA_ONE_LOCATION[, WS] %>% unlist() %>%
    cut(breaks= (CURVA_POTECIA1$WS_ms %>% as.character() %>% as.numeric()), 
        labels = CURVA_POTECIA1$P_kwR[1:(nrow(CURVA_POTECIA1)-1)]) %>%
    as.character() %>% 
    as.numeric()
  
  P_ANALITIC2= DATA_ONE_LOCATION[, WS] %>% unlist() %>%
    cut(breaks= (CURVA_POTECIA2$WS_ms %>% as.character() %>% as.numeric()), 
        labels = CURVA_POTECIA2$P_kwR[1:(nrow(CURVA_POTECIA2)-1)]) %>%
    as.character() %>% 
    as.numeric()
  
  P_ANALITIC<- (P_ANALITIC1*51 + P_ANALITIC2*77)/1000 
  LISTA_POTENCIAS[[WS]]<- P_ANALITIC
}




# CONSTRUCCION DE VARIABLES INICIALES PARA LA PRIMERA TANDA DE CLA --------

DATA_ONE_LOCATION<- DATA_ONE_LOCATION %>% mutate(P100000mAN= LISTA_POTENCIAS$WS100000, 
                                                 P97500mAN= LISTA_POTENCIAS$WS97500, 
                                                 P95000mAN= LISTA_POTENCIAS$WS95000, 
                                                 P80AN= LISTA_POTENCIAS$WS80, 
                                                 P10AN= LISTA_POTENCIAS$WS10, 
                                                 HORA= DATE %>% hour(),
                                                 COSH= cos(2*pi*HORA/24))

'
                                                WS102= WS10^2,
                                                 WS103= WS10^3,
                                                 WS802= WS80^2,
                                                 WS803= WS80^3,
                                                 WS1002= WS100000^2,
                                                 WS1003=WS100000^3,
                                                 WS9752= WS97500^2,
                                                 WS9753=WS97500^3,
                                                 WS9502=WS95000^2,
                                                 WS9503=WS95000^3,
'

#COMPROBAMOS LA EFICACIA DEL METODO ANALITICO
library(forecast)
accuracy(DATA_ONE_LOCATION$P_MWH, DATA_ONE_LOCATION$P950mAN)
MAPE(DATA_ONE_LOCATION$P_MWH, DATA_ONE_LOCATION$P950mAN, na.rm = TRUE)
cor(DATA_ONE_LOCATION$P_MWH, DATA_ONE_LOCATION$P950mAN, use = 'complete.obs')


ggplot(data = DATA_ONE_LOCATION %>% filter(month(DATE)==4))+
  geom_line(aes(x=DATE, y= P950mAN))+
  geom_line(aes(x=DATE, y= P_MWH), colour='green')+
  theme_light()

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

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = (((nrow(DATA_TRAIN)/100)*98) %>% round(0)),
                              horizon = 24,
                              fixedWindow = TRUE,
                              allowParallel = TRUE)


# CREAMOS LAS COMBINACIONES POSIBLES DE VARIABLES -------------------------
VECTOR_COLS<-DATA_ONE_LOCATION %>% colnames() %>% .[!(.%in%c("LON","LAT","DATE","FCST_TIME", 'P_MWH','DISP'))]
VECTOR_COLS1<- VECTOR_COLS %>% .[str_detect(., 'WS|WD|P|U|V')]

pat <- "(\\d)+"
VECTOR_CAPAS<- VECTOR_COLS1 %>% str_extract(., pat) %>% unique() %>% na.omit()

for(ALTURAS_VIENTO in VECTOR_CAPAS){
  VECTOR_COLS_SELECT<- VECTOR_COLS1 %>% .[which(str_extract(.,pat)==ALTURAS_VIENTO)]
  
  TABLA_VARIABLES<- expand.grid(lapply(VECTOR_COLS_SELECT, function(x) c(x, NA)))
  for(i in 1:nrow(TABLA_VARIABLES)){
    
    VARIABLES_MODELO<- TABLA_VARIABLES[i,] %>% unlist() %>% .[!is.na(.)] %>% as.vector() %>% 
      str_split(' ') %>% unlist()
    
    tryCatch({
      
      PATH_MODELOS<- here::here('XGBOOST/GFS_WAUBRA/ALTURAS_VIENTO/')
      if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS, recursive = TRUE)}
      
      NOMBRE_BASE<- paste(VARIABLES_MODELO, collapse = '_')
      
      if(file.exists(paste0(PATH_MODELOS, NOMBRE_BASE,'_linear.RDS'))){
        print(paste('YA EXISTE', NOMBRE_BASE))
      }else{
        

        # CREAMOS INPUT X e INPUT Y -----------------------------------------------
        input_x<- DATA_TRAIN[,VARIABLES_MODELO]
        input_y<- DATA_TRAIN[,"P_MWH"]$P_MWH
        
        # CREAMOS MODELO LINEAL DE REFERENCIA -------------------------------------
        
        linear_base <- train(x = input_x,
                        y = input_y,
                        method = "lm",
                        trControl = myTimeControl,
                        tuneLength=tuneLength.num)
        # XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------
      
        
        xgb_base <- caret::train(
          x = input_x,
          y = input_y,
          trControl = myTimeControl,
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
}



# CHECK CROSS VALIDATED RESULTS -------------------------------------------
NAME_MODELS<- here::here('XGBOOST/GFS_WAUBRA/ALTURAS_VIENTO/') %>% list.files(full.names = TRUE) %>% 
  str_split('/') %>% sapply(function(x){x[length(x)] %>% str_remove('.RDS')})
ALL_MODELS<-here::here('XGBOOST/GFS_WAUBRA/ALTURAS_VIENTO/') %>% list.files(full.names = TRUE) %>% 
  lapply(readRDS)
names(ALL_MODELS)<- NAME_MODELS


resamps <- resamples(ALL_MODELS)

ss <- summary(resamps)


# MIRAMOS LOS MODELOS EN FUNCION DEL RMSEÇ --------------------------------

RMSE_SUMMARIES<- (ss[[3]]$RMSE)
RMSE_XGBASE<- RMSE_SUMMARIES[(RMSE_SUMMARIES %>% rownames() %>% str_detect(.,'xgbBase')), ]
RMSE_linear<- RMSE_SUMMARIES[(RMSE_SUMMARIES %>% rownames() %>% str_detect(.,'linear')), ]

BEST_XGBASE_names<- RMSE_XGBASE[order(RMSE_XGBASE[,"Mean"]), ][1:20, ] %>% rownames() 
BEST_XGBASE<- ALL_MODELS[BEST_XGBASE_names]
RS_XGBASE <- resamples(BEST_XGBASE)

dotplot(RS_XGBASE, metric = "RMSE")
dotplot(RS_XGBASE, metric = "MAE")
dotplot(RS_XGBASE, metric = "Rsquared")

BEST_linear_names<- RMSE_linear[order(RMSE_linear[,"Mean"]), ][1:20, ] %>% rownames() 
BEST_linear<- ALL_MODELS[BEST_linear_names]
RS_linear <- resamples(BEST_linear)

dotplot(RS_linear, metric = "RMSE")
dotplot(RS_linear, metric = "MAE")
dotplot(RS_linear, metric = "Rsquared")



(BEST_XGBASE_names %>% str_remove('_xgbBase'))%>% 
  str_split('_') %>% unlist() %>% table() %>% barplot(las=2) 

(BEST_linear_names %>% str_remove('_linear'))%>% 
  str_split('_') %>% unlist() %>% table() %>% barplot(las=2)

RMSE_XGBASE[order(RMSE_XGBASE[,"Mean"]), ][1:2, ] %>% rownames() 

RMSE_linear[order(RMSE_linear[,"Mean"]), ][1:2, ] %>% rownames() 

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
TABLA_ACCURACY<- TABLA_ACCURACY %>% .[complete.cases(.), ]
TABLA_ACCURACY[, c(2:7)] <- lapply(TABLA_ACCURACY[, c(2:7)], function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})

aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), mean)
aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), max)
aggregate(TABLA_ACCURACY[, c(2:7)], list(TABLA_ACCURACY$METHOD), min)

TABLA_ACCURACY1<- TABLA_ACCURACY

# MIRAMOS CUALES SON LAS VARIABLES MAS REPETIDAS --------------------------
X<- 1
REPEATED_VARIABLES<- TABLA_ACCURACY1[order(TABLA_ACCURACY1$MAPE),][1:X,1] %>% 
  str_split('_') %>% unlist() %>% table()

REPEATED_VARIABLES %>% barplot() 
BEST_VARS1<- REPEATED_VARIABLES %>% names()







# SEGUNDA TANDA DE VARIABLES A PROBAR -------------------------------------
#COJEMOS LOS MEJORES PREDICTORES Y PASAMOS LA SEGUNDA RONDA DE AFINACION

VECTOR_COLS2<- VECTOR_COLS[!(VECTOR_COLS %in% VECTOR_COLS1)] %>% .[!(.%in%c('T_MODELO','P_MWH','DISP',
                                                                            'HORA','COSH','PLAG1',
                                                                            'PLAG2','PLAG3','PLAG4'))]


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
REPEATED_VARIABLES<- TABLA_ACCURACY3[order(TABLA_ACCURACY3$RMSE),][1:X,1] %>% 
  str_split('_') %>% unlist() %>% table()

REPEATED_VARIABLES %>% barplot() 



