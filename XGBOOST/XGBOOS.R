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

DATA_ALL<- here::here('Data/Parques/PRUEBA_EOLICOS/TATANKA_DATA/NAM_12_TATANKA_WITH_PRODUCTION.csv') %>% read_csv()


MAX_COR_POINT<- DATA_ALL %>% group_by(LON.x, LAT.x) %>% group_split() %>% 
  sapply(function(x){
    cor(x$WS10,x$PRUDUCCION_MWH, use = 'complete.obs')
  }) %>% which.max()

DATA_ONE_LOCATION<- DATA_ALL %>%  group_by(LON.x, LAT.x) %>% group_split() %>% .[[MAX_COR_POINT]]


TEST_TRAIN_FACTOR<- 1.5
N_DATOS<-DATA_ONE_LOCATION %>% nrow()
DATA_TRAIN<- DATA_ONE_LOCATION[1:((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)),]
DATA_TEST<- DATA_ONE_LOCATION[((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)):N_DATOS,]

'
NO VAMOS A DIVIDIR EL DATASET DE ENTRENAMIENTO, MIRAREMOS DIRECTAMENTE NUESTRA 
PRECISION SOBRE EL DATASET DE TEST

DATA_TRAIN_HOLDOUT <- dplyr::sample_frac(DATA_TRAIN, 0.2)
hid <- as.numeric(rownames(DATA_TRAIN_HOLDOUT))
DATA_TRAIN <- DATA_TRAIN[-hid, ]
'

# CREAMOS MODELO LINEAL DE REFERENCIA -------------------------------------


linear_base <- lm(paste0("PRUDUCCION_MWH ~ ", paste(c('WS10','WD10','WS80', 'WD80'), collapse = ' + ')),data = DATA_TRAIN)

# XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------


input_x<- DATA_TRAIN[,c('WS80', 'WD80')]
input_y<- DATA_TRAIN[,"PRUDUCCION_MWH"]$PRUDUCCION_MWH

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
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

xgb_base <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE
)


# AJUSTAMOS EL NUMERO DE ARBOLES Y ETAÇ -----------------------------------

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
  
  tuneplot(xgb_tune)
  
  
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

'
PARECE QUE QUEDA COMO 1400 nrounds y 0.3 eta, ahora ajustamos 
depth
'

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
  
  tuneplot(xgb_tune2)
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
  VECTOR_COLSAMPLE<- seq(from = colsamplemin, to = colsamplemax, length.out = 10) %>% round(3)
  VECTOR_SUBSAMPLE<- seq(from = subsamplemin, to= subsamplemax , length.out = 10) %>% round(3)
  VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 10) %>% round()
  
  
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
  
  tuneplot(xgb_tune3, probs = .95)
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
  
  tuneplot(xgb_tune4)

    if(xgb_tune3$bestTune$gamma==gammamin){
    subsamplemin= subsamplemin - subsamplemin/1.5
    subsamplemax= subsamplemax - subsamplemax/1.5
    k<- 1
  }
  if(xgb_tune3$bestTune$gamma==gammamax){
    subsamplemin= subsamplemin + subsamplemin/1.5
    subsamplemax= subsamplemax + subsamplemax/1.5
    k<- 1
  }
  
  if(k==0){
    print(xgb_tune3$bestTune)
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

tuneplot(xgb_tune5)





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




# TEST DATA ---------------------------------------------------------------

library(forecast)

holdout_x <- DATA_TEST[,c('WS10','WD10','WS80', 'WD80')]
holdout_y <-  DATA_TEST[,"PRUDUCCION_MWH"]$PRUDUCCION_MWH

accuracy(predict(linear_base, newdata = holdout_x), DATA_TEST$PRUDUCCION_MWH)
cor(predict(linear_base, newdata = holdout_x), DATA_TEST$PRUDUCCION_MWH,use= 'complete.obs')

accuracy(predict(xgb_base, newdata = holdout_x), DATA_TEST$PRUDUCCION_MWH)
cor(predict(xgb_base, newdata = holdout_x), DATA_TEST$PRUDUCCION_MWH,use= 'complete.obs')

accuracy(predict(xgb_model, newdata = holdout_x), DATA_TEST$PRUDUCCION_MWH)
cor(predict(xgb_model, newdata = holdout_x), DATA_TEST$PRUDUCCION_MWH,use= 'complete.obs')

DATE_RANGE<- DATA_TEST$DATE %>% range()
DATE0<- DATE_RANGE[1] + as.difftime(7, units = 'days')

for(i in 1:5){
  if(i==1){
    DATE1<- DATE0
  }
  DATE2<- DATE1 + as.difftime(7, units = 'days')
  DATA_TEST_PLOT<- DATA_TEST %>% filter(DATE < DATE2, DATE>DATE1)
  
 print( DATA_TEST_PLOT %>% 
          ggplot() +
          geom_line(aes(x= DATE, y= PRUDUCCION_MWH))+
          geom_line(aes(x= DATE, 
                        y= predict(xgb_model, 
                                   newdata =  DATA_TEST_PLOT[,c('WS10','WD10','WS80', 'WD80')])),
                    colour= 'red', 
                    alpha= 0.8)+
          geom_line(aes(x= DATE, 
                        y= predict(linear_base, 
                                   newdata =  DATA_TEST_PLOT[,c('WS10','WD10','WS80', 'WD80')])),
                        colour= 'green', 
                        alpha= 0.8)+
          geom_line(aes(x= DATE, 
                        y= predict(xgb_base, 
                                   newdata =  DATA_TEST_PLOT[,c('WS10','WD10','WS80', 'WD80')])),
                        colour= 'orange', 
                        alpha= 0.8)+
          theme_light()+
          guides(colour=FALSE, alpha= FALSE))
  
  DATE1<- DATE1 + as.difftime(7, units = 'days')
  
}




linear_10_80<- linear_base
xgb_Base_10_80<- xgb_base
xgb_model_10_80<- xgb_model