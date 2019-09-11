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

DATA_ALL<- here::here('NUEVO/Data_calibracion/0B38DAE79059/ERA5_2019-06-01.RDS') %>% readRDS()


DATA_ONE_LOCATION<- DATA_ALL %>% group_by(ERAlon, ERAlat) %>% group_split() %>% .[[1]]

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
# XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------

input_x<- DATA_TRAIN[,c("ERAWS","ERAWD" )]
input_y<- DATA_TRAIN[,"WS_N"]$WS_N

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

nroundsmin<- 500
nroundsmax<- 2500


etamin<- 0.05
etamax<- 0.5


while (TRUE) {
  k<- 0
  VECTOR_ETA<- seq(from = etamin, to = etamax, length.out = 7) %>% round(3)
  VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 7) %>% round()
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


while (TRUE) {
  k<- 0
  VECTOR_ETA<- seq(from = etamin, to = etamax, length.out = 7) %>% round(3)
  VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 7) %>% round()
  tune_grid2 <- expand.grid(
    nrounds = seq(from = 50, to = nrounds, by = 50),
    eta = xgb_tune$bestTune$eta,
    max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                       c(xgb_tune$bestTune$max_depth:4),
                       xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = c(1, 2, 3),
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



# AJUSTAMOS COLUMN AND ROW SAMPLING ---------------------------------------

tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
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




# AJUSTAMOS GAMMA ---------------------------------------------------------

tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
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




# PONEMOS A PRUEBA EL MODELO ----------------------------------------------

holdout_x <- select(DATA_TRAIN_HOLDOUT, -WS_N)
holdout_y <- DATA_TRAIN_HOLDOUT$WS_N


xgb_base_rmse <- ModelMetrics::rmse(holdout_y, predict(xgb_base, newdata = holdout_x))
xgb_model_rmse <- ModelMetrics::rmse(holdout_y, predict(xgb_model, newdata = holdout_x))


ggplot(data = DATA_TRAIN_HOLDOUT[1:500,]) +
    geom_line(aes(x= Date, y= WS_N))+
  geom_line(aes(x= holdout_x$Date[1:500], 
                y= predict(xgb_model, newdata = holdout_x[1:500,])),
            col= 'red', 
            alpha= 0.5)+
  geom_line(data= DATA_TRAIN_HOLDOUT[1:500,], aes(x= Date, y = ERAWS), 
            col='green', alpha= 0.5)+
  theme_light()
  
library(forecast)

accuracy(DATA_TRAIN_HOLDOUT$ERAWS, DATA_TRAIN_HOLDOUT$WS_N)
cor(DATA_TRAIN_HOLDOUT$ERAWS, DATA_TRAIN_HOLDOUT$WS_N)

accuracy(predict(xgb_model, newdata = holdout_x), DATA_TRAIN_HOLDOUT$WS_N)
cor(predict(xgb_model, newdata = holdout_x), DATA_TRAIN_HOLDOUT$WS_N)




# TEST DATA ---------------------------------------------------------------


holdout_x <- select(DATA_TEST, c('ERAWS', 'ERAWD'))
holdout_y <- DATA_TEST$WS_N

accuracy(DATA_TEST$ERAWS, DATA_TEST$WS_N)
cor(DATA_TEST$ERAWS, DATA_TEST$WS_N, use= 'complete.obs')

accuracy(predict(xgb_model, newdata = holdout_x), DATA_TEST$WS_N)
cor(predict(xgb_model, newdata = holdout_x), DATA_TEST$WS_N,use= 'complete.obs')

DATE_RANGE<- DATA_TEST$Date %>% range()
DATE0<- DATE_RANGE[1] + as.difftime(7, units = 'days')

for(i in 1:10){
  if(i==1){
    DATE1<- DATE0
  }
  DATE2<- DATE1 + as.difftime(7, units = 'days')
  DATA_TEST_PLOT<- DATA_TEST %>% filter(Date< DATE2, Date>DATE1)
  
 print( DATA_TEST_PLOT %>% 
          ggplot() +
          geom_line(aes(x= Date, y= WS_N))+
          geom_line(aes(x= Date, 
                        y= predict(xgb_model, 
                                   newdata = DATA_TEST_PLOT %>%  select(c('ERAWS', 'ERAWD')))),
                    col= 'red', 
                    alpha= 0.5)+
          geom_line(aes(x= Date, y = ERAWS), 
                    col='green', alpha= 0.5)+
          theme_light())
  
  DATE1<- DATE1 + as.difftime(7, units = 'days')
  
}

