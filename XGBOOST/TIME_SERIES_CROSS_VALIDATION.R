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
# IMPORTAR DATA -----------------------------------------------------------

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

DATA_ONE_LOCATION<- DATA_ONE_LOCATION %>% mutate(WS102= WS10^2,
                                                 WS103= WS10^3,
                                                 WS802= WS80^2,
                                                 WS803= WS80^3,
                                                 WS1002= WS100000^2,
                                                 WS1003=WS100000^3,
                                                 WS9752= WS97500^2,
                                                 WS9753=WS97500^3,
                                                 WS9502=WS95000^2,
                                                 WS9503=WS95000^3,
                                                 P100mAN= LISTA_POTENCIAS$WS100000, 
                                                 P975mAN= LISTA_POTENCIAS$WS97500, 
                                                 P950mAN= LISTA_POTENCIAS$WS95000, 
                                                 P80AN= LISTA_POTENCIAS$WS80, 
                                                 P10AN= LISTA_POTENCIAS$WS10, 
                                                 HORA= DATE %>% hour(),
                                                 COSH= cos(2*pi*HORA/24))

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

# TIME SERIES CROOSS VALIDATION -------------------------------------------
#AQUI UN TIO QUE EXPLICA MUY BIEN LO QUE HACE LO DE TIMESLICES
#https://stackoverflow.com/questions/24758218/time-series-data-splitting-and-model-evaluation

# DE AQUI SACO LA MANERA DE HACER LOS TIMESLICES Y LOS MODELOS QUE VOY A PRBAR A CONTINUACION
#https://rpubs.com/crossxwill/time-series-cv


# TIME SERIES CROOSS VALIDATION -------------------------------------------
#AQUI UN TIO QUE EXPLICA MUY BIEN LO QUE HACE LO DE TIMESLICES
#https://stackoverflow.com/questions/24758218/time-series-data-splitting-and-model-evaluation

# DE AQUI SACO LA MANERA DE HACER LOS TIMESLICES Y LOS MODELOS QUE VOY A PRBAR A CONTINUACION
#https://rpubs.com/crossxwill/time-series-cv

library(doParallel)
registerDoParallel(cores=10)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = FALSE,
                              allowParallel = TRUE)

VECTOR_TIMESLICES<-createTimeSlices(1:nrow(DATA_TRAIN), 
                 initialWindow = (((nrow(DATA_TRAIN)/100)*95) %>% round(0)),
                 horizon = 24,
                 fixedWindow = FALSE)

VECTOR_TIMESLICES$train %>% length
VECTOR_TIMESLICES$train %>% sapply(length)
VECTOR_TIMESLICES$train[[1]]
VECTOR_TIMESLICES$train %>% .[[length(.)]]


VECTOR_TIMESLICES$test %>% length
VECTOR_TIMESLICES$test %>% sapply(length)

input_x<- DATA_TRAIN[, c("WD10","WD80","WS102","WS103","WS803")]
input_y<- DATA_TRAIN[,"P_MWH"]$P_MWH

glmnet.mod <- train(x = input_x,
                    y = input_y,
                    method = "glmnet",
                    family = "gaussian",
                    trControl = myTimeControl,
                    tuneLength=tuneLength.num)

pois.mod <- train(x = input_x,
                  y = input_y,
                  method = "glmnet",
                  family = "poisson",
                  trControl = myTimeControl,
                  tuneLength=tuneLength.num)

lm.mod <- train(x = input_x,
                y = input_y,
                method = "lm",
                trControl = myTimeControl,
                tuneLength=tuneLength.num)

earth.mod <- train(x = input_x,
                   y = input_y,
                   method = "earth",
                   trControl = myTimeControl,
                   tuneLength=tuneLength.num)

earth.pois.mod <- train(x = input_x,
                        y = input_y,
                        method = "earth",
                        glm=list(family=poisson),
                        trControl = myTimeControl,
                        tuneLength=tuneLength.num)

gam.mod <- train(x = input_x,
                 y = input_y,
                 method = "gam",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num)

rpart.mod <- train(x = input_x,
                   y = input_y,
                   method = "rpart",
                   trControl = myTimeControl,
                   tuneLength=tuneLength.num)

party.mod <- train(x = input_x,
                   y = input_y,
                   method = "ctree",
                   trControl = myTimeControl,
                   tuneLength=tuneLength.num)

rf.mod <- train(x = input_x,
                y = input_y,
                method = "rf",
                trControl = myTimeControl,
                tuneLength=tuneLength.num)


gbm.mod <- train(x = input_x,
                 y = input_y,
                 method = "gbm",
                 distribution="poisson",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

xgb.mod <- train(x = input_x,
                 y = input_y,
                 method = "xgbTree",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

MODEL_LISTS<- list(glmnet = glmnet.mod,
                   glmnet.pois = pois.mod,
                   lm = lm.mod,
                   earth=earth.mod,
                   earth.pois=earth.pois.mod,
                   #gbm=gbm.mod,
                   gam=gam.mod,
                   rf=rf.mod,
                   rpart=rpart.mod,
                   party=party.mod,
                   xgboost= xgb.mod)
saveRDS(MODEL_LISTS, here::here('XGBOOST/GFS_WAUBRA/BENCHMARK_MODELS.RDS'))

resamps <- resamples(MODEL_LISTS)
resamps

ss <- summary(resamps)

knitr::kable(ss[[3]]$Rsquared)
knitr::kable(ss[[3]]$RMSE)
knitr::kable(ss[[3]]$MAE)



library(lattice)

trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")
dotplot(resamps, metric = "MAE")

dotplot(resamps, metric = "Rsquared")

