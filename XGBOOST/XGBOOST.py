#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep 10 11:12:44 2019

@author: oscar
"""

import pandas as pd
import numpy as np
import matplotlib.pylab as plt
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 15, 6

from sklearn.utils import check_array
import xgboost as xgb
from sklearn.model_selection import KFold
from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV 
from sklearn.metrics import mean_squared_error, mean_absolute_error, median_absolute_error
import pickle

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

def rmse(y_true, y_pred):
    return np.sqrt(mean_squared_error(y_true, y_pred))

path = '/media/oscar/4259-F95B/GEFCom2012-Wind-R/data/raw/'

data = pd.read_csv(path + 'train.csv', sep= '\t').drop(['wp2','wp3','wp4','wp5','wp6','wp7'],1)
data.columns = ['time', 'wp1']

time_train1 = 1000
data_train = data.iloc[0:time_train1+1,:]

time_train2 = 1000
data_w = pd.read_csv(path + 'windforecasts_wf1.csv') 
data_w.columns = ['date', 'hour', 'u', 'v', 'ws', 'wd']
data_w_train = data_w.iloc[0:time_train2,:]

time_train = 24*2
data_train['u'] = [0]*len(data_train)
data_train['v'] = [0]*len(data_train)
data_train['ws'] = [0]*len(data_train)
data_train['wd'] = [0]*len(data_train)
 
data_train    
for j in range(10):
    for i in range(time_train):
        data_train.iloc[12*j+i+1,2] = data_w_train['u'][48*j+i]
        data_train.iloc[12*j+i+1,3] = data_w_train['v'][48*j+i]
        data_train.iloc[12*j+i+1,4] = data_w_train['ws'][48*j+i]
        data_train.iloc[12*j+i+1,5] = data_w_train['wd'][48*j+i] 
    
df = data_train.iloc[0:157,:]





label_df = df['wp1']
df.drop(['time','wp1'], axis=1, inplace=True)


X_train = df
y_train = label_df


## Xgboost
xgb_regr = xgb.XGBRegressor()
xgb_regr.fit(X_train, y_train)

y_train_pred_xgb = xgb_regr.predict(X_train)



plt.plot(y_train) 
plt.plot(y_train_pred_xgb, color='red')



from sklearn import linear_model

regr = linear_model.LinearRegression()

# Train the model using the training sets
regr.fit(X_train, y_train)
y_train_pred_regr = regr.predict(X_train)

plt.plot(y_train) 
plt.plot(y_train_pred_regr, color='red')





'''
###############################################################################
###############################################################################
APLICAMOS LO DEL CHINO A TAMAULIPAS
###############################################################################
###############################################################################
'''
path_csv= '/home/meteobit/MB/Data/Parques/PRUEBA_EOLICOS/TAMAULIPAS_DATA/NAM_12_TAMAULIPAS_WITH_PRODUCTION.csv'
data = pd.read_csv(path_csv)

LON_TAMAULIPAS= data[['LON.y']].iloc[0]
LAT_TAMAULIPAS= data[['LAT.y']].iloc[0]

from math import sin, cos, sqrt, atan2, radians
import datetime as datetime

data['dist']= 'NA'

# approximate radius of earth in km
R = 6373.0

def DISTANCE(row):
    lat1 = radians(row['LAT.x'])
    lon1 = radians(row['LON.x'])
    lat2 = radians(LAT_TAMAULIPAS)
    lon2 = radians(LON_TAMAULIPAS)
    
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    
    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    
    return (R * c)



def U_Y_V_FROM_WS_WD(row, U= True, wd= 'WD80', ws= 'WS80' ):
    DIR = radians(row[wd])
    WS = radians(row[ws])
    if U:
        return(-sin(DIR)*WS)
    else:
        return(-cos(DIR)*WS)
        
from scipy.stats.stats import pearsonr

def CORR_WS_WP (gr):
    gr= gr.sort_values(by='DATE')
    gr=gr.dropna()
    try:
       CORR= pearsonr(gr.WS80, gr.PRUDUCCION_MWH)[0]
    except:
       CORR= pearsonr(gr.WS10, gr.PRUDUCCION_MWH)[0]
    return CORR
        
 

data['dist'] = data.apply (lambda row: DISTANCE(row), axis=1)


data['U80'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, True), axis=1)
data['V80'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, False), axis=1)

data['U10'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, True, wd= 'WD10', ws= 'WS10'), axis=1)
data['V10'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, False, wd= 'WD10', ws= 'WS10'), axis=1)




data= data[data.dist< 100]

VAR1 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS80', 'WD80','PRUDUCCION_MWH']
VAR2 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS10', 'WD10','PRUDUCCION_MWH']
VAR3 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS80', 'WD80','U80', 'V80','PRUDUCCION_MWH']
VAR4 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x', 'WS10', 'WD10','U10', 'V10','PRUDUCCION_MWH']
VAR5 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS80', 'WD80','U80', 'V80',
                'WS10', 'WD10','U10', 'V10','PRUDUCCION_MWH']

variables_modelo = [VAR1, VAR2, VAR3,VAR4,VAR5]
TABLA_ACCURACY= pd.DataFrame(columns= ['RMSE', 'MEAN_AE', 'MEDIAN_AE', 'CORR'])

for i in range(len(variables_modelo)):
    data_cut= data[variables_modelo[i]]
    
    
    
    '''
    ###############################################################################
    COJEMOS EL PUNTO CON MEJOR CORRELACION
    ###############################################################################
    
    '''

    
    TABLA_COR= data_cut.groupby(['LON.x', 'LAT.x']).apply(CORR_WS_WP)
    
    data_grouped= data_cut.groupby(['LON.x', 'LAT.x'])
    #TABLA_COR[TABLA_COR.isin(np.sort(TABLA_COR)[::-1][0:5])].index
    
    data_group1= data_grouped.get_group(TABLA_COR.idxmax())
    
    #data_group1= data_cut
    
    data_group1=data_group1.dropna()
    
    data_group1['DATE'] = [datetime.datetime.strptime(x,'%Y-%m-%d %H:%M:%S') for x in data_group1['DATE']]
    
    FECHA_CORTE= datetime.datetime(2019,6,10)
    
    data_train =data_group1[ data_group1['DATE']<FECHA_CORTE]
    data_test =data_group1[ data_group1['DATE']>FECHA_CORTE]
    
    
    '''
    ###############################################################################
    ###############################################################################
    A PARTIR DE AQUI APLICAMOS EL XGBOOST
    ###############################################################################
    ###############################################################################
    
    '''
    var_train= data_train
    var_train.drop(['DATE'], axis=1, inplace=True)
    
    
    label_df = var_train['PRUDUCCION_MWH']
    var_train.drop(['PRUDUCCION_MWH'], axis=1, inplace=True)
    
    
    X_train = var_train
    y_train = label_df
    
    
    ## Xgboost
    xgb_regr = xgb.XGBRegressor()
    xgb_regr.fit(X_train, y_train)
    
    
    
    var_test= data_test
    var_test.drop(['DATE'], axis=1, inplace=True)
    
    
    label_df = var_test['PRUDUCCION_MWH']
    var_test.drop(['PRUDUCCION_MWH'], axis=1, inplace=True)
    
    
    X_test = var_test
    y_test = label_df
    
    
    y_train_pred_xgb = xgb_regr.predict(X_test)
    
    y_true =y_test.reset_index()['PRUDUCCION_MWH']
    y_pred= y_train_pred_xgb.ravel()
    

    TABLA_ACCURACY.loc[i]= [rmse(y_true, y_pred),
                       mean_absolute_error(y_true, y_pred),
                       median_absolute_error(y_true, y_pred),
                       pearsonr(y_true, y_pred)[0]]
    
    plt.plot(y_true) 
    plt.plot(y_pred, color='red')



TABLA_ACCURACY3= TABLA_ACCURACY




'''
###############################################################################
###############################################################################
APLICAMOS LO DEL CHINO A CERROBLANCO
###############################################################################
###############################################################################
'''
path_csv= '/home/oscar/MB/Data/Parques/PRUEBA_EOLICOS/CERROBLANCO/HIRLAM/HISTORICO_HIRLAM_WITH_PRUDCTION.csv'
data = pd.read_csv(path_csv)

LON_CERROBLANCO= data[['LON.y']].iloc[0]
LAT_CERROBLANCO= data[['LAT.y']].iloc[0]

import datetime as datetime

data['dist']= 'NA'

# approximate radius of earth in km
R = 6373.0

def DISTANCE(row):
    lat1 = radians(row['LAT.x'])
    lon1 = radians(row['LON.x'])
    lat2 = radians(LAT_CERROBLANCO)
    lon2 = radians(LON_CERROBLANCO)
    
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    
    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    
    return (R * c)



def U_Y_V_FROM_WS_WD(row, U= True, wd= 'WD100', ws= 'WS100' ):
    DIR = radians(row[wd])
    WS = radians(row[ws])
    if U:
        return(-sin(DIR)*WS)
    else:
        return(-cos(DIR)*WS)
        
from scipy.stats.stats import pearsonr

def CORR_WS_WP (gr):
    gr= gr.sort_values(by='DATE')
    gr=gr.dropna()
    try:
       CORR= pearsonr(gr.WS100, gr.PRUDUCCION_MWH)[0]
    except:
       CORR= pearsonr(gr.WS10, gr.PRUDUCCION_MWH)[0]
    return CORR
        
 

data['dist'] = data.apply (lambda row: DISTANCE(row), axis=1)


data['U100'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, True), axis=1)
data['V100'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, False), axis=1)

data['U10'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, True, wd= 'WD10', ws= 'WS10'), axis=1)
data['V10'] = data.apply (lambda row: U_Y_V_FROM_WS_WD(row, False, wd= 'WD10', ws= 'WS10'), axis=1)




data= data[data.dist< 100]
data= data[data.FCST_TIME < 25]


VAR1 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS100', 'WD100','PRUDUCCION_MWH']
VAR2 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS10', 'WD10','PRUDUCCION_MWH']
VAR3 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS100', 'WD100','U100', 'V100','PRUDUCCION_MWH']
VAR4 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x', 'WS10', 'WD10','U10', 'V10','PRUDUCCION_MWH']
VAR5 = ['DATE', 'FCST_TIME', 'LON.x', 'LAT.x','WS100', 'WD100','U100', 'V100',
                'WS10', 'WD10','U10', 'V10','PRUDUCCION_MWH']

variables_modelo = [VAR1, VAR2, VAR3,VAR4,VAR5]
TABLA_ACCURACY= pd.DataFrame(columns= ['RMSE', 'MEAN_AE', 'MEDIAN_AE', 'CORR'])

for i in range(len(variables_modelo)):
    data_cut= data[variables_modelo[i]]
    
    
    
    '''
    ###############################################################################
    COJEMOS EL PUNTO CON MEJOR CORRELACION
    ###############################################################################
    
    '''

    
    TABLA_COR= data_cut.groupby(['LON.x', 'LAT.x']).apply(CORR_WS_WP)
    
    data_grouped= data_cut.groupby(['LON.x', 'LAT.x'])
    #TABLA_COR[TABLA_COR.isin(np.sort(TABLA_COR)[::-1][0:5])].index
    
    data_group1= data_grouped.get_group(TABLA_COR.idxmax())
    
    #data_group1= data_cut
    
    data_group1=data_group1.dropna()
   
    '''
    data_group1['PLAG_1']= data_group1['PRUDUCCION_MWH'].shift(1)
    data_group1['PLAG_2']= data_group1['PRUDUCCION_MWH'].shift(2)
    data_group1['PLAG_3']= data_group1['PRUDUCCION_MWH'].shift(3)
    data_group1['PLAG_4']= data_group1['PRUDUCCION_MWH'].shift(4)
    data_group1['PLAG_5']= data_group1['PRUDUCCION_MWH'].shift(5)
    data_group1['PLAG_6']= data_group1['PRUDUCCION_MWH'].shift(6)
    data_group1['PLAG_8']= data_group1['PRUDUCCION_MWH'].shift(8)
    data_group1['PLAG_10']= data_group1['PRUDUCCION_MWH'].shift(10)
    data_group1['PLAG_20']= data_group1['PRUDUCCION_MWH'].shift(20)
    data_group1['PLAG_21']= data_group1['PRUDUCCION_MWH'].shift(21)
    data_group1['PLAG_24']= data_group1['PRUDUCCION_MWH'].shift(24)
    '''
    data_group1['DATE'] = [datetime.datetime.strptime(x,'%Y-%m-%d %H:%M:%S') for x in data_group1['DATE']]
    
    FECHA_CORTE= datetime.datetime(2019,6,10)
    
    data_train =data_group1[ data_group1['DATE']<FECHA_CORTE]
    data_test =data_group1[ data_group1['DATE']>FECHA_CORTE]
    
    
    '''
    ###############################################################################
    ###############################################################################
    A PARTIR DE AQUI APLICAMOS EL XGBOOST
    ###############################################################################
    ###############################################################################
    
    '''
    var_train= data_train
    var_train.drop(['DATE'], axis=1, inplace=True)
    
    
    label_df = var_train['PRUDUCCION_MWH']
    var_train.drop(['PRUDUCCION_MWH'], axis=1, inplace=True)
    
    
    X_train = var_train
    y_train = label_df
    
    
    ## Xgboost
    xgb_regr = xgb.XGBRegressor()
    xgb_regr.fit(X_train, y_train)
    
    
    
    var_test= data_test
    var_test.drop(['DATE'], axis=1, inplace=True)
    
    
    label_df = var_test['PRUDUCCION_MWH']
    var_test.drop(['PRUDUCCION_MWH'], axis=1, inplace=True)
    
    
    X_test = var_test
    y_test = label_df
    
    
    y_train_pred_xgb = xgb_regr.predict(X_test)
    
    y_true =y_test.reset_index()['PRUDUCCION_MWH']
    y_pred= y_train_pred_xgb.ravel()
    

    TABLA_ACCURACY.loc[i]= [rmse(y_true, y_pred),
                       mean_absolute_error(y_true, y_pred),
                       median_absolute_error(y_true, y_pred),
                       pearsonr(y_true, y_pred)[0]]
    
    plt.plot(y_true) 
    plt.plot(y_pred, color='red')



TABLA_ACCURACY2= TABLA_ACCURACY
