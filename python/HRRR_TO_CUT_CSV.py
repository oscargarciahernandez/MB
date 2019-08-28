#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 28 10:32:53 2019

@author: meteobit
"""
import os 
import pandas as pd
import re
import pygrib
import os
import pandas as pd
from math import sin, cos, sqrt, atan2, radians
import numpy as np
import re
import cython



pd.set_option('display.max_columns', 15)
pd.set_option('display.max_rows', 30)



def EXTRACT_VARIABLES_FROM_GRIB(HRRR_FILE_NAME, CUSTOM_WORD):
    '''
    FUNCION QUE NOS DEVUELVE LA TABLA DE VARIABLES CONTENIDA EN EL GRIB... Y LA GUARDA A UN CSV
    ADEMAS PODEMOS DECIRLE QUE NOS DE UNA TABLADE VARIABLES PERSONALIZADA... INTRODUCIONDO UNA 
    PALABRA QUE BUSCARA ENTRE LOS NOMBRES DE LAS VARIABLES
    
    '''
    HRRR_GRIB= pygrib.open(HRRR_FILE_NAME)
    
    VARIABLE_NAMES=[]
    k=1
    HRRR_GRIB.seek(0)
    for i in HRRR_GRIB:
        VARIABLE_NAMES.append(str(i).split(':'))
        k=k+1
    
    colnames=  ['index','NAME', 'UNITS', 'PROJ', 'TYPE_VAR', 'LEVEL_Pa', 'TIMESTAMP', 'DATE']   
    TABLA_VAR= pd.DataFrame(VARIABLE_NAMES, columns= colnames)
    
    
    '''
    PARA VER TODOS LOS VALORES UNICOS DE CADA COLUMNA
    '''
    TABLA_CUSTOM = TABLA_VAR[TABLA_VAR.NAME.isin(set([item for item in list(TABLA_VAR['NAME']) if CUSTOM_WORD in item.lower()]))]
    
    PATH_SAVE= os.getcwd() + '/CSV_VARIABLES/' +HRRR_GRIB.name.split('/')[-1].replace('.','') + '/'
    if not os.path.isdir(PATH_SAVE):
        os.makedirs(PATH_SAVE, exist_ok=True)
    
    print('TABLA DE VARIABLES GUARDADA EN ' + PATH_SAVE + 'TABLA_VARIABLES.csv')
    print('TABLA DE VARIABLES DE VIENTO GUARDADA EN ' + PATH_SAVE + 'TABLA_WIND.csv')
    
    TABLA_CUSTOM.to_csv(PATH_SAVE + 'TABLA_'+ CUSTOM_WORD+'.csv', index = False, sep= ';')
    TABLA_VAR.to_csv(PATH_SAVE + 'TABLA_VARIABLES.csv', index = False, sep= ';')
    
    return TABLA_VAR


def OBTAIN_50_NEAREST_POINTS(LONG_NP, LAT_NP, LON, LAT):
    '''
    FUNCION PARA OBTENER LOS 50 PUNTOS MAS CERCANOS A LA ZONA A ESTUDIAR...
    SACAR INFORMACION DE TODOS LOS PUNTOS ES MUY EXIGENTE PARA ESTE SCRIPT
    '''    
    if len(LONG_NP)==2:
        TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
        TABLA_LONLAT['LON']= LONG_NP[:, :].ravel()
        TABLA_LONLAT['LAT']= LAT_NP[:, :].ravel()
    else:
        TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
        TABLA_LONLAT['LON']= LONG_NP
        TABLA_LONLAT['LAT']= LAT_NP
    '''
    METODO MAS CLARO PERO MAS LENTO... EL METODO ANTERIOR ESTA BIEN¡
    
    
    TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
    k=0
    for i in range(LONG_NP.shape[1]):
        for j in range(LAT_NP.shape[0]):
            TABLA_LONLAT.loc[k]= [LONG_NP[j,i], LAT_NP[j,i]]
            k=k+1
            
    '''
    # approximate radius of earth in km
    R = 6373.0
    
    DISTANCIA= []
    for i in range(len(TABLA_LONLAT['LAT'])):
        lat1 = radians(TABLA_LONLAT['LAT'][i])
        lon1 = radians(TABLA_LONLAT['LON'][i])
        lat2 = radians(LAT)
        lon2 = radians(LON)
        
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        
        a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
        c = 2 * atan2(sqrt(a), sqrt(1 - a))
        
        DISTANCIA.append(R * c)
    
    
    
    TABLA_LONLAT.insert(column= 'DIST',   loc= 0,  value= DISTANCIA)
    
    TABLA_ORD_DIST= TABLA_LONLAT.sort_values(by='DIST')
    
    
    return TABLA_ORD_DIST[0:50]





HRRR_PATH= '/media/meteobit/Elements/HRRR_from_UofU/'
HRRR_FILES= os.listdir(HRRR_PATH)

HRRR_csv = [item for item in HRRR_FILES if 'csv' in item]

PATTERN_HOURS= re.compile(r't(06|00|12|18)')
HRRR_csv = [item for item in HRRR_csv if re.search(PATTERN_HOURS, item)]


CSV_HRRR= pd.read_csv(HRRR_PATH + HRRR_csv[0])

LATS = CSV_HRRR['LAT']
LONS= CSV_HRRR['LON']

LON_TATANKA= -98.955699 	
LAT_TATANKA= 45.95685
if  os.path.isfile(HRRR_PATH +  'NEAREST_POINTS_TATANKA.csv'):
    NEAREST_POINTS_TATANKA = pd.read_csv(HRRR_PATH +  'NEAREST_POINTS_TATANKA.csv')
    
else:
    NEAREST_POINTS_TATANKA = OBTAIN_50_NEAREST_POINTS(LONS, LATS, LON_TATANKA, LAT_TATANKA)
    NEAREST_POINTS_TATANKA.to_csv(HRRR_PATH + 'NEAREST_POINTS_TATANKA.csv')


LON_TAMAULIPAS= -98.196080 	
LAT_TAMAULIPAS= 25.78788
if  os.path.isfile(HRRR_PATH +  'NEAREST_POINTS_TAMAULIPAS.csv'):
    NEAREST_POINTS_TAMAULIPAS = pd.read_csv(HRRR_PATH +  'NEAREST_POINTS_TAMAULIPAS.csv')
else:
    NEAREST_POINTS_TAMAULIPAS = OBTAIN_50_NEAREST_POINTS(LONS, LATS, LON_TAMAULIPAS, LAT_TAMAULIPAS)
    NEAREST_POINTS_TAMAULIPAS.to_csv(HRRR_PATH + 'NEAREST_POINTS_TAMAULIPAS.csv')
    
    
    
VARIABLE_GROUPS = list(set([item.split('.')[3] for item in HRRR_csv]))


PATTERN_TSIM= re.compile(r't(\d{2})')
PATTERN_HOURS= re.compile(r'wrfsfcf(\d{2})')

CSV_HRRR= pd.read_csv(HRRR_PATH + HRRR_csv[0])
TABLA_CUT_TATANKA = CSV_HRRR[(CSV_HRRR['LON'].isin(NEAREST_POINTS_TATANKA['LON'])) & (CSV_HRRR['LAT'].isin(NEAREST_POINTS_TATANKA['LAT']))]

INDEX_1= list(TABLA_CUT_TATANKA.index.values.astype(int))
INDEX_1.append(0)

INDEX_ALL = list(CSV_HRRR.index.values.astype(int))
NON_DESIRED = [item for item in INDEX_ALL if item not in INDEX_1 ]


for var_group in VARIABLE_GROUPS:
    HRRR_CSV_FILT= [item for item in HRRR_csv if var_group in item ]
    
    TABLA_FILT = pd.DataFrame()
    for i in HRRR_CSV_FILT:
        print(i)
        CSV_HRRR= pd.read_csv(HRRR_PATH + i, skiprows = NON_DESIRED)
        
        TABLA_CUT_TATANKA = CSV_HRRR[(CSV_HRRR['LON'].isin(NEAREST_POINTS_TATANKA['LON'])) & (CSV_HRRR['LAT'].isin(NEAREST_POINTS_TATANKA['LAT']))]
        TABLA_CUT_TATANKA['TSIM'] = re.findall(PATTERN_TSIM, i)[0]
        TABLA_CUT_TATANKA['SIMT'] = re.findall(PATTERN_HOURS, i)[0]
        
        TABLA_FILT= TABLA_FILT.append(TABLA_CUT_TATANKA)
    
    TABLA_FILT.to_csv(HRRR_PATH + var_group)


