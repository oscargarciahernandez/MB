#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 23 13:17:24 2019

@author: meteobit
"""

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
    cdef float R, c, a, lon1,lat1, lon2,lat2, dlon,lat
    
    TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
    TABLA_LONLAT['LON']= LONG_NP[:, :].ravel()
    TABLA_LONLAT['LAT']= LAT_NP[:, :].ravel()
    
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


def EXTRACT_CSV_INFO_HRRR(SELECTED_VARIABLES,
                          NEAREST_POINTS,
                          CUT_DATA,
                          HRRR_FILE_NAME):
    
    HRRR_GRIB= pygrib.open(HRRR_FILE_NAME)

    for i in range(SELECTED_VARIABLES.shape[0]):
        
        VAR_LEVEL= SELECTED_VARIABLES['LEVEL_Pa'].iloc[i]
        VAR_NAME= SELECTED_VARIABLES['NAME'].iloc[i]
        
        NOMBRE_ARCHIVO = HRRR_FILE_NAME.replace('grib2', '') + VAR_NAME.replace(' ', '_') + VAR_LEVEL.replace(' ', '_') + '.csv'
        
        if os.path.isfile(NOMBRE_ARCHIVO):
            print(NOMBRE_ARCHIVO +  ' YA EXISTE')
        else:        
            VARIABLE = HRRR_GRIB.select(name= VAR_NAME)
            VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-3]==VAR_LEVEL]
            
            
            
            '''
            SACAMOS ARRAY DE VALORES, LONGITUD, LATITUD Y DATE
            AUNQUE HAY MUCHAS MAS VARIABLES CONTENIDAS EN EL FICHERO.
            TODAS ESTAS VARIABLES SE PUEDEN VER EJECUTANDO X.keys()
            
            '''
            VARIABLE_VALUES = VARIABLE_ITEM[0].values
            LATS , LONS = VARIABLE_ITEM[0].latlons()
            DATE = VARIABLE_ITEM[0].validDate            
          
            print('CREANDO TABLA')  
            dfObj = pd.DataFrame()
            dfObj['VALUES']= VARIABLE_VALUES.ravel()
            dfObj['LON']= LONS.ravel()
            dfObj['LAT']= LATS.ravel()
            dfObj['DATE'] = DATE
            
            if CUT_DATA:
                TABLA_CUT = dfObj[(dfObj['LON'].isin(NEAREST_POINTS['LON'])) & (dfObj['LAT'].isin(NEAREST_POINTS['LAT']))]
            else:
                TABLA_CUT= dfObj
                
            print('GUARDANDO CSV en ' + NOMBRE_ARCHIVO)
            TABLA_CUT.to_csv(NOMBRE_ARCHIVO, index = False)



def MAIN():
    HRRR_PATH= '/media/oscar/Elements/HRRR_from_UofU/'
    HRRR_FILES= os.listdir(HRRR_PATH)
    
    
    '''
    LOS ARCHIVOS SE DIVIDEN POR NIVELES O POR SUPERFICIE
    '''
    
    HRRR_prs= [item for item in HRRR_FILES if 'wrfprs' in item]
    HRRR_sfc = [item for item in HRRR_FILES if 'wrfsfc' in item]
    HRRR_sfc = [item for item in HRRR_sfc if 'csv' not in item]
    
    PATTERN_HOURS= re.compile(r't(06|00|12|18)')
    HRRR_sfc = [item for item in HRRR_sfc if re.search(PATTERN_HOURS, item)]
    
                    
    CUSTOM_WORD =    'wind' 
    HRRR_FILE_NAME = HRRR_PATH + HRRR_sfc[0]
    
    
    
    
    TABLA_VARIABLES = EXTRACT_VARIABLES_FROM_GRIB(HRRR_FILE_NAME, CUSTOM_WORD)
    
    
    SELECTED_VARIABLES = TABLA_VARIABLES[TABLA_VARIABLES.NAME.isin(set([item for item in list(TABLA_VARIABLES['NAME']) if 'wind' in item.lower()]))]
    SELECTED_VARIABLES_LEVEL= SELECTED_VARIABLES[SELECTED_VARIABLES['LEVEL_Pa'].isin(['level 80 m', 'level 10 m'])]
    
    VAR_LEVEL= SELECTED_VARIABLES_LEVEL['LEVEL_Pa'].iloc[1]
    VAR_NAME= SELECTED_VARIABLES_LEVEL['NAME'].iloc[1]
    
    HRRR_FILE_NAME = HRRR_PATH + HRRR_sfc[0]
    HRRR_GRIB= pygrib.open(HRRR_FILE_NAME)
    VARIABLE = HRRR_GRIB.select(name= VAR_NAME)
    
    VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-3]==VAR_LEVEL]
    LATS , LONS = VARIABLE_ITEM[0].latlons()
    
    
    LON_TATANKA= -98.955699 	
    LAT_TATANKA= 45.95685
    
    NEAREST_POINTS = OBTAIN_50_NEAREST_POINTS(LONS, LATS, LON_TATANKA, LAT_TATANKA)
    
    
    HRRR_FILES_PATH = [HRRR_PATH + s for s in HRRR_sfc]
    
    for file in HRRR_FILES_PATH:
        EXTRACT_CSV_INFO_HRRR(SELECTED_VARIABLES_LEVEL,NEAREST_POINTS,False, file)
   