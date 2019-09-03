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



def EXTRACT_VARIABLES_FROM_GRIB(NAM12_FILE_NAME, CUSTOM_WORD):
    '''
    FUNCION QUE NOS DEVUELVE LA TABLA DE VARIABLES CONTENIDA EN EL GRIB... Y LA GUARDA A UN CSV
    ADEMAS PODEMOS DECIRLE QUE NOS DE UNA TABLADE VARIABLES PERSONALIZADA... INTRODUCIONDO UNA 
    PALABRA QUE BUSCARA ENTRE LOS NOMBRES DE LAS VARIABLES
    '''
    NAM12_GRIB= pygrib.open(NAM12_FILE_NAME)
    
    VARIABLE_NAMES=[]
    k=1
    NAM12_GRIB.seek(0)
    for i in NAM12_GRIB:
        VARIABLE_NAMES.append(str(i).split(':'))
        k=k+1
    
    colnames=  ['index','NAME', 'UNITS', 'PROJ', 'TYPE_VAR', 'LEVEL_Pa', 'TIMESTAMP', 'DATE']   
    TABLA_VAR= pd.DataFrame(VARIABLE_NAMES, columns= colnames)
    
    
    '''
    PARA VER TODOS LOS VALORES UNICOS DE CADA COLUMNA
    '''
    TABLA_CUSTOM = TABLA_VAR[TABLA_VAR.NAME.isin(set([item for item in list(TABLA_VAR['NAME']) if CUSTOM_WORD in item.lower()]))]
    
    PATH_SAVE= os.getcwd() + '/CSV_VARIABLES/' +NAM12_GRIB.name.split('/')[-1].replace('.','') + '/'
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
    if len(LONG_NP.shape)==2:
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

def EXTRACT_INFO_HIRLAM_TO_CSV(HIRLAM_FILE_NAME,
                              SELECTED_VARIABLES,
                              NEAREST_POINTS,
                              CUT_DATA):
    '''
    FUNCION PARA EXTRAER LA INFORMACION PARA LAS VARIABLES REQUERIDAS
    ESTAS VARIABLES SE PUEDEN SACAR EMPLEANDO LA FUNCION EXTRACT_VARIABLES_FROM_GRIB.
    CUT DATA ES UN BOOLEANO. IF TRUE SE CORTA LA INFORMACION DE ACUERDO A NEAREST POINTS    
    '''
    
    
    HIRLAM_GRIB= pygrib.open(HIRLAM_FILE_NAME)
    
    NOMBRE_ARCHIVO = HIRLAM_FILE_NAME.replace(HIRLAM_FILE_NAME.split('/')[-1], '') + HIRLAM_FILE_NAME.split('/')[-1] + '_CONVERTED.csv'
    TABLA_TOTAL= pd.DataFrame()
    for i in range(SELECTED_VARIABLES.shape[0]):
        
        VAR_LEVEL= SELECTED_VARIABLES['TIMESTAMP'].iloc[i]
        VAR_NAME= SELECTED_VARIABLES['NAME'].iloc[i]
                
        EXE = 1
        if os.path.isfile(NOMBRE_ARCHIVO):
            print(NOMBRE_ARCHIVO +  ' YA EXISTE')
            EXE=0
            if os.stat(NOMBRE_ARCHIVO).st_size < 2000:
                print(NOMBRE_ARCHIVO +  ' PERO ESTA VACIO')
                EXE=1
        if EXE==1:       
            try:
                
                VARIABLE = HIRLAM_GRIB.select(name= VAR_NAME)
                VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-2]==VAR_LEVEL]
            
            
            
                '''
                SACAMOS ARRAY DE VALORES, LONGITUD, LATITUD Y DATE
                AUNQUE HAY MUCHAS MAS VARIABLES CONTENIDAS EN EL FICHERO.
                TODAS ESTAS VARIABLES SE PUEDEN VER EJECUTANDO X.keys()
                
                '''
                VARIABLE_VALUES = VARIABLE_ITEM[0].values
                LATS , LONS = VARIABLE_ITEM[0].latlons()
                DATE = VARIABLE_ITEM[0].validDate            
              
                #print('CREANDO TABLA')  
                dfObj = pd.DataFrame()
                dfObj['VALUES']= VARIABLE_VALUES.ravel()
                dfObj['LON']= LONS.ravel()
                dfObj['LAT']= LATS.ravel()
                dfObj['DATE'] = DATE
                
                if CUT_DATA:
                    TABLA_CUT = dfObj[(dfObj['LON'].isin(NEAREST_POINTS['LON'])) & (dfObj['LAT'].isin(NEAREST_POINTS['LAT']))]
                else:
                    TABLA_CUT= dfObj
                
                TABLA_CUT['VAR_NAME']= str(VARIABLE_ITEM[0]).split(':')[1]
                TABLA_CUT['LEVEL']= str(VARIABLE_ITEM[0]).split(':')[-3]
                TABLA_CUT['FCST_TIME']= str(VARIABLE_ITEM[0]).split(':')[-2]
                TABLA_TOTAL = TABLA_TOTAL.append(TABLA_CUT)
 
            except:
                print(HIRLAM_FILE_NAME + ' NO ENCONTRADO ' + VAR_NAME)
                

    
    if not TABLA_TOTAL.empty:
        print('GUARDANDO ' + NOMBRE_ARCHIVO)
        TABLA_TOTAL.to_csv(NOMBRE_ARCHIVO)
    return(HIRLAM_FILE_NAME)



pd.set_option('display.max_columns', 15)
pd.set_option('display.max_rows', 30)



HIRLAM_FILENAME= '/home/meteobit/Escritorio/Parques/EZ/ATHd00.20190818'

TABLA_VARIABLES = EXTRACT_VARIABLES_FROM_GRIB(HIRLAM_FILENAME, 'wind')

#LA MANERA DE FILTRAR LAS VARIABLES HAY QUE AFINARLO
TABLA_WIND= TABLA_VARIABLES[TABLA_VARIABLES.NAME.isin(set([item for item in list(TABLA_VARIABLES['NAME']) if 'wind' in item.lower()]))]

TIME_STAMP= [item for item in list(set(TABLA_WIND['TIMESTAMP'])) if '(max)' not in item]
VARIABLES = list(set(TABLA_WIND['NAME']))[1:5]


TABLA_SELECTED_VAR= TABLA_WIND[(TABLA_WIND['TIMESTAMP'].isin(TIME_STAMP)) & (TABLA_WIND['NAME'].isin(VARIABLES))]




HIRLAM= pygrib.open(HIRLAM_FILENAME)

VARIABLE_GRIB = HIRLAM.select(name= VARIABLES[0])

VARIABLE_SELECTED = [item for item in VARIABLE_GRIB if str(item).split(':')[-2]==TIME_STAMP[0]][0]

LATS , LONS = VARIABLE_SELECTED.latlons()


LON_CERROBLANCO= -2.409666 	
LAT_CERROBLANCO= 38.72292


NEAREST_POINTS_CERROBLANCO = OBTAIN_50_NEAREST_POINTS(LONS,LATS, LON_CERROBLANCO, LAT_CERROBLANCO)

PATH_TO_HIRLAM= '/home/meteobit/Escritorio/Parques/EZ/'

HIRLAM_FILES = [PATH_TO_HIRLAM + item for item in os.listdir(PATH_TO_HIRLAM) if 'ATH' in item]
HIRLAM_FILES = [item for item in HIRLAM_FILES if not item.endswith('.csv')]



for i in HIRLAM_FILES:
    EXTRACT_INFO_HIRLAM_TO_CSV(i, 
                               CUT_DATA=True,
                               NEAREST_POINTS=NEAREST_POINTS_CERROBLANCO,
                               SELECTED_VARIABLES=TABLA_SELECTED_VAR)




HIRLAM_FILES = [PATH_TO_HIRLAM + item for item in os.listdir(PATH_TO_HIRLAM)]
HIRLAM_FILES = [item for item in HIRLAM_FILES if not item.endswith('.csv')]

HUECOS= pd.read_csv('/home/meteobit/MB/Data/Parques/PRUEBA_EOLICOS/CERROBLANCO/HIRLAM/HUECOS_HIRLAM_2019-09-03.csv')


import re
PATTERN_DATE = re.compile(r'\d{8}')


HIRLAM_DATE= [re.findall(PATTERN_DATE, item)[0] for item in HIRLAM_FILES if re.findall(PATTERN_DATE, item)]


HIRLAM_DISPONIBLE= [item.replace('-','') for item in HUECOS['x'] if item.replace('-','')  in HIRLAM_DATE] 


HIRLAM_FILES_FALTANTES= [item for item in HIRLAM_FILES if re.findall(PATTERN_DATE, item) if re.findall(PATTERN_DATE, item)[0] in HIRLAM_DISPONIBLE]



EXTRACT_INFO_HIRLAM_TO_CSV(HIRLAM_FILES_FALTANTES[0], 
                           CUT_DATA=True,
                           NEAREST_POINTS=NEAREST_POINTS_CERROBLANCO,
                           SELECTED_VARIABLES=TABLA_SELECTED_VAR)
