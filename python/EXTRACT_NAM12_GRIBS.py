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

def OBTAIN_50_NEAREST_POINTS(LONG_NP, LAT_NP, LON, LAT):
    '''
    FUNCION PARA OBTENER LOS 50 PUNTOS MAS CERCANOS A LA ZONA A ESTUDIAR...
    SACAR INFORMACION DE TODOS LOS PUNTOS ES MUY EXIGENTE PARA ESTE SCRIPT
    '''
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



pd.set_option('display.max_columns', 15)
pd.set_option('display.max_rows', 30)


NAM12_PATH= '/media/oscar/Elements/NAM12/'
NAM12_FILES= os.listdir(NAM12_PATH)


'''
TENEMOS 24 H DE SIMULACION VAMOS A HACER PRIMERO SOLAMENTE ENERO

'''


NAM12_JAN= [item for item in NAM12_FILES if '201901' in item]

EXTRAER_EN_BUCLE = False

if EXTRAER_EN_BUCLE:
    
    
    for i in range(len(NAM12_JAN)):
        NAM12_FILE_NAME = NAM12_PATH + NAM12_JAN[i]
        NAM12_GRIB= pygrib.open(NAM12_FILE_NAME)
        
        
        
        if 'TABLA_WIND' not in locals(): 
          
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
            for i in list(TABLA_VAR.columns):
                print(set(list(TABLA_VAR[i])))
            
            TABLA_WIND = TABLA_VAR[TABLA_VAR.NAME.isin(set([item for item in list(TABLA_VAR['NAME']) if 'wind' in item]))]
            
            PATH_TO_NAM12= os.getcwd() + '/NAM12/'
            if not os.path.isdir(PATH_TO_NAM12):
                os.mkdir(PATH_TO_NAM12)
                
            TABLA_WIND.to_csv(PATH_TO_NAM12 + 'TABLA_WIND_SFC.csv', index = False, sep= ';')
            TABLA_VAR.to_csv(PATH_TO_NAM12 + 'TABLA_VARIABLES_SFC.csv', index = False, sep= ';')
        
        
        '''
        TRAS SACAR LOS CSVs  Y VER LAS VARIABLES DECIDIMOS QUE SOLO NOS INTERESA SACAR LA VELOCIDAD DEL VIENTO 
        HASTA LOS 9000hpa
        
        ES DIFERENTE PARA LOS ARCHIVOS DE SFC Y LOS DE PRS. 
        PARA LOS DE SFC ES EL INDEX SUPERIOR A 27 Y PARA LOS PRS 
        SUPERIOR A 470.
        
        
        
        '''
        
        
        SELECTED_VARIABLES = TABLA_WIND[pd.to_numeric(TABLA_WIND['index']).isin([313,314,322,323,329,330,369,370,404,408])]
        
        
        
        '''
        ESTO DE ACONTINUACION ESTA PENSADO PARA HACER UN BUCLE CAMINANDO 
        POR ILOC[X] DONDE X ES EL NUMERO DE FILAS DE SELECTED
        VARIABLES... LO SUYO SERÁ EXTRAER UNICAMENTE LOS 50 PUNTOS 
        MAS CERCANOS Y HACER CSVs COMO YA HICIMOS CON TAMAULIPAS
        
        
        '''
        
        for i in range(SELECTED_VARIABLES.shape[0]):
            
            VAR_LEVEL= SELECTED_VARIABLES['LEVEL_Pa'].iloc[i]
            VAR_NAME= SELECTED_VARIABLES['NAME'].iloc[i]
            NOMBRE_ARCHIVO = NAM12_FILE_NAME.replace('grib2', '') + VAR_NAME.replace(' ', '_') + VAR_LEVEL.replace(' ', '_') + '.csv'
            
            if os.path.isfile(NOMBRE_ARCHIVO):
                print(NOMBRE_ARCHIVO +  ' YA EXISTE')
            else:
            
                VARIABLE = NAM12_GRIB.select(name= VAR_NAME)
                
                
                VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-3]==VAR_LEVEL]
                
                
                
                '''
                SACAMOS ARRAY DE VALORES, LONGITUD, LATITUD Y DATE
                AUNQUE HAY MUCHAS MAS VARIABLES CONTENIDAS EN EL FICHERO.
                TODAS ESTAS VARIABLES SE PUEDEN VER EJECUTANDO X.keys()
                
                '''
                VARIABLE_VALUES = VARIABLE_ITEM[0].values
                
                LATS , LONS = VARIABLE_ITEM[0].latlons()
                
                DATE = VARIABLE_ITEM[0].validDate
                
                VARIABLE_ITEM[0].keys()
                
                 	
                LON_TATANKA= -98.196080
                LAT_TATANKA= 25.78788
                
                if 'NEAREST_POINTS_TATANKA' not in locals():
                    NEAREST_POINTS_TATANKA = OBTAIN_50_NEAREST_POINTS(LONS, LATS, LON_TATANKA, LAT_TATANKA)
                
                
                
                DIMENSIONES= VARIABLE_VALUES.shape
                
                print('CREANDO TABLA')
                dfObj = pd.DataFrame(columns = ['DATE' , 'LON', 'LAT', 'VAR'])
                k= 0
                
                for lat in range(DIMENSIONES[0]):
                    for lon in range(DIMENSIONES[1]):
                        if LONS[lat, lon] in np.float64(NEAREST_POINTS_TATANKA['LON']):
                            if LATS[lat, lon] in np.float64(NEAREST_POINTS_TATANKA['LAT']):
                                    #print('TRUE')
                                    dfObj.loc[k]=[DATE.strftime(format= '%Y-%m-%d %X'), 
                                                  LONS[lat, lon], 
                                                  LATS[lat, lon], 
                                                  VARIABLE_VALUES[lat, lon]]
                                    k=k+1
                                   
                    
                print('GUARDANDO CSV en ' + NOMBRE_ARCHIVO)
                dfObj.to_csv(NOMBRE_ARCHIVO, index = False)
                
                
            
            




    
def EXTRACT_NAM12_CSV(NAM12_FILES):
    NAM12_FILE_NAME = NAM12_PATH + NAM12_FILES
    NAM12_GRIB= pygrib.open(NAM12_FILE_NAME)
    
    
    
    if 'TABLA_WIND' not in locals(): 
      
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
        for i in list(TABLA_VAR.columns):
            print(set(list(TABLA_VAR[i])))
        
        TABLA_WIND = TABLA_VAR[TABLA_VAR.NAME.isin(set([item for item in list(TABLA_VAR['NAME']) if 'wind' in item]))]
        
        PATH_TO_NAM12= os.getcwd() + '/NAM12/'
        if not os.path.isdir(PATH_TO_NAM12):
            os.mkdir(PATH_TO_NAM12)
            
        TABLA_WIND.to_csv(PATH_TO_NAM12 + 'TABLA_WIND_SFC.csv', index = False, sep= ';')
        TABLA_VAR.to_csv(PATH_TO_NAM12 + 'TABLA_VARIABLES_SFC.csv', index = False, sep= ';')
    
    
    '''
    TRAS SACAR LOS CSVs  Y VER LAS VARIABLES DECIDIMOS QUE SOLO NOS INTERESA SACAR LA VELOCIDAD DEL VIENTO 
    HASTA LOS 9000hpa
    
    ES DIFERENTE PARA LOS ARCHIVOS DE SFC Y LOS DE PRS. 
    PARA LOS DE SFC ES EL INDEX SUPERIOR A 27 Y PARA LOS PRS 
    SUPERIOR A 470.
    
    
    
    '''
    
    
    SELECTED_VARIABLES = TABLA_WIND[TABLA_WIND['LEVEL_Pa'].isin(['level 80 m', 'level 10 m', 'level 97500 Pa'])]
    
    
    
    '''
    ESTO DE ACONTINUACION ESTA PENSADO PARA HACER UN BUCLE CAMINANDO 
    POR ILOC[X] DONDE X ES EL NUMERO DE FILAS DE SELECTED
    VARIABLES... LO SUYO SERÁ EXTRAER UNICAMENTE LOS 50 PUNTOS 
    MAS CERCANOS Y HACER CSVs COMO YA HICIMOS CON TAMAULIPAS
    
    
    '''
    
    for i in range(SELECTED_VARIABLES.shape[0]):
        
        VAR_LEVEL= SELECTED_VARIABLES['LEVEL_Pa'].iloc[i]
        VAR_NAME= SELECTED_VARIABLES['NAME'].iloc[i]
        NOMBRE_ARCHIVO = NAM12_FILE_NAME.replace('grib2', '') + VAR_NAME.replace(' ', '_') + VAR_LEVEL.replace(' ', '_') + 'tamaulipas.csv'
        
        if os.path.isfile(NOMBRE_ARCHIVO):
            print(NOMBRE_ARCHIVO +  ' YA EXISTE')
        else:
        
            VARIABLE = NAM12_GRIB.select(name= VAR_NAME)
            
            
            VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-3]==VAR_LEVEL]
            
            
            
            '''
            SACAMOS ARRAY DE VALORES, LONGITUD, LATITUD Y DATE
            AUNQUE HAY MUCHAS MAS VARIABLES CONTENIDAS EN EL FICHERO.
            TODAS ESTAS VARIABLES SE PUEDEN VER EJECUTANDO X.keys()
            
            '''
            VARIABLE_VALUES = VARIABLE_ITEM[0].values
            
            LATS , LONS = VARIABLE_ITEM[0].latlons()
            
            DATE = VARIABLE_ITEM[0].validDate
            
            VARIABLE_ITEM[0].keys()
            
            
            LON_TATANKA= -98.196080
            LAT_TATANKA= 25.78788
            
            if 'NEAREST_POINTS_TATANKA' not in locals():
                NEAREST_POINTS_TATANKA = OBTAIN_50_NEAREST_POINTS(LONS, LATS, LON_TATANKA, LAT_TATANKA)
            
            
            
            DIMENSIONES= VARIABLE_VALUES.shape
            
            print('CREANDO TABLA')
            dfObj = pd.DataFrame(columns = ['DATE' , 'LON', 'LAT', 'VAR'])
            k= 0
            
            for lat in range(DIMENSIONES[0]):
                for lon in range(DIMENSIONES[1]):
                    if LONS[lat, lon] in np.float64(NEAREST_POINTS_TATANKA['LON']):
                        if LATS[lat, lon] in np.float64(NEAREST_POINTS_TATANKA['LAT']):
                                #print('TRUE')
                                dfObj.loc[k]=[DATE.strftime(format= '%Y-%m-%d %X'), 
                                              LONS[lat, lon], 
                                              LATS[lat, lon], 
                                              VARIABLE_VALUES[lat, lon]]
                                k=k+1
                               
                
            print('GUARDANDO CSV en ' + NOMBRE_ARCHIVO)
            dfObj.to_csv(NOMBRE_ARCHIVO, index = False)
            
             
NAM12_JAN= [item for item in NAM12_FILES if '201901' in item]
NAM12_JAN= [item for item in NAM12_JAN if '.csv' not in item]

import tqdm
import multiprocessing as mp
pool = mp.Pool(mp.cpu_count()-6)
for _ in tqdm.tqdm(pool.imap_unordered(EXTRACT_NAM12_CSV, NAM12_JAN)  , total=len(NAM12_JAN)):
    pass

#MATAMOS SUBPROCESOS 
pool.close()
pool.terminate()
pool.join()