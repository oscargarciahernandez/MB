#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 28 13:08:31 2019

@author: meteobit
"""

from netCDF4 import Dataset
import os
import subprocess
from wrf import getvar, ALL_TIMES, to_np
import re
from math import sin, cos, sqrt, atan2, radians
import pandas as pd
import numpy as np
import shutil

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


PATH_METEOGALICIA= '/home/meteobit/METEOGALICIA/'
FILES_WRF = os.listdir(PATH_METEOGALICIA)


FILENAME= [PATH_METEOGALICIA + item for item in FILES_WRF if 'wrf_arw' in item][0]
NETCDF=  Dataset(FILENAME)

DISPLAY_VARIABLES_DESCRIPTION= True
if DISPLAY_VARIABLES_DESCRIPTION: 
    for i in NETCDF.variables.keys():
        try:
            print(NETCDF.variables[i].description + ' ' + i)
        except:
            print(i + ' NO ES UNA VARIABLE')


TIME= NETCDF.variables['time']
LONS= NETCDF.variables['lon']
LATS= NETCDF.variables['lat']

 	
LON_CERROBLANCO= -2.409666
LAT_CERROBLANCO=  38.72292

NEAREST_POINTS_CERROBLANCO = OBTAIN_50_NEAREST_POINTS(LONS[:], LATS[:], LON_CERROBLANCO, LAT_CERROBLANCO)

LISTA_ERRORES= []
for file in sorted([PATH_METEOGALICIA + item for item in FILES_WRF if 'wrf_arw' in item]):
    FILENAME = file
    if os.path.isfile(file.replace('.nc4', 'CONVERTED.csv')):
        print(file.replace('.nc4', 'CONVERTED.csv') + ' YA EXISTE')
    else:
        try:
    
            NETCDF=  Dataset(FILENAME)
            
            TIME= NETCDF.variables['time']
            
            LONS= NETCDF.variables['lon']
            LATS= NETCDF.variables['lat']
            X= NETCDF.variables['x']
            Y= NETCDF.variables['y']
            
            U= NETCDF.variables['u']
            V= NETCDF.variables['v']
            
            Ulev1= NETCDF.variables['ulev1']
            Vlev1= NETCDF.variables['vlev1']
            
            Ulev2= NETCDF.variables['ulev2']
            Vlev2= NETCDF.variables['vlev2']
            
            Ulev3= NETCDF.variables['ulev3']
            Vlev3= NETCDF.variables['vlev3']
            
            TABLA_WIND= pd.DataFrame()
            for DATE in range(len(TIME)):
                dfObj = pd.DataFrame()
        
                dfObj['U10']= U[DATE,:].ravel()
                dfObj['V10']= V[DATE,:].ravel()
                dfObj['ULEV1']= Ulev1[DATE,:].ravel()
                dfObj['VLEV1']= Vlev1[DATE,:].ravel()
                dfObj['ULEV2']= Ulev2[DATE,:].ravel()
                dfObj['VLEV2']= Vlev2[DATE,:].ravel()
                dfObj['ULEV3']= Ulev3[DATE,:].ravel()
                dfObj['VLEV3']= Vlev3[DATE,:].ravel()
            
                dfObj['LON']= LONS[:].ravel()
                dfObj['LAT']= LATS[:].ravel()
                dfObj['DATE'] = TIME[DATE]
                dfObj['DATE_UNITS']= TIME.units
                TABLA_WIND= TABLA_WIND.append(dfObj)
                
            
            
            TABLA_CUT = TABLA_WIND[(TABLA_WIND['LON'].isin(NEAREST_POINTS_CERROBLANCO['LON'])) & (TABLA_WIND['LAT'].isin(NEAREST_POINTS_CERROBLANCO['LAT']))]               
            TABLA_CUT.to_csv(file.replace('.nc4', 'CONVERTED.csv'))
        except:
            print('ERROR EXTRAYENDO DATOS CONSULTA LISTA_ERRORES PARA SABER QUE ARCHIVO FALLA')
            LISTA_ERRORES.append(file)
            




JUNTAR_CSVS =False

if JUNTAR_CSVS:
    PATH_METEOGALICIA= '/home/meteobit/METEOGALICIA/'
    FILES_WRF = os.listdir(PATH_METEOGALICIA)
    
    METEOGALICIA_CSV= [PATH_METEOGALICIA + item for item in FILES_WRF if '.csv' in item]
    
    HISTORICO_METEOGALICIA= pd.DataFrame()
    for csv in METEOGALICIA_CSV: 
        HISTORICO_METEOGALICIA= HISTORICO_METEOGALICIA.append(pd.read_csv(csv))
    
    
    HISTORICO_METEOGALICIA.to_csv(PATH_METEOGALICIA + 'HISTORICO_METEOGALICIA.csv')
