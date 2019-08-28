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



FILES_WRF = os.listdir('/home/meteobit/Descargas/')
LIST_DISTANCIA = []
for file in [item for item in FILES_WRF if 'wrf_arw' in item]:
    FILENAME = '/home/meteobit/Descargas/' +  file
    
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
    
    LON_CERROBLANCO = -2.409666 	
    LAT_CERROBLANCO  =  38.72292
    LIST_DISTANCIA.append(OBTAIN_50_NEAREST_POINTS(LONS[:], LATS[:], LON_CERROBLANCO, LAT_CERROBLANCO))







import os
import requests
from bs4 import BeautifulSoup
import tqdm
import multiprocessing as mp
#ESTE SCRIPT ES PARA OBTENER TODAS LAS URLS DE LOS GRIBS QUE INICIALIZAN AL MODELO
# TIENEN ESTA FORMA PARA IR POR CAPAS, 
# PRIMERO OBTIENE LOS DATOS DE LOS MESES
#LUEGO OBTIENE LOS DATOS DE LOS DIAS
# LUEGO ENTRA EN LOS DIAS Y OBTIENE LA URL DEL ARCHIVO GRIB QUE ES EL QUE DESEAMOS
# DE ESTA FORMA LAS TODAS LAS URLS DISPONIBLES SE ENCUENTRAN EN LISTA_DISPONIBLE2
# LUEGO HAREMOS UN SCRIPT PARA SELECCIONAR LAS QUE QUERAMOS Y DESCARGARLA. 


URL_BUSQUEDA= 'http://mandeo.meteogalicia.es/thredds/catalog/modelos/WRF_HIST/d02/catalog.html'


#INICIAMOS SESION REQUEST
session = requests.Session()
session.trust_env = False
GET_CAPA1= session.get(URL_BUSQUEDA)


#PARSEANDO HTML
user_soup = BeautifulSoup(GET_CAPA1.content, 'html.parser')
user_soup1=user_soup.find_all('a', href= True)

Lista_disponible= []
for i in user_soup1:
    Lista_disponible.append(URL_BUSQUEDA.replace('catalog.html','') + i['href'])
    
        


Lista_disponible1= []
for j in range(len(Lista_disponible)): 
    URL_MES= Lista_disponible[j]
    GET_CAPA2= session.get(URL_MES)
    user_soup = BeautifulSoup(GET_CAPA2.content, 'html.parser')
    user_soup1=user_soup.find_all('a', href=True)
    for i in user_soup1:
        Lista_disponible1.append(URL_BUSQUEDA.replace('catalog.html','') + i['href'])
            
Lista_disponible2= []
for j in range(len(Lista_disponible1)): 
    URL_DIA= Lista_disponible1[j]
    GET_CAPA3= session.get(URL_DIA)
    user_soup = BeautifulSoup(GET_CAPA3.content, 'html.parser')
    user_soup1=user_soup.find_all('a')
    for i in user_soup1:
        Lista_disponible2.append(URL_BUSQUEDA.replace('catalog.html','') + i['href'])

LISTA_JUNIO2019_ALL= [item for item in Lista_disponible2 if '201906' in item]
LISTA_JUNIO2019= [item for item in LISTA_JUNIO2019_ALL if int(item[-3:])<49]
with open('URLS_GFS025_JUNIO_2019.txt', 'w') as f:
    for item in LISTA_JUNIO2019:
        f.write("%s\n" % item)


PATH_ELEMENTS= '/media/meteobit/Elements/GRIB025_JUNIO/'
if not os.path.exists(PATH_ELEMENTS):
    os.makedirs(PATH_ELEMENTS)
    

Gribs_downloaded= os.listdir(PATH_ELEMENTS)


#LEER LOS LINKS DESDE EL TXT
with open('URLS_GFS025_JUNIO_2019.txt') as f:
    Lista_JUNIO_2019= f.readlines()
    


#COMPROBAR CUALES ESTAN YA DESCARGADOS Y BORRARLOS DE LA LISTA
YA_DESCARGADOS= []
for i in range(len(Lista_JUNIO_2019)):
    if str(Lista_JUNIO_2019[i].split('/')[-1])[:-1] in Gribs_downloaded:
        YA_DESCARGADOS.append(i)
        
        
Lista_nueva= [item for item in Lista_JUNIO_2019  if not str(item.split('/')[-1])[:-1] in Gribs_downloaded]

'''
#DESCARGAR GRIBS
for i in range(len(Lista_nueva)): 
    session = requests.Session()
    session.trust_env = False
    grib= session.get(str(Lista_nueva[i])[:-1])  
    PATH_GRIB=  PATH_ELEMENTS + str(Lista_nueva[i])[:-1].split('/')[-1]
    with open( PATH_GRIB, 'wb') as f:
        print('Guardando ' + PATH_GRIB)
        f.write(grib.content)
'''   


def DOWNLOAD_GFS(URL):
    session = requests.Session()
    session.trust_env = False
    grib= session.get(str(URL)[:-1])  
    PATH_GRIB=  PATH_ELEMENTS + str(URL)[:-1].split('/')[-1]
    with open( PATH_GRIB, 'wb') as f:
        #print('Guardando ' + PATH_GRIB)
        f.write(grib.content)

pool = mp.Pool(mp.cpu_count()-2)
for _ in tqdm.tqdm(pool.imap_unordered(DOWNLOAD_GFS, Lista_nueva)  , total=len(Lista_nueva)):
    pass
#MATAMOS SUBPROCESOS 
pool.close()
pool.terminate()
pool.join()