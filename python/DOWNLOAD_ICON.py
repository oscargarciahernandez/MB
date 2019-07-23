#!/usr/bin/env python
#################################################################

import os
import requests
from bs4 import BeautifulSoup
import numpy as np
import random
import re
import csv    
import tqdm
import multiprocessing as mp
from  itertools import chain
from datetime import date

PATH_ICON= '/home/asus/MB/python/ICON/'

if not os.path.exists(PATH_ICON):
    os.makedirs(PATH_ICON)

#ESTE SCRIPT ES PARA DESCARGAR LOS DATOS DEL MODELO ICON 
 
URL_BUSQUEDA= 'https://opendata.dwd.de/weather/nwp/icon/grib/'


#INICIAMOS SESION REQUEST
session = requests.Session()
session.trust_env = False

#HACEMOS EL PRIMER REQUEST PARA VER LAS CARPETAS QUE HAY
GET_CAPA1= session.get(URL_BUSQUEDA)


print('\n ACCEDIENDO A LA PRIMERA CAPA DE SERVIDOR')
#PARSEANDO HTML Y SACAMOS CARPETAS 
user_soup = BeautifulSoup(GET_CAPA1.content, 'html.parser')
user_soup1=user_soup.find_all('a')

Lista_disponible= []
for i in range(len(user_soup1)):
    
    if user_soup1[i].text[:-1].isdigit():
        Lista_disponible.append(URL_BUSQUEDA + user_soup1[i].text)
        

print('\n ACCEDIENDO A LA SEGUNDA CAPA DE SERVIDOR')
#PROFUNDIZAMOS EN LAS CARPETAS
Lista_disponible1= []
for j in range(len(Lista_disponible)): 
    URL_MES= Lista_disponible[j]
    GET_CAPA2= session.get(URL_MES)
    user_soup = BeautifulSoup(GET_CAPA2.content, 'html.parser')
    user_soup1=user_soup.find_all('a')
    for i in range(len(user_soup1)):
        Lista_disponible1.append(URL_MES + user_soup1[i].text)


print('\n LISTANDO TODOS LOS GRIBS')
#SIGUIENTE CAPA DE CARPETAS        
Lista_disponible2= []
for j in range(len(Lista_disponible1)): 
    URL_DIA= Lista_disponible1[j]
    GET_CAPA3= session.get(URL_DIA)
    user_soup = BeautifulSoup(GET_CAPA3.content, 'html.parser')
    user_soup1=user_soup.find_all('a')
    for i in range(len(user_soup1)):
        Lista_disponible2.append(URL_DIA + user_soup1[i]['href'])



#TODOS LOS GRIBS DISPONIBLES 
GRIBS_DISPONIBLES= [item for item in Lista_disponible2 if '.grib2.bz2' in item]       


#SACAMOS GRIBS DE SINGLE LEVELS
SINGLE_L_GRIBS=[item for item in GRIBS_DISPONIBLES if 'icon_global_icosahedral_single-level_' in item]

#SACAMOS GRIBS EN NIVELES
LEVELS_GRIBS=[item for item in GRIBS_DISPONIBLES if 'icon_global_icosahedral_model-level_' in item]


#SACAMOS GRIBS EN DIFERENTES NIVELES DE PRESION 
PRESSURE_LEVELS= [item for item in GRIBS_DISPONIBLES if 'icon_global_icosahedral_pressure-level_' in item]

#DATOS DEL TERRENO
SOIL_LEVEL_GRIBS= [item for item in GRIBS_DISPONIBLES if 'icon_global_icosahedral_soil-level_' in item]


#DATOS INVARIANTES
TIME_INVARIANT_GRIB=   [item for item in GRIBS_DISPONIBLES if 'icon_global_icosahedral_time-invariant_' in item]



#COJEMOS LOS MISMO PERO PARA 48 HORAS
SINGLE_L_GRIBS_48= [item for item in SINGLE_L_GRIBS if int(item.split('/')[-1].replace('icon_global_icosahedral_single-level_', "").split("_")[1]) <= 48 ]

LEVELS_GRIBS_48= [item for item in LEVELS_GRIBS if int(item.split('/')[-1].replace('icon_global_icosahedral_model-level_', "").split("_")[1]) <= 48 ]

PRESSURE_GRIBS_48= [item for item in PRESSURE_LEVELS if int(item.split('/')[-1].replace('icon_global_icosahedral_pressure-level_', "").split("_")[1]) <= 48 ]


print('\n SEPARANDO GRIBS SINGLE LEVELS')

# COJEMOS LOS DATOS DE LA SIMULACION 00 
SINGLE_L_GRIBS_48_00= [item for item in SINGLE_L_GRIBS_48 if int(item.split('/')[-3])==0]

print('\n SEPARANDO GRIBS MODEL LEVELS')

LEVELS_GRIBS_48_00= [item for item in LEVELS_GRIBS_48 if int(item.split('/')[-3])==0]

print('\n SEPARANDO GRIBS PRESSURE LEVELS')

PRESSURE_GRIBS_48_00= [item for item in PRESSURE_GRIBS_48 if int(item.split('/')[-3])==0]


#FUNCION DOWNLOAD
#LA FUNCINO NECESITA QUE LE PASEMOS LA LISTA DE URLS
# Y EL PATH DE DESTINO EN UN UNICO INPUT PARA PODER USAR 
# IMAP Y PODER TENER LA BARRA DE PROGRESO

print('COMPROBANDO GRIBS YA DESCARGADOS')
PATH_ICON_DOWNLOAD= '/home/asus/MB/python/ICON'
Gribs_downloaded= []
for (dirpath, dirnames, filenames) in os.walk(PATH_ICON_DOWNLOAD):
     Gribs_downloaded.append(filenames)
     




def DOWNLOAD_ICON(URLs_PATH_ICON):
    
    #SACAMOS URLSY PATH DE DESTINO DE LA LISTA DE INPUT
    URL= URLs_PATH_ICON[0]
    PATH_DOWN_ICON= URLs_PATH_ICON[1]
    
    #DESCARGAMOS EL GRIB
    session = requests.Session()
    session.trust_env = False
    grib= session.get(str(URL)[:-1])  
    
    
    #CREAMOS EL PATH SI NO EXISTE 
    PATH_GRIB=  PATH_DOWN_ICON + '/' + str(URL)[:-1].split('/')[-1]    
    if not os.path.exists(PATH_DOWN_ICON):
        os.makedirs(PATH_DOWN_ICON)
    
    #GUARDAMOS EL ARVCHIVO
    with open( PATH_GRIB, 'wb') as f:
        #print('Guardando ' + PATH_GRIB)
        f.write(grib.content)



#HACEMOS MULTIPROCCESO CON BARRA DE PROGRESO
for Lista_var in [SINGLE_L_GRIBS_48_00, LEVELS_GRIBS_48_00, PRESSURE_GRIBS_48_00]:
    
    Lista_nueva= [item for item in Lista_var if not str(item.split('/')[-1])[:-1] in str(Gribs_downloaded)]
    
    
    MERGE_INPUT=[]
    for i in Lista_nueva:
        MERGE_INPUT.append([i, PATH_ICON + i.split('/')[-1].split('_')[3]])
    
    if MERGE_INPUT: 
        print('\n DESCARGANDO ' + MERGE_INPUT[0][1].split('/')[-1].upper())
        pool = mp.Pool(mp.cpu_count()-1)
        for _ in tqdm.tqdm(pool.imap_unordered(DOWNLOAD_ICON, MERGE_INPUT)  , total=len(Lista_nueva)):
            pass
        
        #MATAMOS SUBPROCESOS 
        pool.close()
        pool.terminate()
        pool.join()
    else:
        print('\n' + Lista_var[0].split('/')[-1].split('_')[3].upper() + ' YA DESCARGADO' )







