#!/usr/bin/env python
#################################################################

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

BUSCAR_GFS_ONLINE= False



if BUSCAR_GFS_ONLINE: 
    URL_BUSQUEDA= 'http://ems3.comet.ucar.edu/data/grib/gfsp25/'
    
    #INICIAMOS SESION REQUEST
    session = requests.Session()
    session.trust_env = False
    GET_CAPA1= session.get(URL_BUSQUEDA)
    
    
    #PARSEANDO HTML
    user_soup = BeautifulSoup(GET_CAPA1.content, 'html.parser')
    user_soup1=user_soup.find_all('a')
    
    Lista_disponible= []
    for i in range(len(user_soup1)):
        
        if user_soup1[i].text[:-1].isdigit():
            Lista_disponible.append(URL_BUSQUEDA + user_soup1[i].text)
            
    
    
    Lista_disponible1= []
    for j in range(len(Lista_disponible)): 
        URL_MES= Lista_disponible[j]
        GET_CAPA2= session.get(URL_MES)
        user_soup = BeautifulSoup(GET_CAPA2.content, 'html.parser')
        user_soup1=user_soup.find_all('a')
        for i in range(len(user_soup1)):
            
            if user_soup1[i].text[:-1].isdigit():
                Lista_disponible1.append(URL_MES + user_soup1[i].text)
                
    Lista_disponible2= []
    for j in range(len(Lista_disponible1)): 
        URL_DIA= Lista_disponible1[j] + 'grib.t00z/'
        GET_CAPA3= session.get(URL_DIA)
        user_soup = BeautifulSoup(GET_CAPA3.content, 'html.parser')
        user_soup1=user_soup.find_all('a')
        for i in range(len(user_soup1)):
            if user_soup1[i].text[:5].isdigit():
                Lista_disponible2.append(URL_DIA + user_soup1[i].text)
                
                
    #ESCOJEMOS SOLO LOS DATOS DE 2018 Y SOLO HASTA 48 HORAS DE FORECAST
    '''
    Lista_2018= []
    for i in range(len(Lista_disponible2)): 
        if not int(Lista_disponible2[i].split('/')[7][:4]) < 2018:
            if int(Lista_disponible2[i][-3:])<49:
                Lista_2018.append(Lista_disponible2[i])
                
    LISTA_JUNIO2019_ALL= [item for item in Lista_disponible2 if '201906' in item]
    LISTA_JUNIO2019= [item for item in LISTA_JUNIO2019_ALL if int(item[-3:])<49]
    with open('URLS_GFS025_JUNIO_2019.txt', 'w') as f:
        for item in LISTA_JUNIO2019:
            f.write("%s\n" % item)
    '''        


    Lista_2019= []
    for i in range(len(Lista_disponible2)): 
        if not int(Lista_disponible2[i].split('/')[7][:4]) == 2019:
            if int(Lista_disponible2[i][-3:])<49:
                Lista_2019.append(Lista_disponible2[i])
    
    with open('URLS_GFS025_2019.txt', 'w') as f:
        for item in Lista_2019:
            f.write("%s\n" % item)







PATH_ELEMENTS= '/media/meteobit/Elements/GRIBS_025_2019/'
if not os.path.exists(PATH_ELEMENTS):
    os.makedirs(PATH_ELEMENTS)
    

Gribs_downloaded= os.listdir(PATH_ELEMENTS) + os.listdir('/media/meteobit/Elements/GRIB025_JUNIO')



import pandas as pd   

Lista_txt= pd.read_csv('URLS_GFS025_2019.txt', header= None)


        
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

