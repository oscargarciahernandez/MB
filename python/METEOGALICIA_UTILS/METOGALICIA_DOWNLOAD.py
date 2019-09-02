#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 28 13:08:31 2019

@author: meteobit
"""
import os
import requests
import datetime
import numpy as np

'''
SCRIPT PARA DESACARGAR METEOGALICIA

'''

def DOWNLOAD_GFS(URL):
    session = requests.Session()
    session.trust_env = False
    grib= session.get(str(URL))  
    PATH_GRIB=  PATH_METEOGALICIA + str(URL).split('/')[-1]
    with open( PATH_GRIB, 'wb') as f:
        #print('Guardando ' + PATH_GRIB)
        f.write(grib.content)






PATH_METEOGALICIA= '/home/meteobit/METEOGALICIA/'
if not os.path.exists(PATH_METEOGALICIA):
    os.makedirs(PATH_METEOGALICIA)


WRF_DOWNLOADED= os.listdir(PATH_METEOGALICIA)

fechas2018_2019= []
for year in [2018,2019]:
    for month in np.arange(1,13):
        for day in np.arange(1,32):
            try:
                fechas2018_2019.append(datetime.datetime(year, month, day).strftime("%Y%m%d"))
            except:
                pass
            
for fecha in fechas2018_2019:
    for hora_sim in [0000, 1200]:
        URL= 'http://mandeo.meteogalicia.es/thredds/fileServer/modelos/WRF_HIST/d02/' + fecha[:4] + '/' + fecha[4:6] +'/wrf_arw_det_history_d02_'  + fecha+ '_' + "{0:0=4d}".format(hora_sim) + '.nc4'


        if URL.split('/')[-1] in WRF_DOWNLOADED:
            print(URL.split('/')[-1]  + ' YA HA SIDO DESCARGADO')
        else: 
            DOWNLOAD_GFS(URL)


'''

POR SI EN UN FUTURO QUEREMOS HACER LA DESCARGA EN MULTIPROCESO

import tqdm
import multiprocessing as mp

pool = mp.Pool(mp.cpu_count()-2)
for _ in tqdm.tqdm(pool.imap_unordered(DOWNLOAD_GFS, Lista_nueva)  , total=len(Lista_nueva)):
    pass
#MATAMOS SUBPROCESOS 
pool.close()
pool.terminate()
pool.join()
'''