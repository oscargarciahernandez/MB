#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 27 16:46:02 2019

@author: oscar
"""
import datetime
import requests

PATH_CALMET = '/home/oscar/MB/Data/Parques/Salteras/CALMET/'

def DOWNLOAD_CALMET(URL):
    session = requests.Session()
    session.trust_env = False
    grib= session.get(str(URL))  
    PATH_NC=  PATH_CALMET + str(URL).split('/')[-1]
    with open( PATH_NC, 'wb') as f:
        #print('Guardando ' + PATH_GRIB)
        f.write(grib.content)



FECHA_INICIAL = datetime.datetime(2019,5,1)
HOY = datetime.datetime.today()

SUMDAY = datetime.timedelta(days=1)

while True:
    URL_CALMET='http://troposfera.es/datos/sevilla/calmet_'+FECHA_INICIAL.strftime('%Y%m%d')+'.nc'   
    DOWNLOAD_CALMET(URL_CALMET)
    FECHA_INICIAL = FECHA_INICIAL + SUMDAY
    
    if FECHA_INICIAL > HOY:
        break

