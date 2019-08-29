#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 29 10:52:59 2019

@author: meteobit
"""
from ftplib import FTP
import os

DESCARGAR_NAM12 = True 
PATH_BASE= '/media/meteobit/Elements/'

if DESCARGAR_NAM12:
    try:
        ftp.close()
    except:
        print('LOG OUT POR SI ACASO')
    
    ftp = FTP('nomads.ncdc.noaa.gov')   # connect to host, default port

    ftp.login() # user anonymous, passwd anonymous@
    ftp.cwd(ftp.pwd() + '/NAM/Grid218/')
    
    PATH_ORIGEN= ftp.pwd()
    #PARA VER LOS ARCHIVOS CONTENIDOS Y SU INFORMACION 
    #ftp.retrlines('LIST')
    
    #PARA OBT1ENER EL NOMBRE LOS ARCHIVOS EN UNA LISTA
    LISTA_NAM_DISPONIBLE=ftp.nlst()
    
    FILES_MONTH= [item for item in LISTA_NAM_DISPONIBLE if '2019' in item]
    
    for month in FILES_MONTH:
        ftp.cwd(PATH_ORIGEN + '/' +month)
        
        PATH_MES= ftp.pwd()
        
        LISTA_NAM_DISPONIBLE=ftp.nlst()
        FILES= [item for item in LISTA_NAM_DISPONIBLE if '2019' in item]
    
        for Dias_junio in FILES:
            ftp.cwd(PATH_MES + '/' + Dias_junio)
        
            LISTA_NAM_DISPONIBLE=ftp.nlst()
            FILES_grb= [item for item in LISTA_NAM_DISPONIBLE if '.grb2' in item]
            FILES_grb = [item for item in FILES_grb if  int(item.split('_')[-2])==0]
            FILES_grb = [item for item in FILES_grb if int(item.split('.')[-2][-3:])<24]    
            
            
            PATH_NAM12= PATH_BASE + '/NAM12/'
            if not os.path.exists(PATH_NAM12):
                    os.makedirs(PATH_NAM12)
                    
            for filename in FILES_grb:
                
                if os.path.isfile(PATH_NAM12 + filename): 
                    print('EL ARCHIVO ' + filename + ' YA HA SIDO DESCARGADO')
                else:
                    
                    print('DESCARGANDO ' + filename + ' EN ' + PATH_NAM12)
                    
                    try:
                        handle = open(PATH_NAM12.rstrip("/") + "/" + filename.lstrip("/"), 'wb')
                        ftp.retrbinary('RETR %s' % filename, handle.write)
                    except:
                        print('ERROR EN LA DESCARGA DESCONOCIDO... IGUAL TIENE EL MISMO PROBLEMA QUE EL HRRR')
        
        print('TODOS LOS DIAS DESCARGADOS')

