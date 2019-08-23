#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 19 15:11:31 2019

@author: oscar
"""

from ftplib import FTP
import os
import time

DESCARGAR_HRRR_V1= False
DESCARGAR_HRRR_V3 = False
DESCARGAR_NAM12 = True 
PATH_BASE= '/media/meteobit/Elements/'


if DESCARGAR_HRRR_V1:
        
    ftp = FTP('arlftp.arlhq.noaa.gov')   # connect to host, default port
    ftp.login()               # user anonymous, passwd anonymous@
    ftp.cwd(ftp.pwd() + 'pub/archives/hrrr.v1/')
    
    
    #PARA VER LOS ARCHIVOS CONTENIDOS Y SU INFORMACION 
    ftp.retrlines('LIST')
    
    #PARA OBTENER EL NOMBRE LOS ARCHIVOS EN UNA LISTA
    LISTA_HRR_DISPONIBLE=ftp.nlst()
    
    FILES= [item for item in LISTA_HRR_DISPONIBLE if '201901' in item if '00z' in item]
    
    PATH_BASE= '/media/oscar/14002CD4002CBF1C/'
    PATH_HRRR= PATH_BASE + '/HRRR/'
    if not os.path.exists(PATH_HRRR):
            os.makedirs(PATH_HRRR)
            
    for filename in FILES:
        
        print('DESCARGANDO ' + filename + ' EN ' + PATH_HRRR)
        handle = open(PATH_HRRR.rstrip("/") + "/" + filename.lstrip("/"), 'wb')
        ftp.retrbinary('RETR %s' % filename, handle.write)
    

if DESCARGAR_NAM12:
    try:
        ftp.close()
    except:
        print('LOG OUT')
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


    
            






'''
HRRR NUEVA VERSION

'''

from ftplib import FTP
import os

if DESCARGAR_HRRR_V3: 
    '''
    NO LO VOY A DESCARGAR PORQUE DEMOMENTO NO SOMOS CAPACES DE LEER ESTOS ARCHIVOS.... ESTA GENERANDO 3.5 GIGAS Y MEDIA
    CADA 6 HORAS DE SIMULAICON QUE AHORA MISMO NO NOS VALEN PARA NADA... NO SE IDENTIFICA COMO GRIB FORMAT. 
    
    '''
    ftp = FTP('arlftp.arlhq.noaa.gov')   # connect to host, default port
    ftp.login()               # user anonymous, passwd anonymous@
    ftp.cwd(ftp.pwd() + 'pub/archives/hrrr/')
    
    
    #PARA VER LOS ARCHIVOS CONTENIDOS Y SU INFORMACION 
    ftp.retrlines('LIST')
    
    #PARA OBTENER EL NOMBRE LOS ARCHIVOS EN UNA LISTA
    LISTA_HRR_DISPONIBLE=ftp.nlst()
    
    FILES= LISTA_HRR_DISPONIBLE
    
    
    PATH_HRRR= PATH_BASE + '/HRRR/'
    if not os.path.exists(PATH_HRRR):
            os.makedirs(PATH_HRRR)
            
    for filename in FILES:
        
        if os.path.isfile(PATH_HRRR + filename): 
            print('EL ARCHIVO ' + filename + ' YA HA SIDO DESCARGADO')
        else:
            print('DESCARGANDO ' + filename + ' EN ' + PATH_HRRR)
            ftp = FTP('arlftp.arlhq.noaa.gov', timeout= 600)   # connect to host, default port
            ftp.login()               # user anonymous, passwd anonymous@
            ftp.cwd(ftp.pwd() + 'pub/archives/hrrr/')
            try:
                handle = open(PATH_HRRR.rstrip("/") + "/" + filename.lstrip("/"), 'wb')
                ftp.retrbinary('RETR %s' % filename, handle.write)
            except:
                print('TIMEOUT INTENCIONADO Y SEGUIMOS DESCARGANDO... COSAS SIN  RESOLVER DE FTPs Y ARCHIVOS PESADOS')



        
        

