#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 19 15:11:31 2019

@author: oscar
"""

from ftplib import FTP
import os

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



ftp = FTP('arlftp.arlhq.noaa.gov')   # connect to host, default port
ftp.login()               # user anonymous, passwd anonymous@
ftp.cwd(ftp.pwd() + 'pub/archives/nam12/')

#PARA VER LOS ARCHIVOS CONTENIDOS Y SU INFORMACION 
ftp.retrlines('LIST')

#PARA OBT1ENER EL NOMBRE LOS ARCHIVOS EN UNA LISTA
LISTA_NAM_DISPONIBLE=ftp.nlst()

FILES= [item for item in LISTA_NAM_DISPONIBLE if '201901' in item]

PATH_BASE= '/media/oscar/14002CD4002CBF1C/'
PATH_NAM12= PATH_BASE + '/NAM12/'
if not os.path.exists(PATH_NAM12):
        os.makedirs(PATH_NAM12)
        
for filename in FILES:
    
    print('DESCARGANDO ' + filename + ' EN ' + PATH_NAM12)
    handle = open(PATH_NAM12.rstrip("/") + "/" + filename.lstrip("/"), 'wb')
    ftp.retrbinary('RETR %s' % filename, handle.write)


















