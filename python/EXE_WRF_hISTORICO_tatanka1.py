# -*- coding: utf-8 -*-
"""
Created on Mon Jul  8 14:44:36 2019

@author: asus
"""

import os
import shutil
import numpy as np
import subprocess
import datetime


NOMBRE_DOMINIO= 'tatanka'
PATH_MODELO= '/usr1/uems/runs/'+ NOMBRE_DOMINIO 
PATH_GRIBS=  PATH_MODELO + '/grib'
PATH_OUTPUT= PATH_MODELO + '/wrfprd'

PATH_SAVE= '/usr1/uems/runs/ARCHIVOS_GUARDADOS'
PATH_SAVE_GRIBS= PATH_SAVE + '/grib'
PATH_SAVE_OUTPUT= PATH_SAVE + '/netcdf/' + NOMBRE_DOMINIO

GRIB_SOURCE= '/home/meteobit/GRIBS_201801/'



def COPY_GRIB_FILES():
    os.chdir(PATH_GRIBS)
    if not os.path.exists(PATH_SAVE_GRIBS):
        os.makedirs(PATH_SAVE_GRIBS)
    
    grib_files = os.listdir()
    for file_name in grib_files:
        full_file_name = os.path.join(os.getcwd(), file_name)
        if os.path.isfile(full_file_name):
            shutil.copy(full_file_name, PATH_SAVE_GRIBS)
    
    print('\n GRIBs guardados a la carpeta ' + PATH_SAVE_GRIBS)



def COPY_FROM_X_TO_GRIB(FECHA_EJECUCION):
    os.chdir(GRIB_SOURCE)
    grib_files = os.listdir()
   
    GRIBS_FECHA_EXE= [item for item in grib_files if str(item.split('.')[0][:6])==FECHA_EJECUCION[2:]]
    for file_name in GRIBS_FECHA_EXE:
        full_file_name = os.path.join(os.getcwd(), file_name)
        if os.path.isfile(full_file_name): 
            print('\n COPIANDO ' + file_name + ' a ', PATH_GRIBS)
            shutil.copy(full_file_name, PATH_GRIBS)
    




def COPY_NETCDF_FILES(FECHA_EJECUCION):
    
    os.chdir(PATH_OUTPUT)
    if not os.path.exists(PATH_SAVE_OUTPUT):
        os.makedirs(PATH_SAVE_OUTPUT)
        
        
    PATH_SAVE_EXE_DATE= PATH_SAVE_OUTPUT + '/' + FECHA_EJECUCION
    
    if not os.path.exists(PATH_SAVE_EXE_DATE):
        os.makedirs(PATH_SAVE_EXE_DATE)
    
    grib_files = os.listdir()
    if grib_files:
        for file_name in grib_files:
            full_file_name = os.path.join(os.getcwd(), file_name)
            if os.path.isfile(full_file_name):
                shutil.copy(full_file_name, PATH_SAVE_EXE_DATE)
        
        print('\n NETCDFs guardados a la carpeta ' + PATH_SAVE_EXE_DATE)
    else:
        print('\n Carpeta de WRFPRD vacia, ha fallado la ejecucion del modelo')


# PONEMOS LOS DÍAS QUE QUEREMOS DESDE 20190101 HASTA 20190631
dias_2018= []
for mes in np.arange(1,13):
    for dia in np.arange(1,32):
        try:
            dias_2018.append(datetime.datetime(2018, mes, dia).strftime("%Y%m%d"))
        except:
            pass


        

#EJECUTAMOS EL MODELO PARA TODOS LOS DIAS UN PERIODO DE 24 HORAS
# CORREMOS EL MODELO CON --noscour ESO ES IMPORTANTE PORQUE ASI NO SE BORRAN LOS GRIBS 
# Y SE PUEDEN EMPLEAR PARA CORRER OTROS DOMINIOS
#EJECUTAMOS WRF CON SUBPROCCES

if not os.path.exists(PATH_SAVE):
    os.makedirs(PATH_SAVE)

for FECHA_EJECUCION in dias_2018[1:]: 
    
    #COPIAMOS LOS GRIBS A LA CARPETA DE GRIBS
    SIMULACIONES_HECHAS= os.listdir(PATH_SAVE_OUTPUT)

    if FECHA_EJECUCION in SIMULACIONES_HECHAS:
        print('\n SIMULACION ' + FECHA_EJECUCION  + ' YA HECHA ')
        pass
    
    else:
        COPY_FROM_X_TO_GRIB(FECHA_EJECUCION)
        
        
        #CREAMOS EL COMANDO ANADIENDO LA FECHA DE EJECUCION 
        command_modelo= './ems_prep --date ' +  FECHA_EJECUCION + ' --cycle 00:00:48 --dset gfsp25 --local --domain 1,2,3 && ./ems_run --domain 1,2,3'
        print('\n Ejecutamos modelo para el dia ' + FECHA_EJECUCION + ' --- ' + str(datetime.datetime.now())[:-7])
        
        
        #EJECUTAMOS EL MODELO 
        procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
        output, error = procces.communicate()
        print(output)
        print('\n Simulacion para el dia ' + FECHA_EJECUCION + '  finalizada --- ' + str(datetime.datetime.now())[:-7])
        
        
        #COPIAMOS LA SALIDA DEL MODELO A CARPETA EXTERIOR
        COPY_NETCDF_FILES(FECHA_EJECUCION)
    
