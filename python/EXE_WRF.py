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


NOMBRE_DOMINIO= 'eibar2'
PATH_MODELO= '/usr1/uems/runs/'+ NOMBRE_DOMINIO 
PATH_GRIBS=  PATH_MODELO + '/grib'
PATH_OUTPUT= PATH_MODELO + '/wrfprd'

PATH_SAVE= '/usr1/uems/runs/ARCHIVOS_GUARDADOS'
PATH_SAVE_GRIBS= PATH_SAVE + '/grib'
PATH_SAVE_OUTPUT= PATH_SAVE + '/netcdf/' + NOMBRE_DOMINIO



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


def COPY_NETCDF_FILES():
    os.chdir(PATH_OUTPUT)
    if not os.path.exists(PATH_SAVE_GRIBS):
        os.makedirs(PATH_SAVE_OUTPUT)
    
    grib_files = os.listdir()
    for file_name in grib_files:
        full_file_name = os.path.join(os.getcwd(), file_name)
        if os.path.isfile(full_file_name):
            shutil.copy(full_file_name, PATH_SAVE_OUTPUT)
    
    print('\n NETCDFs guardados a la carpeta ' + PATH_SAVE_GRIBS)


# PONEMOS LOS DÍAS QUE QUEREMOS DESDE 20190101 HASTA 20190631
dias_2019= []
for mes in np.arange(1,7):
    for dia in np.arange(1,32):
        dias_2019.append('2019'+format(mes, '02d')+format(dia, '02d'))
        


#EJECUTAMOS EL MODELO PARA TODOS LOS DIAS UN PERIODO DE 24 HORAS
# CORREMOS EL MODELO CON --noscour ESO ES IMPORTANTE PORQUE ASI NO SE BORRAN LOS GRIBS 
# Y SE PUEDEN EMPLEAR PARA CORRER OTROS DOMINIOS
#EJECUTAMOS WRF CON SUBPROCCES
        
for i in dias_2019: 
    FECHA_EJECUCION= dias_2019[0]
    command_modelo= './ems_autorun --date ' +  FECHA_EJECUCION + ' --cycle 00:00:48 --dset gfsp25'
    print('\n Ejecutamos modelo para el dia ' + FECHA_EJECUCION + ' --- ' + str(datetime.datetime.now())[:-7])
    
    procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
    output, error = procces.communicate()
    print('\n Simulacion para el dia ' + FECHA_EJECUCION + '  finalizada --- ' + str(datetime.datetime.now())[:-7])
    
    COPY_GRIB_FILES()
    COPY_NETCDF_FILES()

