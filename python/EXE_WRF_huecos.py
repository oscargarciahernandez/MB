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


NOMBRE_DOMINIO= 'spain1'
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


def COPY_NETCDF_FILES(DATE_EXE):
    os.chdir(PATH_OUTPUT)
    PATH_SAVE_OUTPUT_DATE= PATH_SAVE_OUTPUT + '/' +  DATE_EXE
    if not os.path.exists(PATH_SAVE_OUTPUT_DATE):
        os.makedirs(PATH_SAVE_OUTPUT_DATE)
    
    grib_files = os.listdir()
    for file_name in grib_files:
        full_file_name = os.path.join(os.getcwd(), file_name)
        if os.path.isfile(full_file_name):
            shutil.copy(full_file_name, PATH_SAVE_OUTPUT_DATE)
    
    print('\n NETCDFs guardados a la carpeta ' + PATH_SAVE_OUTPUT_DATE)


# IMPORTAMOS LA LISTA DE HUECOS... ESTA LISTA CONTIENE
# INFORMACION DE LA FECHA DE EJECUCION DEL MODELO
# HORA DE INICIALIZACION Y DURACION DEL LA SIMULACION
        
import re    
with open('/home/asus/MB/python/huecos_info.txt') as f:
    Lista_huecos= f.readlines()


for i in np.arange(8, len(Lista_huecos)):
    HORA_EXE= re.sub("[^0-9]", "", Lista_huecos[i].split(';')[-3])
    DURATION_EXE= re.sub("[^0-9]", "", Lista_huecos[i].split(';')[-2])
    DATE_EXE= re.sub("[^0-9]", "", Lista_huecos[i].split(';')[-1])

    command_modelo= './ems_autorun --date ' +  DATE_EXE + ' --cycle ' + HORA_EXE + ':00:' + DURATION_EXE + ' --dset gfsp25'
    print('\n Ejecutamos modelo para el dia ' + DATE_EXE + ' --- ' + str(datetime.datetime.now())[:-7])
    
    procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
    output, error = procces.communicate()
    print(str(output))
    print('\n Simulacion para el dia ' + DATE_EXE + '  finalizada --- ' + str(datetime.datetime.now())[:-7])
    
    
    print('\n Guardando Gribs')
    
    COPY_GRIB_FILES()
    
    print('\n Guardando NetCDFs')   
    COPY_NETCDF_FILES(DATE_EXE)
    
