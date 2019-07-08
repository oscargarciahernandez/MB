# -*- coding: utf-8 -*-
"""
Created on Mon Jul  8 14:44:36 2019

@author: asus
"""

import os
import re
import shutil

#CAMBIAMOS EL PATH AL LUGAR DONDE SE ENCUENTRA EL SCRIPT.PY
# ESTO QUIERE DECIR QUE HAY QUE UBICAR ESTE SCRIPT DENTRO DE 
# LA CARPETA CON LOS GRIBS
abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

#LISTAMOS TODOS LOS ARCHIVOS DEN shell=TrueTRO DE LA CARPETA
folder_files= os.listdir()


#SELECIONAMOS LOS ARCHIVOS QUE SON GRIBS
regex= re.compile(r'[grib2]')
grib_list = [s for s in folder_files if regex.match(s)]


#SACAMOS TODAS LAS FECHAS DISPONIBLES HASTA EL MOMENTO
grib_date=[]
for s in grib_list: 
    grib_date.append(str(re.findall(r'gfs.0p25.(.*).f', s)[0]))


#ORDENAMOS LAS FECHAS
FECHAS= list(set(grib_date))
FECHAS.sort()



#COJEMOS LOS GRIBS DE LA FECHA SELECCIONADA  
FECHA_EJECUCION= FECHAS[0]

GRIB_FECHA_SELECTED= [k for k in grib_list if FECHA_EJECUCION in k]

GRIB_FECHA_SELECTED[0][9:19] + '.gfs.t00z.0p25.pgrb2f' + GRIB_FECHA_SELECTED[0][21:24]


'''
#COPIAMOS LOS GRIBS A LA CARPETA DE GRIBS DEL DOMINIO QUE QUEREMOS
#EN ESTE CASO SPAIN1 PORQUE AUN NO HEMOS DEFINIDO LOS DOMINIOS DE LOS PARQUES
os.chdir('/home/asus/MB/python')

print('Copiando GFS a carpet a/uems/runs/-----/grib')
for f in GRIB_FECHA_SELECTED:
    shutil.copy(f, '/usr1/uems/runs/spain1/grib')


#RENOMBRAMOS LOS GFS EN LA CARPETA DE DESTINO    
os.chdir( '/usr1/uems/runs/spain1/grib')
grib_files= os.listdir()
for i in range(len(grib_files)):    
    NOMBRE_NUEVO= grib_files[i][11:19] + '.gfs.t00z.0p25.pgrb2f' + grib_files[i][21:24]
    os.rename(grib_files[i], NOMBRE_NUEVO)
'''
#EJECUTAMOS WRF CON SUBPROCCES
import subprocess
command_modelo= './ems_autorun --date ' +  FECHA_EJECUCION[:-2] + ' --cycle 00:00:72  --noscour --dset gfsp25'
procces= subprocess.Popen(command_modelo.split(),stdout= subprocess.PIPE, cwd= '/usr1/uems/runs/spain1/')
output, error = procces.communicate()
print(str(output))
print(str(error))