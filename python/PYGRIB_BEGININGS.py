#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 23 13:17:24 2019

@author: meteobit
"""

import pygrib
import os
import pandas as pd
pd.set_option('display.max_columns', 15)
pd.set_option('display.max_rows', 30)


HRRR_PATH= '/media/meteobit/Elements/HRRR_from_UofU/'
HRRR_FILES= os.listdir(HRRR_PATH)

HRRR_GRIB= pygrib.open(HRRR_PATH + HRRR_FILES[0])


VARIABLE_NAMES=[]
k=1
HRRR_GRIB.seek(0)
for i in HRRR_GRIB:
    VARIABLE_NAMES.append(str(i).split(':'))
    k=k+1

colnames=  ['index','NAME', 'UNITS', 'PROJ', 'TYPE_VAR', 'LEVEL_Pa', 'TIMESTAMP', 'DATE']   
TABLA_VAR= pd.DataFrame(VARIABLE_NAMES, columns= colnames)


'''
PARA VER TODOS LOS VALORES UNICOS DE CADA COLUMNA
'''
for i in list(TABLA_VAR.columns):
    print(set(list(TABLA_VAR[i])))

TABLA_WIND = TABLA_VAR[TABLA_VAR.NAME.isin(set([item for item in list(TABLA_VAR['NAME']) if 'wind' in item]))]

PATH_TO_HRRR= os.getcwd() + '/HRRR/'
if not os.path.isdir(PATH_TO_HRRR):
    os.mkdir(PATH_TO_HRRR)
    
TABLA_WIND.to_csv(PATH_TO_HRRR + 'TABLA_WIND.csv', index = False, sep= ';')
TABLA_VAR.to_csv(PATH_TO_HRRR + 'TABLA_VARIABLES.csv', index = False, sep= ';')


'''
TRAS SACAR LOS CSVs  Y VER LAS VARIABLES DECIDIMOS QUE SOLO NOS INTERESA SACAR LA VELOCIDAD DEL VIENTO 
HASTA LOS 9000hpa



'''


SELECTED_VARIABLES = TABLA_WIND[pd.to_numeric(TABLA_WIND['index']) > 470]



'''
ESTO DE ACONTINUACION ESTA PENSADO PARA HACER UN BUCLE CAMINANDO 
POR ILOC[X] DONDE X ES EL NUMERO DE FILAS DE SELECTED
VARIABLES... LO SUYO SERÁ EXTRAER UNICAMENTE LOS 50 PUNTOS 
MAS CERCANOS Y HACER CSVs COMO YA HICIMOS CON TAMAULIPAS


'''
VARIABLE = HRRR_GRIB.select(name= SELECTED_VARIABLES['NAME'].iloc[0])


VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-3]==SELECTED_VARIABLES['LEVEL_Pa'].iloc[0] ]



'''
SACAMOS ARRAY DE VALORES, LONGITUD, LATITUD Y DATE
AUNQUE HAY MUCHAS MAS VARIABLES CONTENIDAS EN EL FICHERO.
TODAS ESTAS VARIABLES SE PUEDEN VER EJECUTANDO X.keys()

'''
VARIABLE_VALUES = VARIABLE_ITEM[0].values

LATS , LONS = VARIABLE_ITEM[0].latlons()

DATE = VARIABLE_ITEM[0].validDate

VARIABLE_ITEM[0].keys()




