#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 23 13:17:24 2019

@author: meteobit
"""

import pygrib
import os
import pandas as pd
from math import sin, cos, sqrt, atan2, radians
import numpy as np
import re

pd.set_option('display.max_columns', 15)
pd.set_option('display.max_rows', 30)


'''
################################################################################
################################################################################
                            funciones
################################################################################
################################################################################

'''
def EXTRACT_VARIABLES_FROM_GRIB(NAM12_FILE_NAME, CUSTOM_WORD):
    '''
    FUNCION QUE NOS DEVUELVE LA TABLA DE VARIABLES CONTENIDA EN EL GRIB... Y LA GUARDA A UN CSV
    ADEMAS PODEMOS DECIRLE QUE NOS DE UNA TABLADE VARIABLES PERSONALIZADA... INTRODUCIONDO UNA 
    PALABRA QUE BUSCARA ENTRE LOS NOMBRES DE LAS VARIABLES
    '''
    NAM12_GRIB= pygrib.open(NAM12_FILE_NAME)
    
    VARIABLE_NAMES=[]
    k=1
    NAM12_GRIB.seek(0)
    for i in NAM12_GRIB:
        VARIABLE_NAMES.append(str(i).split(':'))
        k=k+1
    
    colnames=  ['index','NAME', 'UNITS', 'PROJ', 'TYPE_VAR', 'LEVEL_Pa', 'TIMESTAMP', 'DATE']   
    TABLA_VAR= pd.DataFrame(VARIABLE_NAMES, columns= colnames)
    
    
    '''
    PARA VER TODOS LOS VALORES UNICOS DE CADA COLUMNA
    '''
    TABLA_CUSTOM = TABLA_VAR[TABLA_VAR.NAME.isin(set([item for item in list(TABLA_VAR['NAME']) if CUSTOM_WORD in item.lower()]))]
    
    PATH_SAVE= os.getcwd() + '/CSV_VARIABLES/' +NAM12_GRIB.name.split('/')[-1].replace('.','') + '/'
    if not os.path.isdir(PATH_SAVE):
        os.makedirs(PATH_SAVE, exist_ok=True)
    
    print('TABLA DE VARIABLES GUARDADA EN ' + PATH_SAVE + 'TABLA_VARIABLES.csv')
    print('TABLA DE VARIABLES DE VIENTO GUARDADA EN ' + PATH_SAVE + 'TABLA_WIND.csv')
    
    TABLA_CUSTOM.to_csv(PATH_SAVE + 'TABLA_'+ CUSTOM_WORD+'.csv', index = False, sep= ';')
    TABLA_VAR.to_csv(PATH_SAVE + 'TABLA_VARIABLES.csv', index = False, sep= ';')
    
    return TABLA_VAR


def OBTAIN_50_NEAREST_POINTS(LONG_NP, LAT_NP, LON, LAT):
    '''
    FUNCION PARA OBTENER LOS 50 PUNTOS MAS CERCANOS A LA ZONA A ESTUDIAR...
    SACAR INFORMACION DE TODOS LOS PUNTOS ES MUY EXIGENTE PARA ESTE SCRIPT
    '''    
    if len(LONG_NP.shape)==2:
        TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
        TABLA_LONLAT['LON']= LONG_NP[:, :].ravel()
        TABLA_LONLAT['LAT']= LAT_NP[:, :].ravel()
    else:
        TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
        TABLA_LONLAT['LON']= LONG_NP
        TABLA_LONLAT['LAT']= LAT_NP
    '''
    METODO MAS CLARO PERO MAS LENTO... EL METODO ANTERIOR ESTA BIEN¡
    
    
    TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
    k=0
    for i in range(LONG_NP.shape[1]):
        for j in range(LAT_NP.shape[0]):
            TABLA_LONLAT.loc[k]= [LONG_NP[j,i], LAT_NP[j,i]]
            k=k+1
            
    '''
    # approximate radius of earth in km
    R = 6373.0
    
    DISTANCIA= []
    for i in range(len(TABLA_LONLAT['LAT'])):
        lat1 = radians(TABLA_LONLAT['LAT'][i])
        lon1 = radians(TABLA_LONLAT['LON'][i])
        lat2 = radians(LAT)
        lon2 = radians(LON)
        
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        
        a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
        c = 2 * atan2(sqrt(a), sqrt(1 - a))
        
        DISTANCIA.append(R * c)
    
    
    
    TABLA_LONLAT.insert(column= 'DIST',   loc= 0,  value= DISTANCIA)
    
    TABLA_ORD_DIST= TABLA_LONLAT.sort_values(by='DIST')
    
    
    return TABLA_ORD_DIST[0:50]


def EXTRACT_INFO_NAM12_TO_CSV(NAM12_FILE_NAME,
                              SELECTED_VARIABLES,
                              NEAREST_POINTS,
                              NAME,
                              SEGUNDA_LOCALIZACION,
                              NEAREST_POINTS2, 
                              NAME2):
    '''
    FUNCION PARA EXTRAER LA INFORMACION PARA LAS VARIABLES REQUERIDAS
    ESTAS VARIABLES SE PUEDEN SACAR EMPLEANDO LA FUNCION EXTRACT_VARIABLES_FROM_GRIB.
    CUT DATA ES UN BOOLEANO. IF TRUE SE CORTA LA INFORMACION DE ACUERDO A NEAREST POINTS    
    '''
    
    
    NAM12_GRIB= pygrib.open(NAM12_FILE_NAME)
    
    NOMBRE_ARCHIVO = NAM12_FILE_NAME.replace(NAM12_FILE_NAME.split('/')[-1], '') + NAM12_FILE_NAME.split('/')[-1] +'_' + NAME+ '_CONVERTED.csv'
    NOMBRE_ARCHIVO2 = NAM12_FILE_NAME.replace(NAM12_FILE_NAME.split('/')[-1], '') + NAM12_FILE_NAME.split('/')[-1] +'_' + NAME2+ '_CONVERTED.csv'

    
    TABLA_TOTAL= pd.DataFrame()
    TABLA_TOTAL2= pd.DataFrame()
    for i in range(SELECTED_VARIABLES.shape[0]):
        
        VAR_LEVEL= SELECTED_VARIABLES['LEVEL_Pa'].iloc[i]
        VAR_NAME= SELECTED_VARIABLES['NAME'].iloc[i]
                
        EXE = 1
        if os.path.isfile(NOMBRE_ARCHIVO):
            print(NOMBRE_ARCHIVO +  ' YA EXISTE')
            EXE=0
            if os.stat(NOMBRE_ARCHIVO).st_size == 0:
                print(NOMBRE_ARCHIVO +  ' PERO ESTA VACIO')
                EXE=1
        if EXE==1:       
            try:
                
                VARIABLE = NAM12_GRIB.select(name= VAR_NAME)
                VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-3]==VAR_LEVEL]
            
            
            
                '''
                SACAMOS ARRAY DE VALORES, LONGITUD, LATITUD Y DATE
                AUNQUE HAY MUCHAS MAS VARIABLES CONTENIDAS EN EL FICHERO.
                TODAS ESTAS VARIABLES SE PUEDEN VER EJECUTANDO X.keys()
                
                '''
                VARIABLE_VALUES = VARIABLE_ITEM[0].values
                LATS , LONS = VARIABLE_ITEM[0].latlons()
                DATE = VARIABLE_ITEM[0].validDate            
              
                #print('CREANDO TABLA')  
                dfObj = pd.DataFrame()
                dfObj['VALUES']= VARIABLE_VALUES.ravel()
                dfObj['LON']= LONS.ravel()
                dfObj['LAT']= LATS.ravel()
                dfObj['DATE'] = DATE

                TABLA_CUT = dfObj[(dfObj['LON'].isin(NEAREST_POINTS['LON'])) & (dfObj['LAT'].isin(NEAREST_POINTS['LAT']))]               
                TABLA_CUT['VAR_NAME']= str(VARIABLE_ITEM[0]).split(':')[1]
                TABLA_CUT['LEVEL']= str(VARIABLE_ITEM[0]).split(':')[-3]
                TABLA_CUT['FCST_TIME']= str(VARIABLE_ITEM[0]).split(':')[-2]
                TABLA_TOTAL = TABLA_TOTAL.append(TABLA_CUT)

                if SEGUNDA_LOCALIZACION:
                    TABLA_CUT2 = dfObj[(dfObj['LON'].isin(NEAREST_POINTS2['LON'])) & (dfObj['LAT'].isin(NEAREST_POINTS2['LAT']))]
                    TABLA_CUT2['VAR_NAME']= str(VARIABLE_ITEM[0]).split(':')[1]
                    TABLA_CUT2['LEVEL']= str(VARIABLE_ITEM[0]).split(':')[-3]
                    TABLA_CUT2['FCST_TIME']= str(VARIABLE_ITEM[0]).split(':')[-2]
                    TABLA_TOTAL2 = TABLA_TOTAL.append(TABLA_CUT2)  
                    
              
 
            except:
                print(NAM12_FILE_NAME + ' NO ENCONTRADO ' + VAR_NAME)
                
    if not TABLA_TOTAL.empty:
 
        print('GUARDANDO ' + NOMBRE_ARCHIVO)
    
        TABLA_TOTAL.to_csv(NOMBRE_ARCHIVO)
    if not TABLA_TOTAL2.empty:
        print('GUARDANDO ' + NOMBRE_ARCHIVO2)
        TABLA_TOTAL2.to_csv(NOMBRE_ARCHIVO2)

    return(NAM12_FILE_NAME)



'''
################################################################################
################################################################################
                                CREAR ARCHIVOS TOTALES
                                        Y
                                CORTAR CSV PARA CADA LOCALIZACION
Este cacho de codigo coje los gribs del Nam12 y extrae las variables requeridas
en este caso el viento(por componentes u y v ) y las convierte a csv para 
todas las localizaciones ofrecidas en el grib. 

La funcion principal tambien esta preparada para extraer los datos a csv
pero cortados para localizaciones especificas. 

Por si acaso, los nams con los que se ha trabajado aqui, se ha sacado de un 
ftp del nomads. No se si nams provinientes de otras fuentes tenga otro 
contenido 

nomads.ncdc.noaa.gov//NAM/Grid218/
################################################################################
################################################################################
'''

   
NAM12_PATH= '/media/meteobit/Elements/NAM12/'

NAM12_FILES= os.listdir(NAM12_PATH)


'''
SEPARAMOS NAM12 GRIBS Y CSVs

'''
NAM12_csv = [NAM12_PATH + item for item in NAM12_FILES if item.endswith('.csv')]
NAM12_grb2 = [NAM12_PATH + item for item in NAM12_FILES if item.endswith('.grb2')]


'''
SACAMOS TABLA DE VARIABLES... PARA ELLO TENEMOS QUE INTRODUCIR
UNA PALABRA... POR LA QUE BUSCAR LA VARIABLE (CUSTOM WORD). 
SELECCIONAMOS UN ARCHIVO CUALQUIERA DE NAM
''' 
CUSTOM_WORD =    'wind' 
NAM12_FILE_NAME = NAM12_grb2[0]

'''
CREAMOS LAS TABLAS DE VARIABLES. LO SUYO ES IR A VER LAS VARIABLES 
CONTENIDAS AL CSV. ESTA FUNCION DEVUELVE TODAS LAS VARIABLES
LAS GUARDAMOS EN TABLA DE VARIABLES Y LUEGO FILTRAMOS POR 
NIVEL Y NOMBRE DE VARIABLE PARA SACAR LAS QUE QUEREMOS
'''
TABLA_VARIABLES = EXTRACT_VARIABLES_FROM_GRIB(NAM12_FILE_NAME, CUSTOM_WORD)


#LA MANERA DE FILTRAR LAS VARIABLES HAY QUE AFINARLO
TABLA_WIND= TABLA_VARIABLES[TABLA_VARIABLES.NAME.isin(set([item for item in list(TABLA_VARIABLES['NAME']) if 'wind' in item.lower()]))]
TABLA_WIND= TABLA_WIND[TABLA_WIND['LEVEL_Pa'].isin(['level 80 m', 'level 10 m'])]



'''
SELECCIONAMOS UNA VARIABLE... LA PRIMERA
PERO PUEDE SER CUALQUIERA PARA SACAR LONGITUD Y 
LATITUD Y PODER SACAR LAS TABLAS DE 50 LOCALIZACIONES MAS
CERCANAS
'''
NAM12_GRIB= pygrib.open(NAM12_FILE_NAME)


'''
SELECCIONAMOS CUALQUIER VARIABLE...
DE ESTE MODO, PRIMERO SACAMOS POR NOMBRE Y TYPO 
Y LUEGO POR LEVEL... NO PUEDO EXTRAER DIRECTAMENTE 
AÑADIENDO EL LEVEL DA ERROR 

ValueError: no matches found

'''
VARIABLE = NAM12_GRIB.select(name= TABLA_WIND['NAME'].iloc[1],
                             typeOfLevel=TABLA_WIND['TYPE_VAR'].iloc[1])

VARIABLE = [item for item in VARIABLE if str(item).split(':')[-3]==TABLA_WIND['LEVEL_Pa'].iloc[1]][0]

LATS , LONS = VARIABLE.latlons()


LON_TATANKA= -98.955699 	
LAT_TATANKA= 45.95685
if  os.path.isfile(NAM12_PATH +  'NEAREST_POINTS_TATANKA.csv'):
    NEAREST_POINTS_TATANKA = pd.read_csv(NAM12_PATH +  'NEAREST_POINTS_TATANKA.csv')
    
else:
    NEAREST_POINTS_TATANKA = OBTAIN_50_NEAREST_POINTS(LONS, LATS, LON_TATANKA, LAT_TATANKA)
    NEAREST_POINTS_TATANKA.to_csv(NAM12_PATH + 'NEAREST_POINTS_TATANKA.csv')


LON_TAMAULIPAS= -98.196080 	
LAT_TAMAULIPAS= 25.78788
if  os.path.isfile(NAM12_PATH +  'NEAREST_POINTS_TAMAULIPAS.csv'):
    NEAREST_POINTS_TAMAULIPAS = pd.read_csv(NAM12_PATH +  'NEAREST_POINTS_TAMAULIPAS.csv')
else:
    NEAREST_POINTS_TAMAULIPAS = OBTAIN_50_NEAREST_POINTS(LONS, LATS, LON_TAMAULIPAS, LAT_TAMAULIPAS)
    NEAREST_POINTS_TAMAULIPAS.to_csv(NAM12_PATH + 'NEAREST_POINTS_TAMAULIPAS.csv')
    
   
'''
COMO LE ESTOY DICIENDO CUTDATA= False, 
ES DECIR, NO CORTARA LOS DATOS POR LOCALIZACION
PUES A NEAREST POINTS LE PASO UN DATAFRAME VACIO
ESTO NO DARÁ ERROR. SI HAY QUE CORTAR LOS DATOS HABRIA QUE 
PASAR UN DATA FRAME GENERADO CON OBTAIN_50_NEAREST_POINTS()
COMO SE HA HECHO UN PAR DE LINEAS HACIA ARRIBA CON 
TAMAULIPAS Y TATANKA
'''

NAM12_PATH= '/media/meteobit/Elements/NAM12/'

NAM12_FILES= os.listdir(NAM12_PATH)


'''
SEPARAMOS NAM12 GRIBS Y CSVs

'''
NAM12_csv = [NAM12_PATH + item for item in NAM12_FILES if item.endswith('.csv')]
NAM12_grb2 = [NAM12_PATH + item for item in NAM12_FILES if item.endswith('.grb2')]


EMPTY_FILES= []
for i in NAM12_csv:
    if os.stat(i).st_size == 0:
        EMPTY_FILES.append(i)
        
print('HAY ' + str(len(EMPTY_FILES)) + ' CSVs VACIOS')


for i in EMPTY_FILES:
    os.remove(i)
    
    
CSVs_CREADOS= [item.split('/')[-1].split('.')[0] for item in NAM12_csv]
NAM_GRB2_faltantes = [item2 for item2 in  NAM12_grb2 if item2.split('/')[-1].split('.')[0] not in CSVs_CREADOS ]



MULTIPROCCES_EXECUTION= False

if not MULTIPROCCES_EXECUTION:

    for file in sorted(NAM_GRB2_faltantes):
        EXTRACT_INFO_NAM12_TO_CSV(SELECTED_VARIABLES=TABLA_WIND,
                                  NEAREST_POINTS=NEAREST_POINTS_TATANKA,
                                  NAME='TATANKA',
                                  SEGUNDA_LOCALIZACION=True,
                                  NAM12_FILE_NAME= file,
                                  NEAREST_POINTS2=NEAREST_POINTS_TAMAULIPAS,
                                  NAME2= 'TAMAULIPAS')
else:

    import multiprocessing as mp
    
    from functools import partial
    '''
    NOTA:
        PARA USAR PARTIAL... COMO HERRAMIENTA PARA PASAR ARGUMENTOS FIJOS A UNA 
        FUNCION CON VARIOS ARGUMENTOS, LOS ARGUMENTOS QUE QUEREMOS QUE SEAN
        ITERABLES SE DEBEN PONER EN PRIMERA POSICION A LA HORA DE DECLARAR LA 
        FUNCION... 
        FUNCION(ITERABLE, ARGUMENTOFIJO1, ARGUMENTOFIJO2 ....)
        
        NO HE PROBADO CON VARIOS ARGUMENTOS ITERABLES... PERO AUN NO ME HA HECHO
        FALTA Y TIENEN PINTA DE SER FUENTE DE PROBLEMAS. 
        
        
    NOTA2:
        A LA HORA DE DIFINIR LOS PROCESOS, SOLIA USAR mp.cpu_count()
        QUE ES EL NUMERO DE PROCESADORES DEL ORDENADOR. 
        
        PERO UNA VEZ ME DIJERON QUE PROCESOS EN PARALELO Y PROCESADORES 
        NO TIENEN PORQUE SER IGUALES.... ES DECIR. PUEDO TENER 4 PROCESADORES
        Y 40 PROCESOS EN PARALELO.... ESTO COJIDITO CON PINZAS POR FAVOR. 
        
        DE TODOS MODOS. ME ESTOY DANDO CUENTA DE QUE AUMENTANDO LOS PROCESOS 
        SE PUEDE LLEGAR A UN PUNTO DE 
    
    '''
    
    pool = mp.Pool(mp.cpu_count())
    PARTIAL_FUNC= partial(EXTRACT_INFO_NAM12_TO_CSV, SELECTED_VARIABLES=TABLA_WIND,
                                  NEAREST_POINTS=pd.DataFrame(),
                                  CUT_DATA=False)
    
    CORRUPTED_FILES = pool.map(PARTIAL_FUNC, NAM12_grb2)
    
    '''
    PARA BARRA DE PROGRESO...
    AQUI NO USAMOS PORQUE SALEN POR PANTALLA UNOS PRINT
    QUE NOS DAN UNA IDEA DE COMO VA AVANZADO
    
    import tqdm
    for _ in tqdm.tqdm(pool.imap_unordered(PARTIAL_FUNC, NAM12_grb2), 
                       total=len(NAM12_grb2)):
        pass
    '''
    
    #MATAMOS SUBPROCESOS 
    pool.close()
    pool.terminate()
    pool.join()



BORRAR_CORRUPTOS= False
if BORRAR_CORRUPTOS:
    for i in CORRUPTED_FILES:
        os.remove(i)

        

