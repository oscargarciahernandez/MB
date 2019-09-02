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

def EXTRACT_INFO_HIRLAM_TO_CSV(HIRLAM_FILE_NAME,
                              SELECTED_VARIABLES,
                              NEAREST_POINTS,
                              CUT_DATA):
    '''
    FUNCION PARA EXTRAER LA INFORMACION PARA LAS VARIABLES REQUERIDAS
    ESTAS VARIABLES SE PUEDEN SACAR EMPLEANDO LA FUNCION EXTRACT_VARIABLES_FROM_GRIB.
    CUT DATA ES UN BOOLEANO. IF TRUE SE CORTA LA INFORMACION DE ACUERDO A NEAREST POINTS    
    '''
    
    
    HIRLAM_GRIB= pygrib.open(HIRLAM_FILE_NAME)
    
    NOMBRE_ARCHIVO = HIRLAM_FILE_NAME.replace(HIRLAM_FILE_NAME.split('/')[-1], '') + HIRLAM_FILE_NAME.split('/')[-1] + '_CONVERTED.csv'
    TABLA_TOTAL= pd.DataFrame()
    for i in range(SELECTED_VARIABLES.shape[0]):
        
        VAR_LEVEL= SELECTED_VARIABLES['TIMESTAMP'].iloc[i]
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
                
                VARIABLE = HIRLAM_GRIB.select(name= VAR_NAME)
                VARIABLE_ITEM = [item for item in VARIABLE if str(item).split(':')[-2]==VAR_LEVEL]
            
            
            
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
                
                if CUT_DATA:
                    TABLA_CUT = dfObj[(dfObj['LON'].isin(NEAREST_POINTS['LON'])) & (dfObj['LAT'].isin(NEAREST_POINTS['LAT']))]
                else:
                    TABLA_CUT= dfObj
                
                TABLA_CUT['VAR_NAME']= str(VARIABLE_ITEM[0]).split(':')[1]
                TABLA_CUT['LEVEL']= str(VARIABLE_ITEM[0]).split(':')[-3]
                TABLA_CUT['FCST_TIME']= str(VARIABLE_ITEM[0]).split(':')[-2]
                TABLA_TOTAL = TABLA_TOTAL.append(TABLA_CUT)
 
            except:
                print(HIRLAM_FILE_NAME + ' NO ENCONTRADO ' + VAR_NAME)
                
    print('GUARDANDO ' + NOMBRE_ARCHIVO)
    TABLA_TOTAL.to_csv(NOMBRE_ARCHIVO)
    return(HIRLAM_FILE_NAME)



pd.set_option('display.max_columns', 15)
pd.set_option('display.max_rows', 30)



HIRLAM_FILENAME= '/home/meteobit/Escritorio/Parques/EZ/ATHd00.20190818'

TABLA_VARIABLES = EXTRACT_VARIABLES_FROM_GRIB(HIRLAM_FILENAME, 'wind')

#LA MANERA DE FILTRAR LAS VARIABLES HAY QUE AFINARLO
TABLA_WIND= TABLA_VARIABLES[TABLA_VARIABLES.NAME.isin(set([item for item in list(TABLA_VARIABLES['NAME']) if 'wind' in item.lower()]))]

TIME_STAMP= [item for item in list(set(TABLA_WIND['TIMESTAMP'])) if '(max)' not in item]
VARIABLES = list(set(TABLA_WIND['NAME']))[0:4]


TABLA_SELECTED_VAR= TABLA_WIND[(TABLA_WIND['TIMESTAMP'].isin(TIME_STAMP)) & (TABLA_WIND['NAME'].isin(VARIABLES))]




HIRLAM= pygrib.open(HIRLAM_FILENAME)

VARIABLE_GRIB = HIRLAM.select(name= VARIABLES[0])

VARIABLE_SELECTED = [item for item in VARIABLE_GRIB if str(item).split(':')[-2]==TIME_STAMP[0]][0]

LATS , LONS = VARIABLE_SELECTED.latlons()


LON_CERROBLANCO= -2.409666 	
LAT_CERROBLANCO= 38.72292


NEAREST_POINTS_CERROBLANCO = OBTAIN_50_NEAREST_POINTS(LONS,LATS, LON_CERROBLANCO, LAT_CERROBLANCO)

PATH_TO_HIRLAM= '/home/meteobit/Escritorio/Parques/EZ/'

HIRLAM_FILES = [PATH_TO_HIRLAM + item for item in os.listdir(PATH_TO_HIRLAM) if 'ATH' in item]

EXTRACT_INFO_HIRLAM_TO_CSV(HIRLAM_FILES[0], 
                           CUT_DATA=True,
                           NEAREST_POINTS=NEAREST_POINTS_CERROBLANCO,
                           SELECTED_VARIABLES=TABLA_SELECTED_VAR)






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
    if len(LONG_NP)==2:
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
                              CUT_DATA):
    '''
    FUNCION PARA EXTRAER LA INFORMACION PARA LAS VARIABLES REQUERIDAS
    ESTAS VARIABLES SE PUEDEN SACAR EMPLEANDO LA FUNCION EXTRACT_VARIABLES_FROM_GRIB.
    CUT DATA ES UN BOOLEANO. IF TRUE SE CORTA LA INFORMACION DE ACUERDO A NEAREST POINTS    
    '''
    
    
    NAM12_GRIB= pygrib.open(NAM12_FILE_NAME)

    for i in range(SELECTED_VARIABLES.shape[0]):
        
        VAR_LEVEL= SELECTED_VARIABLES['LEVEL_Pa'].iloc[i]
        VAR_NAME= SELECTED_VARIABLES['NAME'].iloc[i]
        
        NOMBRE_ARCHIVO = NAM12_FILE_NAME.replace('grib2', '') + VAR_NAME.replace(' ', '_') + VAR_LEVEL.replace(' ', '_') + '.csv'
        
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
              
                print('CREANDO TABLA')  
                dfObj = pd.DataFrame()
                dfObj['VALUES']= VARIABLE_VALUES.ravel()
                dfObj['LON']= LONS.ravel()
                dfObj['LAT']= LATS.ravel()
                dfObj['DATE'] = DATE
                
                if CUT_DATA:
                    TABLA_CUT = dfObj[(dfObj['LON'].isin(NEAREST_POINTS['LON'])) & (dfObj['LAT'].isin(NEAREST_POINTS['LAT']))]
                else:
                    TABLA_CUT= dfObj
                    
                print('GUARDANDO CSV en ' + NOMBRE_ARCHIVO)
                TABLA_CUT.to_csv(NOMBRE_ARCHIVO, index = False)
            except:
                print(NAM12_FILE_NAME + ' NO ENCONTRADO ' + VAR_NAME)
                
                
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
NAM12_FILE_NAME= '/home/meteobit/Descargas/nam_218_20190611_0000_000.grb2'

'''
CREAMOS LAS TABLAS DE VARIABLES. LO SUYO ES IR A VER LAS VARIABLES 
CONTENIDAS AL CSV. ESTA FUNCION DEVUELVE TODAS LAS VARIABLES
LAS GUARDAMOS EN TABLA DE VARIABLES Y LUEGO FILTRAMOS POR 
NIVEL Y NOMBRE DE VARIABLE PARA SACAR LAS QUE QUEREMOS
'''
TABLA_VARIABLES = EXTRACT_VARIABLES_FROM_GRIB(NAM12_FILE_NAME, CUSTOM_WORD)


#LA MANERA DE FILTRAR LAS VARIABLES HAY QUE AFINARLO
TABLA_WIND= TABLA_VARIABLES[TABLA_VARIABLES.NAME.isin(set([item for item in list(TABLA_VARIABLES['NAME']) if 'wind' in item.lower()]))]
TABLA_WIND= TABLA_WIND[TABLA_WIND['LEVEL_Pa'].isin(['level 80 m', 'level 10 m',  'level 95000 Pa','level 97500 Pa', 'level 100000 Pa',])]



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
VARIABLE = NAM12_GRIB.select(name= TABLA_WIND['NAME'].iloc[5],
                             typeOfLevel=TABLA_WIND['TYPE_VAR'].iloc[5])

VARIABLE = [item for item in VARIABLE if str(item).split(':')[-3]==TABLA_WIND['LEVEL_Pa'].iloc[5]][0]

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
CREAR_ARCHIVOS_TOTALES = False



if CREAR_ARCHIVOS_TOTALES: 
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
    
            
    
    NAM12_grb2_CONVERTED = [item2.split('/')[-1].split('grb2')[0] + 'grb2' for item2 in NAM12_csv]
    NAM12_grb2_NO_CONVERSION = [item for item in NAM12_grb2 if item.split('/')[-1] not in NAM12_grb2_CONVERTED]
    
    
    MULTIPROCCES_EXECUTION= True
    
    if not MULTIPROCCES_EXECUTION:
    
        for file in NAM12_grb2_NO_CONVERSION:
            EXTRACT_INFO_NAM12_TO_CSV(SELECTED_VARIABLES=TABLA_WIND,
                                      NEAREST_POINTS=pd.DataFrame(),
                                      CUT_DATA=False,
                                      NAM12_FILE_NAME= file)
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
        
        CORRUPTED_FILES = pool.map(PARTIAL_FUNC, NAM12_grb2_NO_CONVERSION)
        
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

        

'''
COMO YA HEMOS VISTO QUE DEBIDO A ERRORES. 
POR EJEMPLO, QUE EL DISCODURO SE QUEDA SIN ESPACIO
Y EMPIEZA A GENERAR CANTIDAD DE CSVs VACIOS.

LO QUE HAREMOS SERA BORRAR TODOS LOS ARCHIVOS VACIOS

'''





'''
################################################################################
################################################################################
                            COJER LOS ARCHIVOS TOTALES Y 
                            CORTARLOS PARA CADA PARQUE
################################################################################
################################################################################
'''
def FROM_CSV_TO_CUT_DATA(VARIABLE,
                         NEAREST_POINTS,
                         NAME):
    '''
    ESTA FUNCNION COJE LOS CSVs GENERADOS ANTERIORMENTE Y LOS CORTA PARA UNA UNA SERIE DE PUNTOS
    OFRECIDOS POR NEAREST_POINTS... ESTE NEAREST POINTS DEBE SER UNA TABLA SACADO CON LA FUNCION 
    OBTAIN_50_NEAREST_POINTS(). NAME ES UN STRING 
    
    '''
    CSVs_VACIOS=[]
    CSVs_CORTOS = []
    
    NOMBRE_CSV= NAM12_PATH + VARIABLE + NAME + '.csv' 
    
    if os.path.isfile(NOMBRE_CSV):
        print('YA EXISTE ' + NAM12_PATH + VARIABLE)
    else:
        NAM12_CSV_FILT= [item for item in NAM12_csv if VARIABLE in item ]
        
        TABLA_FILT = pd.DataFrame()
        
        '''
        SI DETECTA UN CSV VACIO NO LO AÑADE A NUESTRA TABLA
        LO METE A UNA LISTA QUE LUEGO DEVOLVERA
        '''
        for i in NAM12_CSV_FILT:
            EXE = 1
            if os.path.isfile(i):
                EXE=1
                if os.stat(i).st_size == 0:
                    print(i +  '  ESTA VACIO')
                    CSVs_VACIOS.append(i)
                    EXE=0
                    
            if EXE==1:       
                try:
                    print(i)
                    CSV_NAM12= pd.read_csv(i, skiprows = NON_DESIRED)
                    if CSV_NAM12.shape[0] < 5000:
                        CSVs_CORTOS.append(i)
                        
                    
                    TABLA_CUT_NAME = CSV_NAM12[(CSV_NAM12['LON'].isin(NEAREST_POINTS['LON'])) & (CSV_NAM12['LAT'].isin(NEAREST_POINTS['LAT']))]
                    TABLA_CUT_NAME['TSIM'] = i.split('_')[3]
                    TABLA_CUT_NAME['SIMT'] = i.split('_')[4][:3]
                    
                    TABLA_FILT= TABLA_FILT.append(TABLA_CUT_NAME)
                except:
                    print('ERROR CON ' + i)
        
        print('GUARDANDO ' + NOMBRE_CSV)
        TABLA_FILT.to_csv(NOMBRE_CSV)
    
    
    print('DURANTE LA CREACION DE LAS TABLAS SE HAN DETECTADO ' + str(len(CSVs_VACIOS)) + ' ARCHIVOS VACIOS')
    return([CSVs_VACIOS, CSVs_CORTOS])




CREAR_ARCHIVOS_TATANKA = True
if CREAR_ARCHIVOS_TATANKA:
    
    NAM12_PATH= '/media/meteobit/Elements/NAM12/'
    
    NAM12_FILES= os.listdir(NAM12_PATH)
    
    
    '''
    VEMOS LOS CSVs QUE SE HA GENERADO
    '''
    NAM12_csv = [NAM12_PATH + item for item in NAM12_FILES if item.endswith('.csv')]
    
    
    '''
    COMO ESTAMOS GUARDANDO LOS CSVs POR VARIABLES, VEMOS CUANTAS VARIABLES HAY
    '''
    VARIABLE_GROUPS = list(set([item.split('.')[1].replace('grb2','') for item in NAM12_csv]))
    
    
    
    '''
    LEEMOS UN CSV Y SACAMOS LAS COLUMNAS UTILES
     (LAS 50 MAS CERCANAS A TATANKA EN ESTE CASO). DE ESTA MANERA
    PODEMOS SABER CUALES SON LOS IDEX DE LAS COLUMNAS NECESARIAS
    DE ESTA MANERA PODEMOS ABRIR SOLAMENTE ABRIR LAS FILAS QUE 
    NECESITAMOS Y AGILIZAMOS EL PROCESO
    '''
    CSV_NAM12= pd.read_csv(NAM12_csv[0])
    
    
    TABLA_CUT_TATANKA = CSV_NAM12[(CSV_NAM12['LON'].isin(NEAREST_POINTS_TATANKA['LON'])) & (CSV_NAM12['LAT'].isin(NEAREST_POINTS_TATANKA['LAT']))]
    
    
    '''
    SACAMOS LOS INDEX BUENOS Y AÑADIMOS EL 0 (HEADERS)
    '''
    INDEX_1= list(TABLA_CUT_TATANKA.index.values.astype(int))
    INDEX_1.append(0)
    
    INDEX_ALL = list(CSV_NAM12.index.values.astype(int))
    
    '''
    GENERAMOS LOS INDEX NO NECESARIOS
    '''
    NON_DESIRED = [item for item in INDEX_ALL if item not in INDEX_1 ]

    MULTIPROCCES_EXECUTION= True
    
    if not MULTIPROCCES_EXECUTION:
        for VARIABLE in VARIABLE_GROUPS:
            FROM_CSV_TO_CUT_DATA(NEAREST_POINTS=NEAREST_POINTS_TATANKA,
                                 NAME = 'TATANKA',
                                 VARIABLE= VARIABLE)
    else:
    
        import multiprocessing as mp
        from functools import partial
        
        pool = mp.Pool(mp.cpu_count())
        PARTIAL_FUNC= partial(FROM_CSV_TO_CUT_DATA,
                                      NEAREST_POINTS=NEAREST_POINTS_TATANKA,
                                      NAME = 'TATANKA')
        
        CSV_CORTOS =  pool.map(PARTIAL_FUNC, VARIABLE_GROUPS)
        
        '''
        BARRA DE PROCESO
        import tqdm
    
        for _ in tqdm.tqdm(pool.imap_unordered(PARTIAL_FUNC, VARIABLE_GROUPS), 
                           total=len(NAM12_grb2)):
            pass
        
        '''
        
        #MATAMOS SUBPROCESOS 
        pool.close()
        pool.terminate()
        pool.join()
    



CREAR_ARCHIVOS_TAMAULIPAS = True
if CREAR_ARCHIVOS_TAMAULIPAS:
    
    NAM12_PATH= '/media/meteobit/Elements/NAM12/'
    
    NAM12_FILES= os.listdir(NAM12_PATH)
    
    
    '''
    VEMOS LOS CSVs QUE SE HA GENERADO
    '''
    NAM12_csv = [NAM12_PATH + item for item in NAM12_FILES if item.endswith('.csv')]
    
    
    '''
    COMO ESTAMOS GUARDANDO LOS CSVs POR VARIABLES, VEMOS CUANTAS VARIABLES HAY
    '''
    VARIABLE_GROUPS = list(set([item.split('.')[1].replace('grb2','') for item in NAM12_csv]))
    
    
    
    '''
    LEEMOS UN CSV Y SACAMOS LAS COLUMNAS UTILES
     (LAS 50 MAS CERCANAS A TAMAULIPAS EN ESTE CASO). DE ESTA MANERA
    PODEMOS SABER CUALES SON LOS IDEX DE LAS COLUMNAS NECESARIAS
    DE ESTA MANERA PODEMOS ABRIR SOLAMENTE ABRIR LAS FILAS QUE 
    NECESITAMOS Y AGILIZAMOS EL PROCESO
    '''
    CSV_NAM12= pd.read_csv(NAM12_csv[0])
    
    
    TABLA_CUT_TAMAULIPAS = CSV_NAM12[(CSV_NAM12['LON'].isin(NEAREST_POINTS_TAMAULIPAS['LON'])) & (CSV_NAM12['LAT'].isin(NEAREST_POINTS_TAMAULIPAS['LAT']))]
    '''
    SACAMOS LOS INDEX BUENOS Y AÑADIMOS EL 0 (HEADERS)
    '''
    INDEX_1= list(TABLA_CUT_TAMAULIPAS.index.values.astype(int))
    INDEX_1.append(0)  
    INDEX_ALL = list(CSV_NAM12.index.values.astype(int))
    
    
    '''
    GENERAMOS LOS INDEX NO NECESARIOS
    '''
    NON_DESIRED = [item for item in INDEX_ALL if item not in INDEX_1 ]


    MULTIPROCCES_EXECUTION= True  
    if not MULTIPROCCES_EXECUTION:
        for VARIABLE in VARIABLE_GROUPS:
            FROM_CSV_TO_CUT_DATA(NEAREST_POINTS=NEAREST_POINTS_TAMAULIPAS,
                                 NAME = 'TAMAULIPAS',
                                 VARIABLE= VARIABLE)
    else:
    
        import multiprocessing as mp
        from functools import partial
        
        pool = mp.Pool(mp.cpu_count()-2)
        PARTIAL_FUNC= partial(FROM_CSV_TO_CUT_DATA,
                                      NEAREST_POINTS=NEAREST_POINTS_TAMAULIPAS,
                                      NAME = 'TAMAULIPAS')
        
        CSV_CORTOS =  pool.map(PARTIAL_FUNC, VARIABLE_GROUPS)
        
        '''
        BARRA DE PROCESO
        import tqdm
    
        for _ in tqdm.tqdm(pool.imap_unordered(PARTIAL_FUNC, VARIABLE_GROUPS), 
                           total=len(NAM12_grb2)):
            pass
        
        '''
        #MATAMOS SUBPROCESOS 
        pool.close()
        pool.terminate()
        pool.join()