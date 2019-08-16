# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 12:03:11 2019

@author: asus
"""

from netCDF4 import Dataset
import os
import subprocess
from wrf import getvar, ALL_TIMES, to_np
import re
from math import sin, cos, sqrt, atan2, radians
import pandas as pd
import numpy as np
import shutil



'''
#######################################################################################################################################
CONFIG
#######################################################################################################################################
'''
'''
HAY QUE PONER LA POSICION DEL PARQUE Y EL PATH DONDE SE ENCUENTRAN LAS CARPETAS DE LAS SIMULACIONES
ESTE SCRIPT SE ENCARGA DEL RESTO.
CONCATENA LOS ARCHIVOS, SACA LA INFORMACION DE VIENTO CREANDO UN CSV POR SIMULACION Y NIVEL 
Y BORRA EL RESTO DE LAS CARPETAS DEJANDO UNICAMENTE LOS CSVS Y LOS ARCHIVOS CONCATENADOS
'''


LAT_PARQUE= 25.78788
LON_PARQUE= -98.196080
PATH_OUTPUTS= '/home/meteobit/TAMAULIPAS_OUTPUT/tamaulipas/'


'''
#######################################################################################################################################
/CONFIG
#######################################################################################################################################
'''







'''
#######################################################################################################################################
FUNCIONES
#######################################################################################################################################
'''

def CREATE_FOLDER(PATH):
    if not os.path.exists(PATH):
            os.makedirs(PATH)



def OBTAIN_50_NEAREST_POINTS(LONG_NP, LAT_NP, LON, LAT):
    '''
    FUNCION PARA OBTENER LOS 50 PUNTOS MAS CERCANOS A LA ZONA A ESTUDIAR...
    SACAR INFORMACION DE TODOS LOS PUNTOS ES MUY EXIGENTE PARA ESTE SCRIPT
    '''
    
    
    
    TABLA_LONLAT=pd.DataFrame(columns=['LON', 'LAT'])
    TABLA_LONLAT['LON']= LONG_NP[:, :].ravel()
    TABLA_LONLAT['LAT']= LAT_NP[:, :].ravel()
    
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

def CONCAT_AND_NCVIEW(PATH_DOMAIN, NCVIEW= True):
    '''
    ESTA FUNCION SE ENCARGA DE CONCATENAR TODOS LOS NETCDF's EN UN UNICO ARCHIVO    
    ADEMAS, AJUSTANDO NCVIEW A TRUE SACA LA VENTANA DE NCVIEW.     
    COMO RETORNO DEVUELVE EL PATH DEL ARCHIVO CONCATENADO PARA POSTERIORES ACCIONES SOBRE ESE ARCHIVO
    
    '''
    
    MERGE_FILE= PATH_DOMAIN.split('/')[-2] + '.nc'
    if MERGE_FILE in os.listdir(PATH_DOMAIN):
        print('\nYA EXISTE EL ARCHIVO CONCATENADO')
    else:
        COMMAND_MERGE_NETCDFS='ncecat * -O ' + MERGE_FILE 
        procces= subprocess.Popen(COMMAND_MERGE_NETCDFS, stdout= subprocess.PIPE, cwd= PATH_DOMAIN, shell=True)
        output, error = procces.communicate()
        
        '''
        QUIERO ELIMINAR TODOS LOS ARCHIVOS JUSTO DESPUES DE CONCATENARLOS
        LISTA_WRFOUT= [item for item in os.listdir(PATH_DOMAIN) if 'wrfout_' in item]
        for i in LISTA_WRFOUT:
            os.remove(PATH_DOMAIN + i)
        '''
    
    if NCVIEW:
        COMMAND_NCVIEW='ncview ' + PATH_DOMAIN.split('/')[-2] + '.nc'   
        #EJECUTAMOS EL MODELO 
        procces= subprocess.Popen(COMMAND_NCVIEW, stdout= subprocess.PIPE, cwd= PATH_DOMAIN, shell=True)
        output, error = procces.communicate()
    
    return PATH_DOMAIN + MERGE_FILE


  
    
def FIND_CONCAT_NC_FILE(PATH_OUTPUTS):
    '''
    A ESTA FUNCION SE LE METE EL PATH DONDE ESTAN LAS CARPETAS ORDENADAS DEL MODELO Y BUSCA TODOS LOS ARCHIVOS CONCATENADOS.
    
    '''

    
    LISTA_FILES= []
    for root, dirs, files in os.walk(PATH_OUTPUTS, topdown=False):
       for name in files:
          LISTA_FILES.append(os.path.join(root, name))
    
    PATTERN_NC= re.compile(r'.*?D0\d.nc')
    
    ARCHIVO_NC= list(filter(PATTERN_NC.match, LISTA_FILES))
    return ARCHIVO_NC


'''
#######################################################################################################################################
/FUNCIONES
#######################################################################################################################################
'''



'''
#######################################################################################################################################
CODIGO
#######################################################################################################################################
'''

'''
BUCLE PARA CONCATENAR LOS ARCHIVOS NETCDFS EN UN UNICO ARCHIVO

'''


SUBDIRS= os.listdir(PATH_OUTPUTS)


'''
ESTO ES EXCLUSIVO PARA TAMAULIPAS PORQUE SE NOS COLARON ESTAS SIMULACIONES QUE NO NOS 
VALE, POR ESO LAS ELIMINO

'''

SUBDIRS.remove('20180102')
SUBDIRS.remove('20180105')


for dirs in SUBDIRS:
    PATH_TO_NETCDFS= PATH_OUTPUTS + dirs + '/'
    
    PATH_D01= PATH_TO_NETCDFS + 'D01/'
    PATH_D02= PATH_TO_NETCDFS + 'D02/'
    PATH_D03= PATH_TO_NETCDFS + 'D03/'
        
    
    
    
    FILES= os.listdir(PATH_TO_NETCDFS)
    
    if 'D01' and 'D02' and 'D03' in FILES:
        
        print('\n SEPARACION POR DOMINIOS YA HA SIDO REALIZADA ANTES')
    else:    
        FILES_D01= [item for item in [wrfout for wrfout in FILES if 'wrfout_' in wrfout] if int(item.split('_')[1][-2:])==1]
        
        FILES_D02= [item for item in [wrfout for wrfout in FILES if 'wrfout_' in wrfout] if int(item.split('_')[1][-2:])==2]
        
        FILES_D03= [item for item in [wrfout for wrfout in FILES if 'wrfout_' in wrfout] if int(item.split('_')[1][-2:])==3]
    
    

        CREATE_FOLDER(PATH_D01)
        CREATE_FOLDER(PATH_D02)
        CREATE_FOLDER(PATH_D03)
        
        for i in FILES_D01:
            os.rename(PATH_TO_NETCDFS + i, PATH_D01 + i.replace(':', '_')  )
        
        for i in FILES_D02:
            os.rename(PATH_TO_NETCDFS + i, PATH_D02 + i.replace(':', '_')  )
        for i in FILES_D03:
            os.rename(PATH_TO_NETCDFS + i, PATH_D03 + i.replace(':', '_')  )
    
    
    
    
    
    PATH_D01_CONCAT= CONCAT_AND_NCVIEW(PATH_D01,NCVIEW=False)
    PATH_D02_CONCAT= CONCAT_AND_NCVIEW(PATH_D02,NCVIEW=False)
    PATH_D03_CONCAT= CONCAT_AND_NCVIEW(PATH_D03,NCVIEW=False)
  

'''
SACAMOS TODO LOS NCS CONCATENDADOS 
BORRAMOS LOS QUE ESTAN DAÑADOS O TMP Y VOLVEMOS A LLAMARLOS ASI, TENEMOS UNA LISTA DE ARCHIVOS 

'''
NC_CONCAT= FIND_CONCAT_NC_FILE(PATH_OUTPUTS)


#ELIMINAMOS FALLOS DEL NCCAT....
for i in [item for item in NC_CONCAT if 'ncecat.tmp' in item]:
    os.remove(i)

NC_CONCAT= FIND_CONCAT_NC_FILE(PATH_OUTPUTS)





'''
BUCLE PARA ABRIR CADA ARCHIVO COJER UNICAMENTE LA INFORMACION DEL VIENTO Y CREAR UN CSV
VA A CREAR UN CSV POR NIVEL... SOLAMENTE PARA LOS PRIMEROS 10 NIVELES DEL MODELO, DESDE 0 HASTA 9

'''

for Dominios in range(len(NC_CONCAT)):
    ARCHIVO_NC= NC_CONCAT[Dominios]
    
    '''
    SI DETECTA QUE ESTAN LOS 9 CSV GENERADOS DIRECTAMENTE PASA AL SIGUIENTE OBJETIVO
    
    '''
    if len([item for item in os.listdir(ARCHIVO_NC.replace(ARCHIVO_NC.split('/')[-1], '')) if '.csv' in item])>9:
        print('CSVs YA GENERADOS ANTERIORMENTE')
        pass
    else:
    
        
        '''
        ABRIMOS EL ARCHIVO NETCDF
        
        '''
        NETCDF= Dataset(ARCHIVO_NC)
        
        
        '''
        EL SIGUIENTE FOR NOS SIRVE PARA VER LA DESCRIPCION DE LAS VARIABLES 
        
        
        DISPLAY_VARIABLES_DESCRIPTION= True
        if DISPLAY_VARIABLES_DESCRIPTION: 
            for i in NETCDF.variables.keys():
                try:
                    print(NETCDF.variables[i].description + ' ' + i)
                except:
                    print(i + ' NO ES UNA VARIABLE')
       
        
        '''
        '''
        A PARTIR DE AQUI EXTRAEMOS LAS VARIBLES QUE NOS SERAN UTILES PARA EL 
        ANALISIS DE LOS PARQUES, A PARTE DE LA VELOCIDAD DEL VIENTO, TAMBIEN
        PODRAN SER INTERESANTES OTRAS VARIABLES QUE PUEDAN AFECTAR AL FUNCIONAMIENTO
        DEL PARQUE. 
        
        '''
        
        
        '''
        LAS VARIABLES DE VIENTO ESTAN ORGANIZADAS COMO VARIABLE[TIEMPO, Z, LAT, LON]
        LONG TIENE DOS DIMENSIONES (LAT, LON)
        LAT TIENE DOS DIMENSIONES (LAT, LON)
        Z TIENE 4 DIMENSIONES (TIEMPO, ALTURA, LAT, LON)
        

        
        '''    
        
        
        #VARIABLES 
        LONG= getvar(NETCDF, 'lon')
        LAT= getvar(NETCDF, 'lat')
        Z= getvar(NETCDF, 'z', timeidx=ALL_TIMES)
        DATE= getvar(NETCDF, 'times', timeidx=ALL_TIMES)
        LONG_NP= to_np(LONG)
        LAT_NP= to_np(LAT)
        Z_NP= to_np(Z)
        DATE_NP=to_np(DATE)
        
        # VELOCIDAD Y DIRECCION EN LA MISMA TABLA
        WS_WD= getvar(NETCDF, 'uvmet_wspd_wdir', timeidx=ALL_TIMES)
        #COMPONENTES U Y V (JUNTAS) Y COMPONENTE W. ROTATED TO EARTH COORDINATES
        
        UV= getvar(NETCDF, 'uvmet', timeidx=ALL_TIMES)
        W= getvar(NETCDF, 'wa', timeidx=ALL_TIMES)
        NETCDF.close()
        
        
        '''
        HAY VARIABLES QUE NO FUNCIONAN CON getvar()... CREO QUE TIENE QUE VER CON EL FORMATO. 
        ANTES FUNCIONABA... YO LO QUE HACIA ERA USAR GETVAR Y OBTENIA UNA 
        ARCHIVO FORMATO XARRAY QUE LUEGO TENIA QUE CONVERTIR A ALGO MAS UTILIZABLE
        EMPLEANDO TO_NP... PARA CONVERTIR A FORMATO NUMPY...
        
        AHORA PARECE QUE ESTE PASO YA NO EXISTE, ES DECIR... GETVAR NO ME DEVULVE
        XARRAY Y POR LO TANTO NO TENGO QUE USAR TO_NP. AUNQUE SE PUEDE EMPLEAR
        DE TODOS MODOS QUE NO DA ERROR. 
        
        PERO... HAY COSAS QUE NO FUNCIONAN... TODAS LAS VARIABLES QUE ESTAN AQUI, 
        DENTRO NO FUNCIONAN... IMAGINO QUE SERA POR ALGO ASI...
        DA IGUAL... SE PUEDE SEGUIR EL PROCESO EMPLEANDO LAS VARIABLES 
        QUE SACO JUSTO DESPUES DE ESTE MEGA-COMENTARIO. 
        ESTO ME DEJA UN POCO DESPISTADO, PERO VOY A HACER COMO QUE NO HE VISTO NADA.
        
        
        
        
        #WIND SPEED AND WIND DIRECTION. ROTATED TO EARTH COORDINATES
        WS= getvar(NETCDF, 'uvmet_wspd', timeidx=ALL_TIMES)
        WD= getvar(NETCDF, 'uvmet_wdir', timeidx=ALL_TIMES)
        
        #LO MISMO QUE LO ANTERIOR PERO A 10 METROS
        WS10= getvar(NETCDF, 'uvmet10_wspd', timeidx=ALL_TIMES)
        WD10= getvar(NETCDF, 'uvmet10_wdir', timeidx=ALL_TIMES)
        
        
        WS_NP= to_np(WS)
        WD_NP= to_np(WD)
        '''
        

        
        
        
        
        '''
        COMO ESTAMOS HABLANDO DE TONELADAS DE DATOS VAS A CORTAR LOS 50 PUNTOS MAS CERCANOS AL PARQUE
        
        '''
        TABLA_50_NEAR_P= OBTAIN_50_NEAREST_POINTS(LONG_NP, LAT_NP, LON=LON_PARQUE, LAT=LAT_PARQUE)

        
        
        DIMENSIONES= WS_WD.shape
        
        k=0
        for z in range(10):
            CSV= ARCHIVO_NC[:-3]+'_L'+ str(z) + '.csv'
            if os.path.isfile(CSV):
                print(CSV  + ' YA EXISTE')
            
            else:
            
                dfObj = pd.DataFrame(columns = ['DATE' , 'LON', 'LAT', 'LEVEL','H_LEVEL' , 'WS', 'WD', 'U' , 'V', 'W'])
                
              
                for DATE in range(DIMENSIONES[1]):
                    for lat in range(DIMENSIONES[3]):
                        for lon in range(DIMENSIONES[4]):
                            if LONG_NP[lat, lon] in np.float32(TABLA_50_NEAR_P['LON']):
                                if LAT_NP[lat, lon] in np.float32(TABLA_50_NEAR_P['LAT']):
                                    #print('TRUE')
                                    dfObj.loc[k]=[DATE_NP[DATE], 
                                                  LONG_NP[lat, lon], 
                                                  LAT_NP[lat, lon],
                                                  z,
                                                  round(Z_NP[DATE,z, lat, lon], 0), 
                                                  round(WS_WD[0,DATE,z, lat, lon],2), 
                                                  round(WS_WD[1,DATE,z, lat, lon],2),
                                                  round(UV[0,DATE,z, lat, lon],2),
                                                  round(UV[1,DATE,z, lat, lon],2),
                                                  round(W[DATE,z, lat, lon], 2)]
                                    k=k+1
                           
            
            
                dfObj.to_csv(CSV)





'''
MOVEMOS LOS ARCHIVOS CONCATENADOS Y LOS CSVs A UNA CARPETA EXTERIOR

'''




LISTA_CSV= []
for root, dirs, files in os.walk(PATH_OUTPUTS, topdown=False):
   for name in files:
       if '.csv' in name:
           LISTA_CSV.append(os.path.join(root, name))
PATH_CSV=     PATH_OUTPUTS + 'CSV/'    
if not os.path.exists(PATH_CSV):
    os.makedirs(PATH_CSV)
for file in LISTA_CSV:         
    shutil.move(file, PATH_CSV + file.split('/')[-1].replace('.csv', '') + '_' + file.split('/')[-3] + '.csv')
    
    
    

LISTA_NC= FIND_CONCAT_NC_FILE(PATH_OUTPUTS)
PATH_NC= PATH_OUTPUTS + 'CONCAT_NC/'    
if not os.path.exists(PATH_NC):
    os.makedirs(PATH_NC)
for file in LISTA_NC:         
    shutil.move(file, PATH_NC + file.split('/')[-1].replace('.nc', '') + '_' + file.split('/')[-3] + '.nc')
    
    
    
'''
BORRAMOS EL RESTO DE CARPETAS MENOS LAS QUE CONTIENEN LOS CSVs Y LOS ARCHIVOS CONCATENADOS

''' 
DIRECTORIOS= os.listdir(PATH_OUTPUTS)
DIRECTORIOS.remove('CSV')
DIRECTORIOS.remove('CONCAT_NC')

for dirs in DIRECTORIOS:
    shutil.rmtree(PATH_OUTPUTS + dirs, ignore_errors=True)
    


'''
#######################################################################################################################################
CODIGO
#######################################################################################################################################
'''
