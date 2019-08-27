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

GRIB_SOURCE=   '/media/meteobit/Elements/GRIB025_JUNIO/'


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



def CREATE_EMPTY_FOLDER(FECHA_EJECUCION):
    
    os.chdir(PATH_OUTPUT)
    if not os.path.exists(PATH_SAVE_OUTPUT):
        os.makedirs(PATH_SAVE_OUTPUT)
        
        
    PATH_SAVE_EXE_DATE= PATH_SAVE_OUTPUT + '/' + FECHA_EJECUCION
    
    if not os.path.exists(PATH_SAVE_EXE_DATE):
        os.makedirs(PATH_SAVE_EXE_DATE)
    
    


def DETECTAR_RESULTADO_WRF(output):
    '''
    Esta funcion analiza el mensage de salida de las simulaciones de wrf (aka output)
    y busca en ella dos frases:
        1) Si tiene exito : "Your awesome UEMS Simulation party is complete - <<fecha>> "
    2) Si falla: "Your UEMS Prep party was busted at <<fecha>> "
    
    Devolverá un string para informar del resultado, que puede ser:
        - 'exito' si la simulacion se ha completado
        - 'fracaso' si la simulacion NO se ha completado
        - 'desconocido' si no es capaz de encontrar ninguna de las dos frases que nos indica el resultado, o si encuentra las dos
        '''
    import re
    
    patron_busted=re.compile('Your UEMS Prep party was busted at.+UTC')
    patron_complete=re.compile('Your awesome UEMS Simulation party is complete.+UTC')
    match_busted=patron_busted.search(str(output))
    match_complete=patron_complete.search(str(output))
    
    if (((match_complete==None) & (match_busted==None)) | (bool(match_complete) & bool(match_busted))):     #Si el mensage de salida (output) no tiene mensagito ni de exito ni de fallo, o tiene ambos
        return 'desconocido'
    if (bool(match_complete) & (match_busted==None)):     #Si tiene exito
        return 'exito'
    if (bool(match_busted) & (match_complete==None)):   #Si falla
        return 'fracaso'



# PONEMOS LOS DÍAS QUE QUEREMOS DESDE 20190101 HASTA 20190631
dias_2018= []
for mes in np.arange(1,13):
    for dia in np.arange(1,32):
        try:
            dias_2018.append(datetime.datetime(2019, mes, dia).strftime("%Y%m%d"))
        except:
            pass

    

#EJECUTAMOS EL MODELO PARA TODOS LOS DIAS UN PERIODO DE 24 HORAS
# CORREMOS EL MODELO CON --noscour ESO ES IMPORTANTE PORQUE ASI NO SE BORRAN LOS GRIBS 
# Y SE PUEDEN EMPLEAR PARA CORRER OTROS DOMINIOS
#EJECUTAMOS WRF CON SUBPROCCES

if not os.path.exists(PATH_SAVE):
    os.makedirs(PATH_SAVE)

if not os.path.exists(PATH_SAVE_OUTPUT):
    os.makedirs(PATH_SAVE_OUTPUT)
    
DIAS_JUNIO= [item for item in dias_2018 if '201906' in item]

for FECHA_EJECUCION in  DIAS_JUNIO: 
    
    #COPIAMOS LOS GRIBS A LA CARPETA DE GRIBS
    SIMULACIONES_HECHAS= os.listdir(PATH_SAVE_OUTPUT)

    if FECHA_EJECUCION in SIMULACIONES_HECHAS:
        print('\n SIMULACION ' + FECHA_EJECUCION  + ' YA HECHA ')
        pass
    
    else:
        try:
            COPY_FROM_X_TO_GRIB(FECHA_EJECUCION)
        except:
            print('NO ESTA CONECTADO ELEMENTS LANZAMOS EL MODELO Y QUE SE BUSQUE LA VIDA PARA DESCARGAR LOS GRIBS...')
        
        #CREAMOS EL COMANDO ANADIENDO LA FECHA DE EJECUCION 
        command_modelo= './ems_prep --date ' +  FECHA_EJECUCION + ' --cycle 00:00:48 --dset gfsp25 --sleep 1 --attempts 15 --domain 1,2 && ./ems_run --domain 1,2'
        print('\n Ejecutamos modelo para el dia ' + FECHA_EJECUCION + ' --- ' + str(datetime.datetime.now())[:-7])
        
        
        #EJECUTAMOS EL MODELO 
        procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
        output, error = procces.communicate()
        print(output)        
        
        
        #HA FALLADO EL MODELO? 
        resultado=DETECTAR_RESULTADO_WRF(output=output)
        
        if resultado == 'desconocido':
            print('\n ERROR. No se ha podido determinar si el modelo para el dia ' + FECHA_EJECUCION + ' se ha completado o no')
            print('\n VOLVEMOS A EJECUTARLO IGUAL IGUAL --- '+ str(datetime.datetime.now())[:-7])
            #EJECUTAMOS EL MODELO 
            procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
            output, error = procces.communicate()
            print(output)
            if resultado =='desconocido':
                print('\n ERROR. No se ha podido determinar POR SEGUNDA VEZ si el modelo para el dia ' + FECHA_EJECUCION + ' se ha completado o no')
                print('\n NOS RENDIMOS Y PASAMOS AL DIA SIGUIENTE --- ' + str(datetime.datetime.now())[:-7])
                
                
        if resultado == 'fracaso':
            print('\n ERROR. El modelo para el dia ' + FECHA_EJECUCION + ' HA FALLADO. Volvemos a intentarlo con otro grib --- '+ str(datetime.datetime.now())[:-7])
            #CREAMOS EL COMANDO ANADIENDO LA FECHA DE EJECUCION 
            command_modelo= './ems_prep --date ' +  FECHA_EJECUCION + ' --cycle 06:00:42 --dset gfsp25 --sleep 1 --attempts 15 --domain 1,2 && ./ems_run --domain 1,2'
            print('\n Ejecutamos modelo para el dia ' + FECHA_EJECUCION + ' --- ' + str(datetime.datetime.now())[:-7])
            
            #EJECUTAMOS EL MODELO 
            procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
            output, error = procces.communicate()
            print(output)
            
            #HA FALLADO EL MODELO? 
            resultado=DETECTAR_RESULTADO_WRF(output=output)
            
            
            if resultado == 'fracaso':
                print('\n ERROR. El modelo para el dia ' + FECHA_EJECUCION + ' HA FALLADO. Volvemos a intentarlo con otro grib --- '+ str(datetime.datetime.now())[:-7])
                #CREAMOS EL COMANDO ANADIENDO LA FECHA DE EJECUCION 
                command_modelo= './ems_prep --date ' +  FECHA_EJECUCION + ' --cycle 12:00:36 --dset gfsp25 --sleep 1 --attempts 15 --domain 1,2 && ./ems_run --domain 1,2'
                print('\n Ejecutamos modelo para el dia ' + FECHA_EJECUCION + ' --- ' + str(datetime.datetime.now())[:-7])
                
                #EJECUTAMOS EL MODELO 
                procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
                output, error = procces.communicate()
                print(output)   
        
                #HA FALLADO EL MODELO? 
                resultado=DETECTAR_RESULTADO_WRF(output=output)
                
                
                if resultado == 'fracaso':
                    print('\n ERROR. El modelo para el dia ' + FECHA_EJECUCION + ' HA FALLADO. Volvemos a intentarlo con otro grib --- '+ str(datetime.datetime.now())[:-7])
                    #CREAMOS EL COMANDO ANADIENDO LA FECHA DE EJECUCION 
                    command_modelo= './ems_prep --date ' +  FECHA_EJECUCION + ' --cycle 18:00:30 --dset gfsp25 --sleep 1 --attempts 15 --domain 1,2 && ./ems_run --domain 1,2'
                    print('\n Ejecutamos modelo para el dia ' + FECHA_EJECUCION + ' --- ' + str(datetime.datetime.now())[:-7])
                    
                    
                    #EJECUTAMOS EL MODELO 
                    procces= subprocess.Popen(command_modelo,stdout= subprocess.PIPE, cwd= PATH_MODELO, shell=True)
                    output, error = procces.communicate()
                    print(output)   
            
                    #HA FALLADO EL MODELO? 
                    resultado=DETECTAR_RESULTADO_WRF(output=output)
                    
                    
                    if resultado == 'fracaso':
                        print('\n ERROR. Despues de intentarlo con todos los gribs posibles, el modelo sigue fallando')
                        print('\n NOS RENDIMOS Y PASAMOS AL DIA SIGUIENTE --- ' + str(datetime.datetime.now())[:-7])
                        
        if resultado == 'exito':
            #COPIAMOS LA SALIDA DEL MODELO A CARPETA EXTERIOR
            COPY_NETCDF_FILES(FECHA_EJECUCION)
        else:
            CREATE_EMPTY_FOLDER(FECHA_EJECUCION)
            
        

    
