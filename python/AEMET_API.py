#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug  4 15:06:28 2019

@author: oscar
"""

import requests 
from pandas.io.json import json_normalize
import pandas as pd



API_key= 'eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJnYXJjaWE5Ni5vc2NhckBnbWFpbC5jb20iLCJqdGkiOiJjODFjZjk5Yy0xOWJkLTQyOWUtYmViYy0yZjFmNjg0M2FhZjAiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTU2NDg0ODU5NCwidXNlcklkIjoiYzgxY2Y5OWMtMTliZC00MjllLWJlYmMtMmYxZjY4NDNhYWYwIiwicm9sZSI6IiJ9.BvwiJNgmr64LDVLo3pc6Hw6R2y2PbkpPlZYjTzIh2ds'
header= {'Accept' : 'application/json',
          'api_key' : API_key}
URL= "https://opendata.aemet.es/opendata/api/prediccion/especifica/municipio/diaria/01001"

REQUEST= requests.get(URL, headers= header)


URL_DATOS= REQUEST.json()['datos']
REQUEST_DATOS= requests.get(URL_DATOS)







# INFORMACION DEL ARCHIVO
FECHA_CREACION= REQUEST_DATOS.json()[0]['elaborado']
MUNICIPIO= REQUEST_DATOS.json()[0]['nombre']
PROVINCIA= REQUEST_DATOS.json()[0]['provincia']

#A PARTIR DE AQUI EMPEZAMOS A SACAR DATOS DE LA PREDICCION
DATA= REQUEST_DATOS.json()[0]['prediccion']['dia']



'''
COMO TIENEN FORMATOS DIFERENTES CADA DATO LO VAMOS A SACAR DE MANERA DIFERENTE
LA PROBABILIDAD DE PRCIPITACION COTA DE NIEVE ESTADO DEL CIELO Y 
VIENTO LO VAMOS A SACAR CON EL SIGUIENTE BUCLE 

'''

#COJEMOS TODAS LAS VARIABLES CONTENIDAS EN EL JSON 
DAY= REQUEST_DATOS.json()[0]['prediccion']['dia'][0]
CONTET_DAY= list(DAY.keys())

#ELIMINAMOS LAS VARIABLES QUE NO NOS INTERESAN
REMOVE_ELEMENTS= ['rachaMax', 'temperatura','sensTermica',  'humedadRelativa', 'fecha', 'uvMax']
for i in REMOVE_ELEMENTS:
    CONTET_DAY.remove(i)


#CREAMOS LAS TABLAS CON LAS VARIABLES PROB_PRECIPITACION
    # COTA DE NIEVE
    # ESTADO DEL CIELO 
    # VIENTO
lista_values=[]
for i in CONTET_DAY:
    JSON_DATA_FRAME= json_normalize(DATA,record_path=[i],meta=['fecha'],errors='ignore')
    lista_values.append(JSON_DATA_FRAME)




#AHORA SACAMOS TEMPERATURA SENSACION TERMICA Y HUMEDAD RELATIVA

#ESTE PRIMER BUCLE PARA OBTENER LOS DATOS 6HORARIOS DE LOS PROXIMOS 
    # 2 DIAS
TEM_SENS_HUM= ['temperatura','sensTermica',  'humedadRelativa']  
LIST_TEMP=[]
for i in TEM_SENS_HUM:
    LIST_INTEE=[]

    for days in DATA:
        JSON_DATA_FRAME= json_normalize(days[i]['dato'])
        FILL_DF= JSON_DATA_FRAME.assign(fecha= days['fecha'])
        if not FILL_DF.empty:
            LIST_INTEE.append(FILL_DF)
    LIST_TEMP.append(pd.concat(LIST_INTEE))


# ESTE SEGUNDO BUCLE PARA SACAR MAXIMAS Y MINIMAS DIARIAS
LIST_MAX=[]
for i in TEM_SENS_HUM:
    LIST_INTEE=[]
    for days in DATA:
        JSON_DATA_FRAME= json_normalize(days[i])
        JSON_DATA_FRAME= JSON_DATA_FRAME.drop(['dato'], axis=1)
        LIST_INTEE.append(JSON_DATA_FRAME.assign(fecha= days['fecha']))
    LIST_MAX.append(pd.concat(LIST_INTEE))
