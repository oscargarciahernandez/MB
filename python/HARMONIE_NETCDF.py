#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug  1 18:26:08 2019

@author: oscar
"""

from netCDF4 import Dataset
import os
import subprocess
from wrf import getvar, ALL_TIMES, combine_files, to_np

PATH_TO_NETCDF= '/home/oscar/MB/HARMONIE/ALBO2500-2019072900-B2019072900-HC.nc'
 
NETCDF=Dataset(PATH_TO_NETCDF)

'''
EL SIGUIENTE FOR NOS SIRVE PARA VER LA DESCRIPCION DE LAS VARIABLES 

'''
for i in NETCDF.variables.keys():
    try:
        print(NETCDF.variables[i].standard_name + ' ' + i)
    except:
        print(i + ' NO ES UNA VARIABLE')


