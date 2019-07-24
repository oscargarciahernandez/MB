# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 12:03:11 2019

@author: asus
"""

from netCDF4 import Dataset
import os
import subprocess



def CREATE_FOLDER(PATH):
    if not os.path.exists(PATH):
            os.makedirs(PATH)



PATH_TO_NETCDFS= '/home/asus/20180102/'

FILES= os.listdir(PATH_TO_NETCDFS)

FILES_D01= [item for item in [wrfout for wrfout in FILES if 'wrfout_' in wrfout] if int(item.split('_')[1][-2:])==1]

FILES_D02= [item for item in [wrfout for wrfout in FILES if 'wrfout_' in wrfout] if int(item.split('_')[1][-2:])==2]

FILES_D03= [item for item in [wrfout for wrfout in FILES if 'wrfout_' in wrfout] if int(item.split('_')[1][-2:])==3]


PATH_D01= PATH_TO_NETCDFS + 'D01/'
PATH_D02= PATH_TO_NETCDFS + 'D02/'
PATH_D03= PATH_TO_NETCDFS + 'D03/'

CREATE_FOLDER(PATH_D01)
CREATE_FOLDER(PATH_D02)
CREATE_FOLDER(PATH_D03)

for i in FILES_D01:
    os.rename(PATH_TO_NETCDFS + i, PATH_D01 + i )

for i in FILES_D02:
    os.rename(PATH_TO_NETCDFS + i, PATH_D02 + i )
        
for i in FILES_D03:
    os.rename(PATH_TO_NETCDFS + i, PATH_D03 + i )




COMMAND_MERGE_NETCDFS='ncecat * -O D01.nc' 
    
    
#EJECUTAMOS EL MODELO 
procces= subprocess.Popen(COMMAND_MERGE_NETCDFS, stdout= subprocess.PIPE, cwd= PATH_D01, shell=True)
output, error = procces.communicate()





















dataset=Dataset('/usr1/uems/runs/spain1/wrfprd/wrfout_d01_2019-07-24_00:00:00')
