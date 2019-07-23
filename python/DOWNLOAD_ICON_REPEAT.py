# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 15:30:54 2019

@author: asus
"""

from subprocess import Popen

filename = 'DOWNLOAD_ICON.py'
while True:
    print("\nStarting " + filename)
    p = Popen("python3 " + filename, shell=True)
    p.wait()