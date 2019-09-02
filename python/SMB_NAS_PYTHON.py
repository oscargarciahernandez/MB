#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 29 16:57:40 2019

@author: meteobit
"""
from smb.SMBConnection import SMBConnection

conn = SMBConnection('admin', 'meteobit', 'local_NetBIOS_name', 'remote_NetBIOS_name')
conn.connect('ip_address')
results = conn.listPath('share_name', '/optionally/some/subfolder')

for x in results:
    print x.filename

