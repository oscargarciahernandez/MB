#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 23 13:12:15 2019

@author: meteobit
"""
import urllib.request
from datetime import date
import time
import os
import numpy as np

def reporthook(a, b, c):
    """
    Report download progress in megabytes
    """
    # ',' at the end of the line is important!
    print("\r % 3.1f%% of %.2f MB\r" % (min(100, float(a * b) / c * 100), c/1000000.), end='')

def download_HRRR(DATE,
                  model='hrrr',
                  field='sfc',
                  hour=range(0, 24),
                  fxx=range(0, 1),
                  OUTDIR='./'):
    """
    Downloads from the University of Utah MesoWest HRRR archive
    Input:
        DATE   - A date object for the model run you are downloading from.
        model  - The model type you want to download. Default is 'hrrr'
                 Model Options are ['hrrr', 'hrrrX','hrrrak']
        field  - Variable fields you wish to download. Default is sfc, surface.
                 Options are fields ['prs', 'sfc','subh', 'nat']
        hour   - Range of model run hours. Default grabs all hours of day.
        fxx    - Range of forecast hours. Default grabs analysis hour (f00).
        OUTDIR - Directory to save the files.
    Outcome:
        Downloads the desired HRRR file and renames with date info preceeding
        the original file name (i.e. 20170101_hrrr.t00z.wrfsfcf00.grib2)
    """
    # Make OUTDIR if path doesn't exist
    if not os.path.exists(OUTDIR):
        os.makedirs(OUTDIR)

    # Loop through each hour and each forecast and download.
    for h in hour:
        for f in fxx:
            # 1) Build the URL string we want to download.
            #    fname is the file name in the format
            #    [model].t[hh]z.wrf[field]f[xx].grib2
            #    i.e. hrrr.t00z.wrfsfcf00.grib2
            fname = "%s.t%02dz.wrf%sf%02d.grib2" % (model, h, field, f)
            URL = "https://pando-rgw01.chpc.utah.edu/%s/%s/%s/%s" \
                   % (model, field, DATE.strftime('%Y%m%d'), fname)

            # 2) Rename file with date preceeding original filename
            #    i.e. 20170105_hrrr.t00z.wrfsfcf00.grib2
            rename = "%s_%s" \
                     % (DATE.strftime('%Y%m%d'), fname)

            # 3) Download the file via https
            # Check the file size, make it's big enough to exist.         
            check_this = urllib.request.urlopen(URL)
            file_size = int(check_this.info()['content-length'])
            if os.path.isfile(OUTDIR+rename):
                print('YA EXISTE ' + OUTDIR+rename + ' SALTAMOS A LA SIGUIENTE')
            else:
                if file_size > 10000:
                    print("Downloading:", URL)
                    urllib.request.urlretrieve(URL, OUTDIR+rename, reporthook)
                    print("\n")
                else:
                    # URL returns an "Key does not exist" message
                    print("ERROR:", URL, "Does Not Exist")
    
                # 4) Sleep five seconds, as a courtesy for using the archive.
                time.sleep(3)

if __name__ == '__main__':

    # Example downloads all analysis hours for a single day.

    # -------------------------------------------------------------------------
    # --- Settings: Check online documentation for available dates and hours --
    # -------------------------------------------------------------------------
    # Start and End Date
    for days in np.arange(1,31):
        get_this_date = date(2019, 6, days)
    
        # Model Type: options include 'hrrr', 'hrrrX', 'hrrrak'
        model_type = 'hrrr'
    
        # Variable field: options include 'sfc' or 'prs'
        # (if you want to initialize WRF with HRRR, you'll need the prs files)
        for sfc_levels in ['sfc', 'prs']:
            var_type = sfc_levels
        
            # Specify which hours to download
            # (this example downloads all hours)
            if model_type == 'hrrrak':
                # HRRR Alaska run every 3 hours at [0, 3, 6, 9, 12, 15, 18, 21] UTC
                hours = range(0, 24, 3)
            else:
                hours = range(0, 24)
        
            # Specify which forecasts hours to download. Most can be range(19) for hrrr.
            # (this example downloads the analysis hours, f00)
            forecasts = [0]
        
            # Specify a Save Directory
            SAVEDIR = '/media/meteobit/Elements/HRRR_from_UofU/'
            
            # -------------------------------------------------------------------------
        
            # Call the function to download
            download_HRRR(get_this_date, model=model_type, field=var_type,
                          hour=hours, fxx=forecasts, OUTDIR=SAVEDIR)