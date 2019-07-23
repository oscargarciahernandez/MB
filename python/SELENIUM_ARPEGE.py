# -*- coding: utf-8 -*-
"""
ESTE SCRIPT ES PARA DESCARGAR LA INFORMACION DEL MODELO ARPEGE 
HAY QUE CONFIGURAR EL PATH DE DESCARGA 
"""





from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
import os
import csv
import datetime
import os
import numpy as np
import zipfile
import re
import time


PATH_ARPEGE= '/home/asus/MB/python/ARPEGE/'

profile = webdriver.FirefoxProfile()
profile.set_preference('browser.download.folderList', 2) 
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference('browser.download.dir', PATH_ARPEGE)
profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/zip')
profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/download')
profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/octet-stream')

    
def main():
    driver=webdriver.Firefox(profile)
    url= "https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=130&id_rubrique=51"
    driver.get(url)
    
    #CONSEGUIR FECHAS DISPONIBLES    
    DATE= driver.find_element_by_id('reftimeselect')
    TXT_DATE= DATE.text.split("\n")
    
    
    LOG_DATES= []
    file='ARPEGE_LOG'
    with open(file, 'rt') as myfile:
         wx = csv.reader(myfile)
         for x in wx:
             LOG_DATES.append(x)
    
    
    #SELECCIONAMOS SOLO LA SIMULACION 0
    for Av_dates in [item for item in TXT_DATE if int(item.split(' ')[-2])==0]: 
        if Av_dates not in str(LOG_DATES):
           
            for HP in [1,2]:              
                for hours in [1,2]:
                    #ENTRAMOS A LA PAGINA EN CADA BUCLE... PORQUE 
                    driver.get(url)
                    #SELECCIONAMOS DESCARGAR EL MUNDO
                    WebDriverWait(driver, 30).until(EC.visibility_of_element_located((By.CSS_SELECTOR, '#gridselect > option:nth-child(2)'))).click()
                    
            
            
                    # SELECCIONAMOS HP1-- VARIABLES CORRIENTES Y ADICIONALES                    
                    driver.find_element_by_css_selector('#packageselect > option:nth-child(' + str(HP) + ')').click()

                    #SELECCIONAMOS 0-24 H Y 0-48
                    driver.find_element_by_css_selector('#timeselect > option:nth-child(' + str(hours) + ' )').click()
                    
        
                    # SELECIONAMOS DIA Y HORA SIMULACION
                    driver.find_element_by_xpath("//*[contains(text(), '" + str(Av_dates) +"' )]").click() 
                    
                    
                    #PULSAMOS BOTON DE DESCARGA
                    driver.find_element_by_css_selector('input.publication-btn').click()
                   
                    time.sleep(15)
                    while True:
                        FILES= os.listdir('ARPEGE')
                        if '.part' in str(FILES):
                            time.sleep(30)
                        else: 
                            break
                         
                        

                        
                #time.sleep(900)
                    #VOLVEMOS A LA PAGINA ANTERIOR
                    #driver.execute_script("window.history.go(-1)")
                    
                    
        
            #APUNTAMOS DESCARGA EN LOG ARPEGE
            with open(file, "a") as myfile:
                myfile.write('\n' + Av_dates)
    
    
    
    driver.close()
    driver.quit()
    


if __name__ == '__main__':
    main()
    
    
