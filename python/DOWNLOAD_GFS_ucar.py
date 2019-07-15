#!/usr/bin/env python
#################################################################

import requests
from bs4 import BeautifulSoup

    
URL_BUSQUEDA= 'http://ems3.comet.ucar.edu/data/grib/gfsp25/'

#INICIAMOS SESION REQUEST
session = requests.Session()
session.trust_env = False
user_agents= session.get(URL_BUSQUEDA)

#PARSEANDO HTML
user_soup = BeautifulSoup(user_agents.content, 'html.parser')
user_soup1=user_soup.find_all('a')

user_soup1[10].text

