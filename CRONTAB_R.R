library(here)
source('libraries.R')
sudo<- 'echo anderroman | sudo -S sudo'

#IMPORT AEMET Y SUBIR TODO AL FTP + CREAR PROVINCIAS
system(paste('cd /home/asus/Escritorio/Programas/meteoimport &&', 
             sudo,
             'python import-aemet.py &&',
             sudo,
             'wget --spider http://www.triskel-meteo.com/demo/sqlfiles/importer.php &&',
             sudo,
             'wget --spider http://triskel-meteo.com/crear_provincias.php '))

#GFS IMPORT PARA GENERAR SPAIN Y WORLD.SQL
system(paste('cd /home/asus/Escritorio/Programas/meteoimport &&', 
             sudo,
             'python import-gfs.py  &&',
             sudo,
             'wget --spider http://www.triskel-meteo.com/demo/sqlfiles/importer-gfs.php'))

#GFS IMPORT PARA GENERAR SPAIN Y WORLD.SQL
DATE_HOY<- now() %>% as.Date() %>% as.character() %>% str_remove_all("-")
Sys.setenv('PATH="/usr1//uems/strc/Ubin:/usr1//uems/domwiz/bin:/usr1//uems/bin:/usr1//uems/util/grads/bin:/usr1//uems/util/bin:/usr1//uems/util/grads/data:/usr1//uems/util/mpich2/bin:/usr1//uems/util/ncview/bin:/usr1//uems/util/HDFView/bin:.:/usr1//uems/strc/Ubin:/usr1//uems/domwiz/bin:/usr1//uems/bin:/usr1//uems/util/grads/bin:/usr1//uems/util/bin:/usr1//uems/util/grads/data:/usr1//uems/util/mpich2/bin:/usr1//uems/util/ncview/bin:/usr1//uems/util/HDFView/bin:/home/asus/bin:/home/asus/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/usr/local/wgrib2:/usr/local/wgrib2:/snap/bin"')
system(paste('/bin/bash -c export PATH=$PATH:/usr1//uems/strc/Ubin:/usr1//uems/domwiz/bin:/usr1//uems/bin:/usr1//uems/util/grads/bin:/usr1//uems/util/bin:/usr1//uems/util/grads/data:/usr1//uems/util/mpich2/bin:/usr1//uems/util/ncview/bin:/usr1//uems/util/HDFView/bin:.:/usr1//uems/strc/Ubin:/usr1//uems/domwiz/bin:/usr1//uems/bin:/usr1//uems/util/grads/bin:/usr1//uems/util/bin:/usr1//uems/util/grads/data:/usr1//uems/util/mpich2/bin:/usr1//uems/util/ncview/bin:/usr1//uems/util/HDFView/bin:/home/asus/bin:/home/asus/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/usr/local/wgrib2:/usr/local/wgrib2:/snap/bin && cd /usr1/uems/etc && ./EMS.profile  && cd /usr1/uems/runs/spain1 &&
             ./ems_autorun --dset gfsp25 --date ', 
             DATE_HOY, '--cycle 00:00:72 && cd /home/asus/MB/ && ./CSV_Spain'))

system('echo $PATH')
0 12 * * 1,4  cp -r /usr1/uems/runs/spain1/grib/* /usr1/uems/runs/europe1/grib/ && cd /usr1/uems/runs/europe1/ && ./ems_autorun --dset gfsp25 --date `date +"\%Y\%m\%d"` -- cycle 00:00:72 


40 11  * * * cd /usr1/uems/runs/spain1 && ./ems_autorun --noscour --dset gfsp25 --date `date +"\%Y\%m\%d"`  --cycle 00:00:48 --domain 1,2  && mkdir /home/asus/dominios12/`date +"\%Y\%m\%d"` && cp -R wrfprd/ /home/asus/dominios12/`date +"\%Y\%m\%d"` > /var/log/myjob.log 2>&1
