def _LecturaWRF_V10_VN5_VN7_VN9_T02_ARW(Direccion_ArchivosWRF,fechaInicio):
	from numpy import array
	from numpy import transpose
	from numpy import pi
	from netCDF4 import Dataset
	import datetime, time
	Nombre_Parque='ARW'
	Longitud_Parque=-3.819
	Latitud_Parque=40.437
	Direccion_Resultados='/home/asus/Escritorio/Programas/wrf'
	lenght=24
	fu=open('%s\WRF_u10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fuN5=open('%s\WRF_uN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fuN7=open('%s\WRF_uN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fuN9=open('%s\WRF_uN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fv=open('%s\WRF_v10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fvN5=open('%s\WRF_vN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fvN7=open('%s\WRF_vN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fvN9=open('%s\WRF_vN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fMV=open('%s\WRF_MV10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fMVN5=open('%s\WRF_MVN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fMVN7=open('%s\WRF_MVN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fMVN9=open('%s\WRF_MVN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fT=open('%s\WRF_T02_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
	fu=open('%s\WRF_u10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fuN5=open('%s\WRF_uN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fuN7=open('%s\WRF_uN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fuN9=open('%s\WRF_uN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fv=open('%s\WRF_v10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fvN5=open('%s\WRF_vN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fvN7=open('%s\WRF_vN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fvN9=open('%s\WRF_vN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fMV=open('%s\WRF_MV10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fMVN5=open('%s\WRF_MVN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fMVN7=open('%s\WRF_MVN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fMVN9=open('%s\WRF_MVN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	fT=open('%s\WRF_T02_%s.txt'%(Direccion_Resultados,Nombre_Parque),'r')
	lu=fu.readlines()
	luN5=fuN5.readlines()
	luN7=fuN7.readlines()
	luN9=fuN9.readlines()
	lv=fv.readlines()
	lvN5=fvN5.readlines()
	lvN7=fvN7.readlines()
	lvN9=fvN9.readlines()
	lMV=fMV.readlines()
	lMVN5=fMVN5.readlines()
	lMVN7=fMVN7.readlines()
	lMVN9=fMVN9.readlines()
	lT=fT.readlines()
	if len(lu)==len(lv) and len(lu)==len(lMV) and len(lu)==len(lT) and len(lu)==len(luN5) and len(lu)==len(luN7) and len(lu)==len(lvN5) and len(lu)==len(lvN7) and len(lu)==len(lMVN5) and len(lu)==len(lMVN7) and len(lu)==len(luN9) and len(lu)==len(lvN9):
		#Fecha inicial con formato pasada por parametro.
		Fecha_Actual = datetime.datetime.strptime(fechaInicio,'%Y%m%d')
		Ano_Actual=Fecha_Actual.year
		Mes_Actual=Fecha_Actual.month
		Dia_Actual=Fecha_Actual.day
		Hora_Referencia=0
		Fecha_Actual_00=datetime.datetime(Ano_Actual,Mes_Actual,Dia_Actual,Hora_Referencia)
		Fecha_Actualizada=Fecha_Actual_00+datetime.timedelta(days=1)
		fu=open('%s\WRF_u10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fuN5=open('%s\WRF_uN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fuN7=open('%s\WRF_uN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fuN9=open('%s\WRF_uN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fv=open('%s\WRF_v10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fvN5=open('%s\WRF_vN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fvN7=open('%s\WRF_vN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fvN9=open('%s\WRF_vN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fMV=open('%s\WRF_MV10_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fMVN5=open('%s\WRF_MVN5_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fMVN7=open('%s\WRF_MVN7_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fMVN9=open('%s\WRF_MVN9_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		fT=open('%s\WRF_T02_%s.txt'%(Direccion_Resultados,Nombre_Parque),'a')
		for l in range(lenght):
			Nombre_Fichero_WRF=str(Fecha_Actualizada.strftime("wrfout_d01_%Y-%m-%d_%H_%M_%S"))
			Fecha_Formato=str(Fecha_Actualizada.strftime("%d.%m.%Y.%H.%M"))
			#Indicar la ruta de los WRF.
			data=Dataset('%s\%s'%(Direccion_ArchivosWRF,Nombre_Fichero_WRF))
			#print data
			if l==0:
				Lon=data.variables['XLONG'][0]
				Lat=data.variables['XLAT'][0]
				N_Lon=len(transpose(Lon))
				N_Lat=len(Lat)
				for i in range(1,(N_Lat-1)):
					stop=0
					for j in range(1,(N_Lon-1)):
						if Lon[i,j-1]<Longitud_Parque and Lon[i,j+1]>=Longitud_Parque and Lat[i-1,j]<Latitud_Parque and Lat[i+1,j]>=Latitud_Parque:
							m=i
							n=j
							stop=1
							break
					if stop==1:break
			u=data.variables['U10'][0][:]
			uN5=data.variables['U10'][0][:]
			uN7=data.variables['U10'][0][:]
			uN9=data.variables['RAINNC'][0][:]
			#uN5=data.variables['U'][0][5]
			#uN7=data.variables['U'][0][7]
			#uN9=data.variables['U'][0][9]
			v=data.variables['V10'][0][:]
			vN5=data.variables['V10'][0][:]
			vN7=data.variables['V10'][0][:]
			vN9=data.variables['RAINC'][0][:]
			#vN5=data.variables['V'][0][5]
			#vN7=data.variables['V'][0][7]
			#vN9=data.variables['V'][0][9]
			Modulo_Viento=(u**2+v**2)**0.5
			Modulo_VientoN5=(uN5**2+vN5**2)**0.5
			Modulo_VientoN7=(vN9)*1000
			Modulo_VientoN9=(uN9)*1000
			Tmax=(data.variables['T02_MAX'][0][:])-273.16
			Tmin=(data.variables['T02_MIN'][0][:])-273.16
			T_media=(Tmax+Tmin)/2.
			if len(lu)==0 and l==0:
				fu.write('%s\t'%('coordenadas'))
				fuN5.write('%s\t'%('coordenadas'))
				fuN7.write('%s\t'%('coordenadas'))
				fuN9.write('%s\t'%('coordenadas'))
				fv.write('%s\t'%('coordenadas'))
				fvN5.write('%s\t'%('coordenadas'))
				fvN7.write('%s\t'%('coordenadas'))
				fvN9.write('%s\t'%('coordenadas'))
				fMV.write('%s\t'%('coordenadas'))
				fMVN5.write('%s\t'%('coordenadas'))
				fMVN7.write('%s\t'%('coordenadas'))
				fMVN9.write('%s\t'%('coordenadas'))
				fT.write('%s\t'%('coordenadas'))
				for hm in range((m-50),((m+43)+1)):
					for hn in range((n-35),((n+40)+1)):
						fu.write('%1.4f\t'%(Lon[hm,hn]))
						fuN5.write('%1.4f\t'%(Lon[hm,hn]))
						fuN7.write('%1.4f\t'%(Lon[hm,hn]))
						fuN9.write('%1.4f\t'%(Lon[hm,hn]))
						fv.write('%1.4f\t'%(Lon[hm,hn]))
						fvN5.write('%1.4f\t'%(Lon[hm,hn]))
						fvN7.write('%1.4f\t'%(Lon[hm,hn]))
						fvN9.write('%1.4f\t'%(Lon[hm,hn]))
						fMV.write('%1.4f\t'%(Lon[hm,hn]))
						fMVN5.write('%1.4f\t'%(Lon[hm,hn]))
						fMVN7.write('%1.4f\t'%(Lon[hm,hn]))
						fMVN9.write('%1.4f\t'%(Lon[hm,hn]))
						fT.write('%1.4f\t'%(Lon[hm,hn]))
				fu.write('\n')
				fuN5.write('\n')
				fuN7.write('\n')
				fuN9.write('\n')
				fv.write('\n')
				fvN5.write('\n')
				fvN7.write('\n')
				fvN9.write('\n')
				fMV.write('\n')
				fMVN5.write('\n')
				fMVN7.write('\n')
				fMVN9.write('\n')
				fT.write('\n')		
				fu.write('%s\t'%('fechas'))
				fuN5.write('%s\t'%('fechas'))
				fuN7.write('%s\t'%('fechas'))
				fuN9.write('%s\t'%('fechas'))
				fv.write('%s\t'%('fechas'))
				fvN5.write('%s\t'%('fechas'))
				fvN7.write('%s\t'%('fechas'))
				fvN9.write('%s\t'%('fechas'))
				fMV.write('%s\t'%('fechas'))
				fMVN5.write('%s\t'%('fechas'))
				fMVN7.write('%s\t'%('fechas'))
				fMVN9.write('%s\t'%('fechas'))
				fT.write('%s\t'%('fechas'))
				for hm in range((m-50),((m+43)+1)):
					for hn in range((n-35),((n+40)+1)):
						fu.write('%1.4f\t'%(Lat[hm,hn]))
						fuN5.write('%1.4f\t'%(Lat[hm,hn]))
						fuN7.write('%1.4f\t'%(Lat[hm,hn]))
						fuN9.write('%1.4f\t'%(Lat[hm,hn]))
						fv.write('%1.4f\t'%(Lat[hm,hn]))
						fvN5.write('%1.4f\t'%(Lat[hm,hn]))
						fvN7.write('%1.4f\t'%(Lat[hm,hn]))
						fvN9.write('%1.4f\t'%(Lat[hm,hn]))
						fMV.write('%1.4f\t'%(Lat[hm,hn]))
						fMVN5.write('%1.4f\t'%(Lat[hm,hn]))
						fMVN7.write('%1.4f\t'%(Lat[hm,hn]))
						fMVN9.write('%1.4f\t'%(Lat[hm,hn]))
						fT.write('%1.4f\t'%(Lat[hm,hn]))
				fu.write('\n')
				fuN5.write('\n')
				fuN7.write('\n')
				fuN9.write('\n')
				fv.write('\n')
				fvN5.write('\n')
				fvN7.write('\n')
				fvN9.write('\n')
				fMV.write('\n')
				fMVN5.write('\n')
				fMVN7.write('\n')
				fMVN9.write('\n')
				fT.write('\n')
				fu.write('%s\t'%(Fecha_Formato))
				fuN5.write('%s\t'%(Fecha_Formato))
				fuN7.write('%s\t'%(Fecha_Formato))
				fuN9.write('%s\t'%(Fecha_Formato))
				fv.write('%s\t'%(Fecha_Formato))
				fvN5.write('%s\t'%(Fecha_Formato))
				fvN7.write('%s\t'%(Fecha_Formato))
				fvN9.write('%s\t'%(Fecha_Formato))
				fMV.write('%s\t'%(Fecha_Formato))
				fMVN5.write('%s\t'%(Fecha_Formato))
				fMVN7.write('%s\t'%(Fecha_Formato))
				fMVN9.write('%s\t'%(Fecha_Formato))
				fT.write('%s\t'%(Fecha_Formato))
				for hm in range((m-50),((m+43)+1)):
					for hn in range((n-35),((n+40)+1)):
						fu.write('%1.2f\t'%(u[hm,hn]))
						fuN5.write('%1.2f\t'%(uN5[hm,hn]))
						fuN7.write('%1.2f\t'%(uN7[hm,hn]))
						fuN9.write('%1.2f\t'%(uN9[hm,hn]))
						fv.write('%1.2f\t'%(v[hm,hn]))
						fvN5.write('%1.2f\t'%(vN5[hm,hn]))
						fvN7.write('%1.2f\t'%(vN7[hm,hn]))
						fvN9.write('%1.2f\t'%(vN9[hm,hn]))
						fMV.write('%1.2f\t'%(Modulo_Viento[hm,hn]))
						fMVN5.write('%1.2f\t'%(Modulo_VientoN5[hm,hn]))
						fMVN7.write('%1.2f\t'%(Modulo_VientoN7[hm,hn]))
						fMVN9.write('%1.2f\t'%(Modulo_VientoN9[hm,hn]))
						fT.write('%1.1f\t'%(T_media[hm,hn]))
				fu.write('\n')
				fuN5.write('\n')
				fuN7.write('\n')
				fuN9.write('\n')
				fv.write('\n')
				fvN5.write('\n')
				fvN7.write('\n')
				fvN9.write('\n')
				fMV.write('\n')
				fMVN5.write('\n')
				fMVN7.write('\n')
				fMVN9.write('\n')
				fT.write('\n')
			else:
				fu.write('%s\t'%(Fecha_Formato))
				fuN5.write('%s\t'%(Fecha_Formato))
				fuN7.write('%s\t'%(Fecha_Formato))
				fuN9.write('%s\t'%(Fecha_Formato))
				fv.write('%s\t'%(Fecha_Formato))
				fvN5.write('%s\t'%(Fecha_Formato))
				fvN7.write('%s\t'%(Fecha_Formato))
				fvN9.write('%s\t'%(Fecha_Formato))
				fMV.write('%s\t'%(Fecha_Formato))
				fMVN5.write('%s\t'%(Fecha_Formato))
				fMVN7.write('%s\t'%(Fecha_Formato))
				fMVN9.write('%s\t'%(Fecha_Formato))
				fT.write('%s\t'%(Fecha_Formato))
				for hm in range((m-50),((m+43)+1)):
					for hn in range((n-35),((n+40)+1)):
						fu.write('%1.2f\t'%(u[hm,hn]))
						fuN5.write('%1.2f\t'%(uN5[hm,hn]))
						fuN7.write('%1.2f\t'%(uN7[hm,hn]))
						fuN9.write('%1.2f\t'%(uN9[hm,hn]))
						fv.write('%1.2f\t'%(v[hm,hn]))
						fvN5.write('%1.2f\t'%(vN5[hm,hn]))
						fvN7.write('%1.2f\t'%(vN7[hm,hn]))
						fvN9.write('%1.2f\t'%(vN9[hm,hn]))
						fMV.write('%1.2f\t'%(Modulo_Viento[hm,hn]))
						fMVN5.write('%1.2f\t'%(Modulo_VientoN5[hm,hn]))
						fMVN7.write('%1.2f\t'%(Modulo_VientoN7[hm,hn]))
						fMVN9.write('%1.2f\t'%(Modulo_VientoN9[hm,hn]))
						fT.write('%1.1f\t'%(T_media[hm,hn]))
				fu.write('\n')
				fuN5.write('\n')
				fuN7.write('\n')
				fuN9.write('\n')
				fv.write('\n')
				fvN5.write('\n')
				fvN7.write('\n')
				fvN9.write('\n')
				fMV.write('\n')
				fMVN5.write('\n')
				fMVN7.write('\n')
				fMVN9.write('\n')
				fT.write('\n')
			Fecha_Actualizada=Fecha_Actualizada+datetime.timedelta(hours=1)
		fu.close()
		fuN5.close()
		fuN7.close()
		fuN9.close()
		fv.close()
		fvN5.close()
		fvN7.close()
		fvN9.close()
		fMV.close()
		fMVN5.close()
		fMVN7.close()
		fMVN9.close()
		fT.close()
		return
	else:return 'ERROR: El numero de filas o fechas de las matrices de los predictores no coincide. No se puede preparar el Post-Proceso.'
def recorridoHistorico(direccion,fechaInicio,fechaFin):
	import os
	import re
	directorio = []
	directorioTramo = []
	for root,dirs,files in os.walk(direccion):
		#Coincidencias de todos los directorios que corresponden a dias.
		if re.match('.*(\d{8})$',root):
			directorio.append(root)
	#Ordenamiento cronologico de dias.
	directorio.sort()
	for item in directorio:
		if os.path.basename(item)==fechaInicio:
			inicio=directorio.index(item)
		if os.path.basename(item)==fechaFin:
			fin=directorio.index(item)
			directorioTramo = directorio[inicio:fin+1]
			break
	for item in directorioTramo:
		os.chdir(item)
		_LecturaWRF_V10_VN5_VN7_VN9_T02_ARW(item,os.path.basename(item))
