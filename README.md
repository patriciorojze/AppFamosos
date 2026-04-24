
El proyecto muestra un proyecto desarrollado en R con el paquete ShinyMobile, que muestra un directorio de personas con datos de Wikipedia.

Consta de dos partes, divididas en dos carpetas diferentes:

-Proceso: Existen 3 archivos principales: funciones.R, proceso.R y orden.R. Sirven para actualizar la base de datos. A continuación se detalla que hace cada uno:
  -funciones.R: contiene las funciones con la cual se ejecuta el scraping sobre Wikipedia.
  -proceso.R: al ejecutarse, se crean o actualizan los archivos ListaPersonas.RData y ListaReferencias.RData. Registran el historial de búsqueda. 
  -orden.R: al ejecutarse, se crea o actualiza el archivo ListaEdades.RData en la carpeta app. Es el archivo limpio que puede subirse.
Estos dos últimos tienen parámetros que controlar el tiempo que se tarda en ejecutar, entre otras cosas. Se recomienda en una primera ejecución dejarlo un mínimo de 6 horas.

-App: contiene una app hecha en ShinyMobile.
  
