

# En esta clase realizaremos el proyecto de eficiencia/automatización de procesos en R
# Guarda este código, te ayudará bastante en tu aprendizaje


# Abrir RStudio
# Crear archivos
# Guardar archivos
# Abrir archivos
# Ejecutar código
# Comentar código
# Correción de idioma
# Tipografía y temas
# Environment
# Console
# Limpiar Console



# Instalación de librerías
install.packages("readxl")
install.packages("writexl")
install.packages("dplyr")



# Carga de librerías
library(readxl)
library(writexl)
library(dplyr)


#Carga de base de datos:
library(readxl)
X4_Datos <- read_excel("4_Datos.xlsx", sheet = "datos_resumen")
View(X4_Datos)



datos <- X4_Datos

#Vamos a importar desde 
read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/4_DatosIPS.xlsx")

datos_resumen <- read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/4_DatosIPS.xlsx")
#aca tengo la base de datos, pero no la pagina que yo quiero, por ende con el siguiente codigo
#la dejare asignada

datos_resumen <- read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/4_DatosIPS.xlsx", sheet = "datos_resumen")
#ahi asigne la hoja datos_resumen
View(datos_resumen)


library(readxl)




# Variables
sueldo <- 5000  

#A diferencia de Python la asignacion en R es asi <- no asi =
sueldo = 5000   
sueldo    #Dato numerico

mascota <- 'perro' 

#Dato categorico
mascota

print(mascota)



# Vectores
calificaciones <- c(6.3,7.8,9.9,8.3,6.3,6.3)   #Conjunto de valores, el colocar c antes del
                                               #Parentesis de los numeros es que se crea un vector
                                               #A diferencia de antes, y tambien es que en Python
                                               #Es con corchetes y aca en R es con parentesis.

#A continuacion el codigo siguiente me arrrojara un error precisamente porque no inclui el c 
Calficaciones <- c(6.3,7.8,9.9)
calificaciones

animales <- c('perro','gato','conejo')
animales



# Funciones
calificaciones

print(calificaciones)

sum(calificaciones)

mean(calificaciones)
#A diferencia de Python que hay que sumar y despues ocupar la funcion (len) para dividir el total, por 
#el total de numeros, R es mas intuitivo en este aspecto, ya que solo se debe colocar mean = promedio





promedio <- mean(calificaciones)
promedio

#Y aca asigne una variable llamada promedio, y de ahi aplique la funcion promedio en mi vector
# de calificaciones. 


unique(calificaciones)  #Aca R a diferencia de Python en vez de ocupar la funcion set, ocupamos la 
                        #La funcion unique para darnos los valores unicos de
                        #determinado vector, en este caso de calificaciones

paste(sueldo,mascota) #La funcion paste es para concatenar variables en este caso sueldo y mascota
                      #que una viene siendo cuantitativa y la otra categorica

paste0(sueldo,mascota) #El paste0 es para sacar ese espacio en blanco que podemos ver que hay entre estas
                       #Dos variables 

gsub("-","_" ,'barco---mar') #En Python se ocupa replace para cambiar de alguna forma la escritura de
                              # el nombre de un vector, mientras que en R con gsub

file.path('perro', 'gato')



# Lectura de archivos 
# ojo con el sentido de los ///
# la ruta de carga de archivo debe incluir el nombre y extensión del archivo
# library(readxl)
# read_excel("C:/IPSDATAX/2. Cursos/2.9 Curso Introducción a Ciencia de Datos/4. Proyecto 1/4.3 R/X4_datos.xlsx")
# read_excel("C:/IPSDATAX/2. Cursos/2.9 Curso Introducción a Ciencia de Datos/4. Proyecto 1/4.3 R/4_datos.xlsx",sheet="datos_resumen")
# datos <- read_excel("C:/IPS DATAX/2. Cursos/2.9 Curso Introducción a Ciencia de Datos/4. Proyecto 1/4.3 R/4_datos.xlsx",sheet="datos_resumen")

#Esta es la ruta del profe del IPS_DATAX,
#a continuacion lo hare con mi ruta de documentos, ojo en R siempre hay que dar vuelta los slash"\" a"/":


read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/4_DatosIPS.xlsx")
datos_resumen <- read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/4_DatosIPS.xlsx", sheet = "datos_resumen")



read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/4_Datos.xlsx") #Aca con este codigo tengo 
#la base de datos, pero aun no tengo la hoja de datos, de datos resumen, por ende esto hare a continuacion:

datos_resumen <- read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/4_DatosIPS.xlsx" ,sheet= "datos_resumen") 




# Ciclos
calificaciones

for (x in calificaciones) {
  x
}
#Aca 

for (x in calificaciones) {
  print(x)
}
#Siempre se tiene que utilizar print con for para que se pueda ver los resultados 

for (x in calificaciones) {
  print(x+1000)
}
#Lo que pasa aca es que x que tiene sus determinados valores de calificaciones le sumamos a mil a cada
#una de estas calificaciones.

for (x in unique(calificaciones)) {
  print(x)
}



# Procesamiento de datos
# library(dplyr)
datos  #Aca estan los datos de la hoja de datos resumen y por ende puedo ver los vendedores que estan en
#Esta hoja

datos_resumen %>% filter(Vendedor == "Luis")
  
  #Aca ocupamos la hoja de datos_resumen, pero filtramos con luis, el 
#pipe es una operacion adicional ejemplo: %>% filter, osea agarramos datos con ayuda de pipe filtramos
#la variable que queremos y en este caso a la persona que vendria siendo luis
datos_resumen %>% filter(Vendedor == "Ana") #Lo mismo aplica aca
datos_ana <- datos_resumen %>% filter(Vendedor == "Ana") #Aca asignaremos datos_ana para crear un nuevo vector
#que contenga los datos de de an y queden guardados en la data

datos_ana

datos_ana
datos_ana['Producto'] #Asi se utiliza a menudo en Python
datos_ana$Producto    #Asi se utiliza a menudo en R

datos_resumen %>% filter(Vendedor == "Ana")




#El simbolo peso es para asignar una columna que queremos ver, si lo colocar con corchetes y con comillas 
#ahi me data de  distinta manera el resultado 

#tambien podriamos hacer uno de luis;

datos_luis <- datos_resumen %>% filter(Vendedor == "Luis")
datos_luis


# exportar excel     

#Como podemos exportar los datos de ana a Excel? De la siguiente manera:


install.packages("writexl")
# library(writexl)
library(writexl)
write_xlsx(datos_ana, path = "D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/datos_ana.xlsx")

#De esta manera podemos traspasar datos selecitvos que trajimos desde excel para despues estructurarlos
#con el cliente que queremos  

#tambien podriamos hacer uno de luis;

datos_luis <- datos_resumen %>% filter(Vendedor == "Luis")
datos_luis
write_xlsx(datos_luis, path = "D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/datos_luis.xlsx")

#Ocupo la funcion write_xlsx, de ahi datos_ana, coloco path, que es ruta; y doy de ahi mi ruta de documentos
#donde se encuentra la base de datos de excel 4_Datos.xlsx







# REVISIÓN DEL PROYECTO

# Definir la ruta donde está el proyecto
ruta_base <- "D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/"



# Unimos la ruta base con nombre del archivo
archivo_ventas <- file.path(ruta_base, "4_DatosIPS.xlsx")
#Aca estamos asignando archivo ventas con la ruta base de la base de datos de 4_Datos.xlsx
archivo_ventas

# Leemos el archivo Excel
ventas <- read_excel(archivo_ventas, sheet="datos_resumen")
ventas
#Ahora asignaremos a ventas que lea el archivo excel, y que vaya a la paigna de datos_resumen
#, ahora a datos
#resumen como lo habiamos hecho antes en el script.

# Obtenemos los vendedores únicos
vendedores <- unique(ventas$Vendedor)
vendedores
#Aca podemos corroborar que tenemos todos los datos de los vendedores, que estan en "datos_resumen"


# Recorremos 
for (v in vendedores) {
  
  #"v" va a tomar cada nombre de cada vendedor, es decir de ana, carla, luis y tomas, y va a realizar
  #todo el siguiente procedimiento por cada una de estas personas. Entoces la v se va a convertir en cada
  #uno de estos nombres y repetira el ciclo. 
  
  # Filtramos por el vendedor del ciclo
  ventas_filtradas <- ventas %>% filter(Vendedor == v)
  
  #Entonces al principio en ventas vamos a hacer una operacion con el comando pipe para filtrar donde
  #vendedor sea igual a v, entonces a la data ventas le vamos a filtrar donde el vendedor sea igual a v
  #en estos ana, carla, luis y tomas 
  
  #Y filtraremos las ventas por los vendedores que tenemos en la hoja de datos_resumen
  
  # Creamos el nombre del archivo utilizando el nombre del vendedor y sin espacios
  nombre_archivo <- paste0(gsub(" ", "_", v), ".xlsx")
  
  #Luego vamos a crear el nombre de archivo, vamos a unir todos los textos, pero los textos
  #que estan de gsub en adelante, entoces gsub lo que hace es reemplazar valores, entonces vamos
  #a tomar v que en este caso es ana (como es la primera iteracion del ciclo) y va a reemplazar los 
  #espacios en blanco por guion bajo, para ana en estricto rigor no aplica, pero si aplica para la hoja
  #de datos que contiene todos los nombres ya apellidos del df. 
  
  # Creamos la ruta de salida (carpeta + nombre archivo)
  ruta_salida <- file.path(ruta_base, nombre_archivo)
  
  # Exportamos el archivo en su respectiva ruta
  write_xlsx(ventas_filtradas, path = ruta_salida)
}

#Ahi me dejo creado/exportado en hojas deexcel de cada persona: Luis, tomas, ana, carla en nuestra
#ruta base (arriba). 
#-------------------------------------------------------------------------------

#Ahora haremos exactamente lo mismo pero solo que con la diferencia que ahoralo haremos con la hoja
#de datos y no de datos_resumen

# Definir la ruta donde está el proyecto
ruta_base <- "D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/"

# Unimos la ruta base con nombre del archivo
archivo_ventas <- file.path(ruta_base, "4_DatosIPS.xlsx")


# Leemos el archivo Excel
ventas <- read_excel(archivo_ventas, sheet="datos")
ventas

vendedores <- unique(ventas$Vendedor)
vendedores
vendedores
#Ahora podemos darnos cuenta que estan todos los vendedores de la hoja "datos"
ventas
ventas
for (v in vendedores)


# Filtramos por el vendedor del ciclo
ventas_filtradas <- ventas %>% filter(Vendedor == v)
ventas_filtradas

# Creamos el nombre del archivo utilizando el nombre del vendedor y sin espacios
nombre_archivo <- paste0(gsub(" ", "_", v), ".xlsx")


ruta_salida <- file.path(ruta_base, nombre_archivo)

# Exportamos el archivo en su respectiva ruta
write_xlsx(ventas_filtradas, path = ruta_salida)




#-------------------------------------------------------------------------------


#Entonces al principio en ventas vamos a hacer una operacion con el comando pipe para filtrar donde
#vendedor sea igual a v, entonces a la data ventas le vamos a filtrar donde el vendedor sea igual a v

#Y filtraremos las ventas por los vendedores que tenemos en la hoja de datos

# Creamos el nombre del archivo utilizando el nombre del vendedor y sin espacios

#Luego vamos a crear el nombre de archivo, vamos a unir todos los textos, pero los textos
#que estan de gsub en adelante, entoces gsub lo que hace es reemplazar valores, entonces vamos
#a tomar v que en este caso es ana (como es la primera iteracion del ciclo) y va a reemplazar los 
#espacios en blanco por guion bajo, para ana en estricto rigor no aplica, pero si aplica para la hoja
#de datos que contiene todos los nombres ya apellidos del df. 

# Creamos la ruta de salida (carpeta + nombre archivo)


#1)

ruta_base <- "D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/"

archivo_ventas <- file.path(ruta_base, "4_DatosIPS.xlsx")

ventas <- read_excel(archivo_ventas, sheet="datos_resumen")

vendedores <- unique(ventas$Vendedor)
for (v in vendedores) {
  
  #"v" va a tomar cada nombre de cada vendedor, es decir de ana, carla, luis y tomas, y va a realizar
  #todo el siguiente procedimiento por cada una de estas personas. Entoces la v se va a convertir en cada
  #uno de estos nombres y repetira el ciclo. 
  
  # Filtramos por el vendedor del ciclo
  ventas_filtradas <- ventas %>% filter(Vendedor == v)
  
  #Entonces al principio en ventas vamos a hacer una operacion con el comando pipe para filtrar donde
  #vendedor sea igual a v, entonces a la data ventas le vamos a filtrar donde el vendedor sea igual a v
  #en estos ana, carla, luis y tomas 
  
  #Y filtraremos las ventas por los vendedores que tenemos en la hoja de datos_resumen
  
  # Creamos el nombre del archivo utilizando el nombre del vendedor y sin espacios
  nombre_archivo <- paste0(gsub(" ", "_", v), ".xlsx")
  
  # Creamos la ruta de salida (carpeta + nombre archivo)
  ruta_salida <- file.path(ruta_base, nombre_archivo)
  
  # Exportamos el archivo en su respectiva ruta
  write_xlsx(ventas_filtradas, path = ruta_salida)
}



#2

# Definir la ruta donde está el proyecto
ruta_base <- "D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/"

# Unimos la ruta base con nombre del archivo
archivo_ventas <- file.path(ruta_base, "4_DatosIPS.xlsx")


# Leemos el archivo Excel
ventas <- read_excel(archivo_ventas, sheet="datos")
ventas

vendedores <- unique(ventas$Vendedor)
vendedores
vendedores
#Ahora podemos darnos cuenta que estan todos los vendedores de la hoja "datos"
ventas
ventas
for (v in vendedores) {
  
  
  # Filtramos por el vendedor del ciclo
  ventas_filtradas <- ventas %>% filter(Vendedor == v)
ventas_filtradas

# Creamos el nombre del archivo utilizando el nombre del vendedor y sin espacios
nombre_archivo <- paste0(gsub(" ", "_", v), ".xlsx")


ruta_salida <- file.path(ruta_base, nombre_archivo)

# Exportamos el archivo en su respectiva ruta
write_xlsx(ventas_filtradas, path = ruta_salida)
}


#Ojo con los corchetes. 