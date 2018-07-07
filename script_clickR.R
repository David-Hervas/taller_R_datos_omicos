###############################
#Utilización del paquete clickR
###############################

#El paquete clickR se ha diseñado para facilitar el análisis de datos en 
#investigación biomédica. Proporciona herramientas para la revisión de la
#calidad de los datos, corrección de errores en los mismos y generación de
#informes a partir de los resultados de análisis estadístico.

#Carga del paquete y de datos de ejemplo
library(clickR)
datos_mirna_raw <- read.csv2("datos_miRNAs.csv")

#Visualización de los datos
names(datos_mirna_raw)[1:50]
datos_mirna <- nice_names(datos_mirna_raw)  #Nombres estandarizados
names(datos_mirna)[1:50]
descriptive(datos_mirna[,1:13])  #Variables 1-13, parte clínica. El resto miRNAs.
mine.plot(datos_mirna[,1:13])  #Mapa de valores faltantes
mine.plot(datos_mirna[,1:13], what="x==0")  #Podemos usar cualquier expresión lógica
mine.plot(datos_mirna[,1:13], what="x>20 & x<30")

#Control de calidad
check_quality(datos_mirna$sexo)  #Variable categórica con niveles erróneos
check_quality(datos_mirna$progresion)  #Variable categórica con nivels erróneos
check_quality(datos_mirna$slp_meses)  #Variable numérica codificada como factor
check_quality(datos_mirna$f_inclusion)  #Variable de fecha codificada como factor
check_quality(datos_mirna$ecog)  #Variable categórica codificada como numérica

#Arreglar factores
datos_mirna <- fix.factors(datos_mirna)
check_quality(datos_mirna$sexo)
check_quality(datos_mirna$progresion)

#Arreglar fechas
datos_mirna <- fix.dates(datos_mirna)
check_quality(datos_mirna$f_nacimiento)
check_quality(datos_mirna$f_inclusion)
#Ejemplo más extremo:
mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
                   Dates2=c("01/01/85", "04/04/1982", "07/12-2016", NA),
                   Numeric1=rnorm(4))
mydata
fix.dates(mydata)

#Arreglar variables numéricas
datos_mirna <- fix.numerics(datos_mirna)
check_quality(datos_mirna$slp_meses)

descriptive(datos_mirna[,1:13])

#Descriptivo formato publicación
report(datos_mirna[,2:13])
report(datos_mirna[,2:13], by="ecog", file="descriptivo")  #Exportación como Word
report(datos_mirna[,2:13], by="ecog", file="descriptivo", type="latex")  #O latex
report(datos_mirna[,2:13], by="ecog", file="descriptivo", type="csv")  #O csv