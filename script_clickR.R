###############################
#Utilización del paquete clickR
###############################

#El paquete clickR se ha diseñado para facilitar el análisis de datos en 
#investigación biomédica. Proporciona herramientas para la revisión de la
#calidad de los datos, corrección de errores en los mismos y generación de
#informes a partir de los resultados de análisis estadístico.

#Carga del paquete y de datos de ejemplo
library(clickR)
datos_mirna <- read.csv2("datos_miRNAs.csv")

#Visualización de los datos
names(datos_mirna)[1:50]
descriptive(datos_mirna[,1:14])

