#####################################################
#Análisis básicos: Regresión lineal y no paramétricos
#####################################################

#Datos ejemplo en R (mtcars)
descriptive(mtcars)
mtcars <- fix.factors(mtcars)
descriptive(mtcars)

#Test t es igual a regresión lineal
t.test(mpg ~ am, data=mtcars, var.equal=TRUE)

lm1 <- lm(mpg ~ am, data=mtcars)
report(lm1, digits = 5, digitspvals = 6)

#Modelo lineal sobre un único miRNA
lm2 <- lm(hsa_let_7a_3p ~ respuesta, data=datos_mirna)
report(lm2)
#P-valor
summary(lm2)$coeff[2, 4]

#Modelo lineal controlando por batch effect
lm3 <- lm(hsa_let_7a_3p ~ respuesta + batch, data=datos_mirna)
report(lm3)


#Aplicación de modelo lineal a todos los miRNAs
library(pbapply)
#Usamos la función pbsapply, y nos quedamos tanto con el p-valor como con el coeficiente
results_lm <- pbsapply(datos_mirna[,14:6622], function(x){
  summary(lm(x ~ datos_mirna$respuesta + datos_mirna$batch))$coeff[2,c(1,4)]
})

#Obtenemos una matriz. La primera fila es el coeficiente y la segunda es el p-valor
dim(results_lm)

#Resultados significativos
table(results_lm[2,] < 0.05)

#Lista de p-valores significativos
results_lm[2,][results_lm[2,] < 0.05]

#Coeficientes que han tenido un p-valor significativo
results_lm[1, results_lm[2,] < 0.05]

#Ajuste de los p-valores con FDR
results_adjusted <- p.adjust(results_lm[2,], "fdr")

#Resultados ajustados
table(results_adjusted < 0.05)

#Qué miRNAs han salido significativos?
results_adjusted[results_adjusted < 0.05]

#Exportación de resultados a fichero csv
write.csv2(cbind(t(results_lm), results_adjusted), "resultados_lm.csv")

####################################
#Modelo para análisis no paramétrico
####################################
library(ordinal)

#Test wilcoxon-mann-whitney es igual a regresión ordinal
wilcox.test(hsa_let_7a_3p ~ respuesta, data=datos_mirna)

ord1 <- clm(ordered(hsa_let_7a_3p) ~ respuesta, data=datos_mirna)
report(ord1)
summary(ord1)$coeff[rownames(summary(ord1)$coef) %in% "respuestasi", c(1,4)]

#Por lo tanto, podemos hacer un análisis no paramétrico igual que el lineal
#tan solo cambiando la función lm por la función clm del paquete ordinal
#Usamos la función pbsapply, y nos quedamos tanto con el p-valor como con el coeficiente
results_ord <- pbsapply(datos_mirna[,14:6622], function(x){
  coeffs <- summary(clm(ordered(x) ~ datos_mirna$respuesta + datos_mirna$batch))$coeff
  coeffs[rownames(coeffs) %in% "datos_mirna$respuestasi", c(1,4)]
})


table(results_ord[2,] < 0.05)


##############
#Otros modelos
##############
#Para datos de metilación (valores entre 0 y 1) utilizaríamos una regresión beta
#mediante la función betareg() del paquete betareg

#Para datos de RNAseq (recuentos) utilizaríamos una regresión binomial negativa
#mediante la función glm.nb() del paquete MASS


########################
#Computación en paralelo
########################
library(parallel)
cl <- makeCluster(4)  #Creación del cluster de computación
clusterExport(cl, list("datos_mirna"))  #Exportación de los datos a cada nodo
clusterCall(cl, function() require(ordinal))  #Carga del paquete ordinal en cada nodo
system.time(
  results_ord_par <- parSapply(cl, datos_mirna[,14:6622], function(x){
  coeffs <- summary(clm(ordered(x) ~ datos_mirna$respuesta + datos_mirna$batch))$coeff
  coeffs[rownames(coeffs) %in% "datos_mirna$respuestasi", c(1,4)]
})
)

all.equal(results_ord, results_ord_par)  #Los resultados son, obviamente, iguales

