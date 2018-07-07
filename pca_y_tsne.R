############################
#Exploración de datos ómicos
############################

#Análisis de componentes principales (PCA)
library(pcaMethods)
library(ggplot2)

names(datos_mirna)[1:50]
pca1 <- pca(datos_mirna[,14:6622], nPcs = 2, scale="uv", center=TRUE) 
plot(pca1)  #Explicación de la variabilidad por cada componente

#Coeficientes o "pesos" de cada variable en cada componente
pca1@loadings  

#Visualización de los loadings con plot base
matplot(pca1@loadings, type="l")

#Visualización con ggplot2
dim(pca1@loadings)
pca_loadings <- data.frame(id=rep(1:6609, 2), 
                           loading=c(pca1@loadings[,1], pca1@loadings[,2]), 
                           pc=rep(c("PC1","PC2"), each=6609))
ggplot(pca_loadings, aes(x=id, y=loading, color=pc))+geom_line()+facet_wrap(~pc)

#Proyecciones para cada observación
pca1@scores

#Visualización con plot base
plot(pca1@scores, pch=16)
#batch effect?
plot(pca1@scores, pch=16, col=c("red", "blue")[unclass(datos_mirna$batch)], las=1)
abline(h=0, v=0, lty=2)
#Diferencias en la variable respuesta?
plot(pca1@scores, pch=16, col=c("red", "blue")[unclass(datos_mirna$respuesta)], las=1)
abline(h=0, v=0, lty=2)

#Visualización con ggplot2
ggplot(data.frame(pca1@scores), aes(x=PC1, y=PC2, color=datos_mirna$batch)) + 
  geom_point() + geom_vline(xintercept=0, lty=2) + geom_hline(yintercept=0, lty=2)

#3D (no recomendado)
library(car)
pca1 <- pca(datos_mirna[,14:6622], nPcs = 3, scale="uv", center=TRUE) 
scatter3d(pca1@scores[,1], pca1@scores[,2], pca1@scores[,3], 
               group=datos_mirna$batch, ellipsoid=TRUE, surface=FALSE)

#t-distributed stochastic neighbor embedding (t-SNE)
library(Rtsne)
tsne1 <- Rtsne(datos_mirna[,14:6622], perplexity=4, theta=0.0, max_iter=10000)
plot(tsne1$Y, col=datos_mirna$batch, pch=16, las=1)
