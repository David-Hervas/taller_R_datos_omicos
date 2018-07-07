###################
#Análisis avanzados
###################

#Partial least squares
library("mixOmics")
pls1 <- pls(datos_mirna[,14:6622], as.numeric(datos_mirna$respuesta)-1)
pls1$loadings  #Pesos de cada variable en cada componente
pls1$variates  #Valores de cada observación en la proyección

#Representación de resultados
plot(pls1$variates$X, pch=16, las=1,
     col=c("black", "red")[unclass(datos_mirna$respuesta)])
abline(h=0, v=0, lty=2)
plotIndiv(pls1)

#Selección de variables (importancia de las variables)
pls_vars <- vip(pls1)
pls_vars[order(pls_vars[,1], decreasing=TRUE),][1:10,]
pls_vars[order(pls_vars[,2], decreasing=TRUE),][1:10,]

#Regresión con penalización: LASSO
library(glmnet)
cv1 <- cv.glmnet(as.matrix(datos_mirna[,14:6622]), 
                 datos_mirna$respuesta, 
                 family="binomial", alpha=1)  
plot(cv1, sign.lambda = -1, las=1)
cv1$lambda.1se  #Demasiada variabilidad en la CV

#Repetimos la CV 500 veces (posibilidad de paralelizar como se ha visto antes)
cv_rep1 <- pbreplicate(500, cv.glmnet(as.matrix(datos_mirna[,14:6622]), 
                 datos_mirna$respuesta, 
                 family="binomial", alpha=1)$lambda.1se)

plot(density(cv_rep1))
median(cv_rep1)

fit_lasso <- glmnet(as.matrix(datos_mirna[,14:6622]), datos_mirna$respuesta, 
                    family="binomial", alpha=1)
report(fit_lasso, s=median(cv_rep1))  

#Regresión con penalización: Elastic Net
cv_rep2 <- pbreplicate(500, cv.glmnet(as.matrix(datos_mirna[,14:6622]), 
                                      datos_mirna$respuesta, 
                                      family="binomial", alpha=0.5)$lambda.1se)

plot(density(cv_rep2))
median(cv_rep2)

fit_enet <- glmnet(as.matrix(datos_mirna[,14:6622]), datos_mirna$respuesta, 
                   family="binomial", alpha=0.5)
report(fit_enet, s=median(cv_rep2))  

#Heatmap con los resultados
library(NMF)
report_enet <- report(fit_enet, s=median(cv_rep2))
selected_vars <- names(report_enet$coefficients)[-1]

#Con clustering jerárquico de las observaciones
annotation <- data.frame(Respuesta = datos_mirna$respuesta)
aheatmap(t(datos_mirna[,selected_vars]), color=c("red", "black", "green"), 
         scale="row", annCol = annotation, breaks=0)

#Sin clustering jerárquico de las observaciones
annotation_ord <- data.frame(Respuesta = sort(datos_mirna$respuesta))
aheatmap(t(datos_mirna[order(datos_mirna$respuesta),selected_vars]), 
         color=c("red", "black", "green"), scale="row", annCol = annotation_ord, 
         breaks=0, Colv=NA)

#Random forest
library(ranger)
rf1 <- ranger(respuesta ~., data=datos_mirna[,c(10, 14:6622)], 
              importance = "impurity_corrected")
rf1  #Resultados del modelo
rf_varsel <- importance_pvalues(rf1, method = "janitza")  #Selección de variables
rf_varsel[order(rf_varsel[,"pvalue"]),][1:50,]
