#Instalación de paquetes necesarios

install.packages(c("glmnet", "randomForest", "clickR", "pbapply", "mixOmics",
                   "sNPLS", "Rtsne"))
source("https://bioconductor.org/biocLite.R")
biocLite("pcaMethods")
