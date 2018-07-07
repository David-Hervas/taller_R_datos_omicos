#Instalación de paquetes necesarios

install.packages(c("glmnet", "randomForest", "clickR", "pbapply", "mixOmics",
                   "sNPLS", "Rtsne", "car", "ordinal", "NMF"))
source("https://bioconductor.org/biocLite.R")
biocLite("pcaMethods")
