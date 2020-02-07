#Optimization Practice#

install.packages("NlcOptim")
library(MASS); library(NlcOptim)

obj = function(x){return(x[1]^2 + x[2]^2)}
