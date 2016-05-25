require(gridExtra)
require(runjags)
require(ggmcmc)
library(coda)
library(Rcpp)
library(maps)
library(mapproj)
library(ggplot2)
library(shiny)
library(R2jags)
library(rjags)
library(psych)
library(caret)
library(bnclassify)
library(klaR)
library(rocc)

setwd("~/Dropbox/01_ITAM_Ciencia_de_Datos/2do_semestre/Analisis_Multivariado/Bayesian-Variable-Selection")
tabla<-read.csv("datos_falsos_para_pruebas.csv",header=TRUE)

tabla_datos<-tabla
Entidad<-"DISTRITO FEDERAL"
names(tabla)
Concurrente=1
proporcion_entrena<-.6
vector_variables<-names(tabla)[10:ncol(tabla_datos)]
iteraciones_jags<-15000
calentamiento_jags<-7500
# modelo_jags1<-1

tabla_entrena1<-subset(subset(tabla_datos,NOMBRE_ESTADO.x==Entidad),select=c("Ausentismo2",vector_variables))
indicador_nacional<-0
Concurrente<-unique(tabla_datos$Concurrente1[which(tabla_datos$NOMBRE_ESTADO.x==Entidad)])
tabla_resultados1<-subset(subset(tabla_datos,NOMBRE_ESTADO.x==Entidad,select=c(vector_variables,"Ausentismo2",
                                                                               "Llave.Casilla","NOMBRE_ESTADO.x","iD_ESTADO.x","ID_DISTRITO.x","SECCION","ID_CASILLA","TIPO_CASILLA","EXT_CONTIGUA")))

inTraining <- createDataPartition(tabla_entrena1$Ausentismo2, p = proporcion_entrena, list = FALSE)
tabla_entrena2 <- tabla_entrena1[ inTraining,]
n<-nrow(tabla_entrena2)
var_expl<-ncol(tabla_entrena2)-1

#-Defining data-
data<-list("n"=n,"var_expl"=var_expl,"y"=tabla_entrena2$Ausentismo2)
for(i in 1:var_expl){
  #i<-1
  data[[i+3]]<-as.array(tabla_entrena2[,i+1])
  
}

for( j in 1:var_expl){
  #j<-1
  names(data)[j+3]<-sprintf("x%i",j)
  
}

x<-matrix(nrow=n,ncol=var_expl)
for( j in 1:var_expl){
  
  x[,j]<-data[[j+3]]
  
}



data2<-list("n"=n,"var_expl"=var_expl,"y"=tabla_entrena2$Ausentismo2,"x"=x)
#-Defining inits-
inits<-function(){list(alpha=0,sdBeta=.5,IndA=c(rep(1,var_expl)))}

#-Selecting parameters to monitor-
parameters<-c("alpha","sdBeta","Ind","beta","tauBeta","TauM")#,"yest")
out3_2 <- run.jags("ssvs_03.txt", parameters, data=data2, n.chains=3,inits=inits,
                 method="parallel", adapt=5000, burnin=5000)

jags_out3_1<-jags(data2,inits,
                  parameters,
                  model.file="ssvs_03.txt",
                  n.iter=iteraciones_jags,
                  n.chains=3,n.burnin=calentamiento_jags)

jags_out3_2<-jags(data2,inits,
     parameters,
     model.file="ssvs_03.txt",
     n.iter=iteraciones_jags,
     n.chains=1,n.burnin=calentamiento_jags)


jags_out_60porc<-jags(data2,inits,
                  parameters,
                  model.file="ssvs_03.txt",
                  n.iter=iteraciones_jags,
                  n.chains=1,n.burnin=calentamiento_jags)

# out3 <- run.jags("ssvs_03.txt", parameters, data=data2, n.chains=3,inits=inits,
#                  method="parallel", adapt=5000, burnin=5000)


outdf <- ggs(as.mcmc.list(out3_1))
ncov<-var_expl
probs <- out3_1$summary$statistics[((3):(2+ncov)), 1]
residuals(out3_2,out3_2$residual,show.summary=TRUE)
