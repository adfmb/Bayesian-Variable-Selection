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
library(plyr)

setwd("~/Dropbox/01_ITAM_Ciencia_de_Datos/2do_semestre/Analisis_Multivariado/Bayesian-Variable-Selection")
tabla<-read.csv("datos_falsos_para_pruebas.csv",header=TRUE)

tabla_datos<-tabla
Entidad<-"DISTRITO FEDERAL"
names(tabla)
Concurrente=1
proporcion_entrena<-.6
vector_variables<-names(tabla)[c(10:16,17,20:21,23,25:29,31:40)]
iteraciones_jags<-13000
calentamiento_jags<-6000
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

jags_out_60porc<-jags(data2,inits,
                      parameters,
                      model.file="ssvs_03.txt",
                      n.iter=iteraciones_jags,
                      n.chains=3,n.burnin=calentamiento_jags) #Se corrió con p=20/127

jags_out_60porc_PInd_05<-jags(data2,inits,
                      parameters,
                      model.file="ssvs_03.txt",
                      n.iter=iteraciones_jags,
                      n.chains=3,n.burnin=calentamiento_jags) #Se corrió con p=.5


jags_out_60porc_PInd_05_nsim<-jags(data2,inits,
                              parameters,
                              model.file="ssvs_03.txt",
                              n.iter=iteraciones_jags,
                              n.chains=3,n.burnin=calentamiento_jags,n.thin=2)

#Datos para generar la gráfica de betas vs probabilidades de aparición en el modelo
probs<-jags_out_60porc_PInd_05$BUGSoutput$summary[1:var_expl,1]
coef_betas<-jags_out_60porc_PInd_05$BUGSoutput$summary[(var_expl+4):(2*var_expl+4-1),1]

cfs<-numeric(length(coef_betas))
abs_cfs<-numeric(length(coef_betas))
for(i in 1:length(coef_betas)){
    
    cfs[i]<-coef_betas[i]
    abs_cfs[i]<-abs(coef_betas[i])
}
or1<-order(abs_cfs)
ordenados_cfs<-abs_cfs[or1]
or_coef<-order(abs(coef_betas))
ordenados_proba<-probs[or_coef]

df <- data.frame(probs=probs[or_coef], coefs = abs(coef_betas)[or_coef])
p2 <- ggplot(df, aes(x=coefs, y=probs)) +
geom_point(size=5, alpha=.7) +
theme_classic() +
xlab("Absolute value of estimate coefficient") +
ylab("Posterior probability of non-zeroness")
# Aquí termina la gráfica de betas vs probabilidades de aparición en el modelo

##############################################################################
#Generación de código para elegir el modelo de mayor frecuencia
betas<-as.data.frame(jags_out_60porc_PInd_05$BUGSoutput$sims.list$beta)
indicadoras<-as.data.frame(jags_out_60porc_PInd_05$BUGSoutput$sims.list$Ind)
write.csv(betas,file=paste("betas.csv",sep=""),row.names = TRUE)
write.csv(indicadoras,file=paste("indicadoras.csv",sep=""),row.names = TRUE)


frecuencias_configuraciones<-ddply(indicadoras,~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+
V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+
V24+V25+V26,summarise,
Frecuencia=length(V1)) #






##############################################################################
#Generación de código para correr modelo elegido







write.csv(as.data.frame(jags_out_60porc$BUGSoutput$summary),file=paste(Entidad,"_",100*proporcion_entrena,"%_entrena",".csv",sep=""),row.names = TRUE)
write.csv(tabla_resultados1[inTraining,],file=paste(Entidad,"_Datos_entrenamiento_",100*proporcion_entrena,"%_entrena",".csv",sep=""),row.names = TRUE)
write.csv(tabla_resultados1[- inTraining,],file=paste(Entidad,"_Datos_prueba_",100*(1-proporcion_entrena),"%_prueba",".csv",sep=""),row.names = TRUE)
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



# out3 <- run.jags("ssvs_03.txt", parameters, data=data2, n.chains=3,inits=inits,
#                  method="parallel", adapt=5000, burnin=5000)


outdf <- ggs(as.mcmc.list(out3_1))
ncov<-var_expl
probs <- out3_1$summary$statistics[((3):(2+ncov)), 1]
residuals(out3_2,out3_2$residual,show.summary=TRUE)
