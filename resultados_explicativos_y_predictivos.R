library(plyr)
resultados_entidad<-read.csv(paste(Entidad,"_60%_entrena_p05.csv",sep=""),header=TRUE)
resultados_entidad$Mult_extremos_intervalos<-1-as.numeric((resultados_entidad$X2.50.*resultados_entidad$X97.50.)<0)
datos_entrenamiento<-read.csv(paste(Entidad,"_Datos_entrenamiento_60%_entrena.csv",sep=""),header=TRUE)
datos_prueba<-read.csv(paste(Entidad,"_Datos_prueba_40%_prueba.csv",sep=""),header=TRUE)

##############################################################################
#Generación de código para elegir el modelo de mayor frecuencia
betas_simuladas<-read.csv("betas.csv",header=TRUE)
indicadoras_simuladas<-read.csv("indicadoras.csv",header=TRUE)
var_expl<-ncol(betas_simuladas)-1
probs_individuales_Indicadoras<-as.numeric(as.character(resultados_entidad[1:var_expl,2]))
valores_individuales_Betas<-as.numeric(as.character(resultados_entidad[(var_expl+4):(2*var_expl+4-1),2]))

frecuencias_configuraciones<-ddply(indicadoras_simuladas,~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+
                                     V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+
                                     V24+V25+V26,summarise,
                                   Frecuencia=length(V1),
                                   Probabilidad=length(V1)/nrow(indicadoras_simuladas)) #

orden_freq<-frecuencias_configuraciones[order(frecuencias_configuraciones$Frecuencia,decreasing = T) , ]
alpha<-as.numeric(as.character(resultados_entidad[var_expl+3,2]))
betas_promedio<-as.numeric(as.character(resultados_entidad[(var_expl+4):(2*var_expl+4-1),2]))


conf_theta_final_maximo<-numeric(var_expl)
for(j in 1:var_expl){
  # j<-1  
  conf_theta_final_maximo[j]=orden_freq[1,j]*betas_promedio[j]
}

conf_theta_final_promedio<-numeric(var_expl)
for(i in 1:nrow(orden_freq)){
  # i<-1
  for(j in 1:var_expl){
  # j<-1  
    conf_theta_final_promedio[j]=conf_theta_final_promedio[j]+orden_freq[i,j]*orden_freq$Probabilidad[i]*betas_promedio[j]
    
  }
  
}

conf_theta_probas_individuales<-numeric(var_expl)
  for(j in 1:var_expl){
    # j<-1  
    conf_theta_probas_individuales[j]=probs_individuales_Indicadoras[j]*valores_individuales_Betas[j]#*resultados_entidad$Mult_extremos_intervalos[var_expl+3+j]
    
  }


##############################################################################
#####Para explicar datos de entrenamiento
#Se toman los coeficientes de la configuración más visitada/frecuentada/con-probabilidad-mayor

vector_eta_entrenamiento<-rep(alpha,nrow(datos_entrenamiento))
for(i in 1:nrow(datos_entrenamiento)){
  
  for(j in 1:var_expl){
    
    vector_eta_entrenamiento[i]=vector_eta_entrenamiento[i]+conf_theta_final_maximo[j]*datos_entrenamiento[i,j+1]
    
  }
  
}

vector_est_probs<-numeric(nrow(datos_entrenamiento))
for(i in 1:nrow(datos_entrenamiento)){
  
  vector_est_probs[i]<-exp(vector_eta_entrenamiento[i])/(1+exp(vector_eta_entrenamiento[i]))
}

data_comparacion_entrena<-data.frame(Y_reales=datos_entrenamiento$Ausentismo2,Probabilidad_Ausentismo_Estimada=vector_est_probs)

#Graficando
or2<-order(data_comparacion_entrena$Probabilidad_Ausentismo_Estimada)
plot(data_comparacion_entrena$Probabilidad_Ausentismo_Estimada[or2],type="l",ylim=c(0,1.2),xlab="Casillas
(ordenadas segun el modelo de menor a mayor probabilidad)",ylab="Probabilidad de Ausentismo",
main=paste("Comparativa de probabilidades de Ausentismo del modelo
vs Casos Reales en los datos de entrenamiento
(usando argmax)"))
points(data_comparacion_entrena$Y_reales[or2],cex=.5,col=2,type="p")

##############################################################################
#####Para predecir con datos de prueba
#Se toman los coeficientes de cada covariable por su indicadora por su respectiva probabilidad de aparición (promedios)


vector_eta_prueba<-rep(alpha,nrow(datos_prueba))
for(i in 1:nrow(datos_prueba)){
  
  for(j in 1:var_expl){
    
    vector_eta_prueba[i]=vector_eta_prueba[i]+conf_theta_final_promedio[j]*datos_prueba[i,j+1]
    
  }
  
}

vector_prediccion_probs<-numeric(nrow(datos_prueba))
for(i in 1:nrow(datos_prueba)){
  
  vector_prediccion_probs[i]<-exp(vector_eta_prueba[i])/(1+exp(vector_eta_prueba[i]))
}

data_comparacion_prueba<-data.frame(Y_reales=datos_prueba$Ausentismo2,Probabilidad_Ausentismo_Predicha=vector_prediccion_probs)

#Graficando
or3<-order(data_comparacion_prueba$Probabilidad_Ausentismo_Predicha)
plot(data_comparacion_prueba$Probabilidad_Ausentismo_Predicha[or3],type="l",ylim=c(0,1.2),xlab="Casillas
(ordenadas segun el modelo de menor a mayor probabilidad)",ylab="Probabilidad de Ausentismo",
     main=paste("Comparativa de probabilidades de Ausentismo del modelo
vs Casos Reales en los datos de prueba
(usando promedio bayesiano)"))
points(data_comparacion_prueba$Y_reales[or3],cex=.5,col=2,type="p")


##############################################################################
#####Usando las probabilidades individuales

#Datos de entrenamiento


vector_eta_entrenamiento2<-rep(alpha,nrow(datos_entrenamiento))
for(i in 1:nrow(datos_entrenamiento)){
  
  for(j in 1:var_expl){
    
    vector_eta_entrenamiento2[i]=vector_eta_entrenamiento2[i]+conf_theta_probas_individuales[j]*datos_entrenamiento[i,j+1]
    
  }
  
}

vector_est_probs2<-numeric(nrow(datos_entrenamiento))
for(i in 1:nrow(datos_entrenamiento)){
  
  vector_est_probs2[i]<-exp(vector_eta_entrenamiento2[i])/(1+exp(vector_eta_entrenamiento2[i]))
}

data_comparacion_entrenamiento2<-data.frame(Y_reales=datos_entrenamiento$Ausentismo2,Probabilidad_Ausentismo_Estimada2=vector_est_probs2)

#Graficando
or5<-order(data_comparacion_entrenamiento2$Probabilidad_Ausentismo_Estimada2)
plot(data_comparacion_entrenamiento2$Probabilidad_Ausentismo_Estimada2[or5],type="l",ylim=c(0,1.2),xlab="Casillas
     (ordenadas segun el modelo de menor a mayor probabilidad)",ylab="Probabilidad de Ausentismo",
     main=paste("Comparativa de probabilidades de Ausentismo del modelo
                vs Casos Reales"))
points(data_comparacion_entrenamiento2$Y_reales[or5],cex=.5,col=2,type="p")




#Datos de prueba

vector_eta_prueba2<-rep(alpha,nrow(datos_prueba))
for(i in 1:nrow(datos_prueba)){
  
  for(j in 1:var_expl){
    
    vector_eta_prueba2[i]=vector_eta_prueba2[i]+conf_theta_probas_individuales[j]*datos_prueba[i,j+1]
    
  }
  
}

vector_prediccion_probs2<-numeric(nrow(datos_prueba))
for(i in 1:nrow(datos_prueba)){
  
  vector_prediccion_probs2[i]<-exp(vector_eta_prueba2[i])/(1+exp(vector_eta_prueba2[i]))
}

data_comparacion_prueba2<-data.frame(Y_reales=datos_prueba$Ausentismo2,Probabilidad_Ausentismo_Predicha2=vector_prediccion_probs2)

#Graficando
or4<-order(data_comparacion_prueba2$Probabilidad_Ausentismo_Predicha2)
plot(data_comparacion_prueba2$Probabilidad_Ausentismo_Predicha2[or4],type="l",ylim=c(0,1.2),xlab="Casillas
     (ordenadas segun el modelo de menor a mayor probabilidad)",ylab="Probabilidad de Ausentismo",
     main=paste("Comparativa de probabilidades de Ausentismo del modelo
vs Casos Reales"))
points(data_comparacion_prueba2$Y_reales[or4],cex=.5,col=2,type="p")

###################################################################
#####Matriz de confusión
#Hay que recordar que siempre la usaremos para predicción.
matriz_confusion<-function(configuracion_theta,umbral=0.5){
  resumen_<-data.frame(resumen_jags$resumen)
  tabla_prueba<-data.frame(resumen_jags$Tabla_no_usada)
  #y_estimadas2<-resumen_[grep("yest",rownames(resumen_)),]
  #titulo<-resumen_jags$Titulo
  
  betas<-resumen_[grep("beta",rownames(resumen_)),]
  
  coeficientes<-rbind(resumen_[1,],betas)
  coeficientes_final<-subset(coeficientes,select = c("mean","sd","X2.5.","X97.5."))
  
  names(coeficientes_final)<-c("Media","DE","cuantil 2.5%","cuantil 97.5%")
  names(coeficientes_final)<-c("Media","DE","lim.inf. 95%","lim. sup. 95%")
  
  medias_coef<-coeficientes_final$Media
  
  x_prueba<-resumen_jags$matriz_X_prueba
  x2_prueba<-matrix(cbind(rep(1,nrow(x_prueba)),x_prueba),ncol=ncol(x_prueba)+1)
  
  x_prueba<-resumen_jags$matriz_X_prueba
  x2_prueba<-matrix(cbind(rep(1,nrow(x_prueba)),x_prueba),ncol=ncol(x_prueba)+1)
  
  eta<-numeric(nrow(x_prueba))
  for(i in 1:nrow(x2_prueba))
    #i<-1
    for(j in 1:ncol(x2_prueba)){
      #j<-1
      eta[i]<-eta[i]+x2_prueba[i,j]*medias_coef[j]
      
    }
  if(tipo_de_liga=="logit"){
    
    p<-exp(eta)/(1+exp(eta))
    
  }else{
    
    p<-pnorm(eta)
  }
  
  tabla_comparativa<-data.frame(
    #Y_Estimadas=y_estimadas2$mean,
    Reales=tabla_prueba$Ausentismo2,Probabilidad_estimada=p)
  #or2<-order(p)
  data_real_ausentismo<-subset(tabla_comparativa,Reales==1)
  data_real_NO_ausentismo<-subset(tabla_comparativa,Reales==0)
  
  verdaderos_positivos<-length(data_real_ausentismo$Probabilidad_estimada[which(data_real_ausentismo$Probabilidad_estimada>=umbral)])
  falsos_negativos<-length(data_real_ausentismo$Probabilidad_estimada[which(data_real_ausentismo$Probabilidad_estimada<=umbral)])
  falsos_positivos<-length(data_real_NO_ausentismo$Probabilidad_estimada[which(data_real_NO_ausentismo$Probabilidad_estimada>=umbral)])
  verdaderos_negativos<-length(data_real_NO_ausentismo$Probabilidad_estimada[which(data_real_NO_ausentismo$Probabilidad_estimada<=umbral)])
  
  matriz_confusion<-matrix(ncol=2,nrow=2)
  colnames(matriz_confusion)<-c("Positivos predichos", "Negativos predichos")
  rownames(matriz_confusion)<-c("Positivos Reales","Negativos reales")
  
  matriz_confusion[1,1]<-verdaderos_positivos
  matriz_confusion[1,2]<-falsos_negativos
  matriz_confusion[2,1]<-falsos_positivos
  matriz_confusion[2,2]<-verdaderos_negativos
  
  mat_conf<-as.data.frame(matriz_confusion)
  
  return(mat_conf)
}


