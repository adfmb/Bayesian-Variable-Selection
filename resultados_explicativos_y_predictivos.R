library(plyr)
resultados_entidad<-read.csv(paste(Entidad,"_60%_entrena_p05.csv",sep=""),header=TRUE)
resultados_entidad$Mult_extremos_intervalos<-1-as.numeric((resultados_entidad$X2.50.*resultados_entidad$X97.50.)<0)
datos_entrenamiento<-read.csv(paste(Entidad,"_Datos_entrenamiento_60%_entrena.csv",sep=""),header=TRUE)
datos_prueba<-read.csv(paste(Entidad,"_Datos_prueba_40%_prueba.csv",sep=""),header=TRUE)
betas_simuladas<-read.csv("betas.csv",header=TRUE)
indicadoras_simuladas<-read.csv("indicadoras.csv",header=TRUE)
var_expl<-ncol(betas_simuladas)-1
probs_individuales_Indicadoras<-as.numeric(as.character(resultados_entidad[1:var_expl,2]))
valores_individuales_Betas<-as.numeric(as.character(resultados_entidad[(var_expl+4):(2*var_expl+4-1),2]))

##############################################################################
#Generación de código para generar las configuraciones del vector final theta dependiendo de
#del método para la generación del modelo

#Tabla para generar frecuencias y probabilidades de cada una de las configuraciones gamma generadas
frecuencias_configuraciones<-ddply(indicadoras_simuladas,~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+
                                     V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+
                                     V24+V25+V26,summarise,
                                   Frecuencia=length(V1),
                                   Probabilidad=length(V1)/nrow(indicadoras_simuladas)) #
#Ponemos la configuración de mayor frecuencia (probabilidad) hasta arriba de la tabla y la dejamos en orden decreciente
orden_freq<-frecuencias_configuraciones[order(frecuencias_configuraciones$Frecuencia,decreasing = T) , ]

#Tomamos las medias de las betas y alpha generadas or Gibbs
alpha<-as.numeric(as.character(resultados_entidad[var_expl+3,2]))
betas_promedio<-as.numeric(as.character(resultados_entidad[(var_expl+4):(2*var_expl+4-1),2]))


#1.- Configuración para argmax (para casos explicativos)
conf_theta_final_maximo<-numeric(var_expl)
for(j in 1:var_expl){
  # j<-1  
  conf_theta_final_maximo[j]=orden_freq[1,j]*betas_promedio[j]
}

#2.- Configuración para promedio bayesiano (para casos predictivos)
conf_theta_final_promedio<-numeric(var_expl)
for(i in 1:nrow(orden_freq)){
  # i<-1
  for(j in 1:var_expl){
  # j<-1  
    conf_theta_final_promedio[j]=conf_theta_final_promedio[j]+orden_freq[i,j]*orden_freq$Probabilidad[i]*betas_promedio[j]
    
  }
  
}

#3.- Configuración para utilizar las probabilidades individuales de cada variable (método propio)
conf_theta_probas_individuales<-numeric(var_expl)
  for(j in 1:var_expl){
    # j<-1  
    conf_theta_probas_individuales[j]=probs_individuales_Indicadoras[j]*valores_individuales_Betas[j]#*resultados_entidad$Mult_extremos_intervalos[var_expl+3+j]
    
  }


##############################################################################
#####Para explicar datos de entrenamiento
#Se toman los coeficientes de la configuración más visitada/frecuentada/con-probabilidad-mayor (1)

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
#Se toman los coeficientes de cada covariable por su indicadora por su respectiva probabilidad de aparición (promedios) (2)


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
#####Usando las probabilidades individuales (3)

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
vs Casos Reales en los datos de entrenamiento
(usando las probabilidades individuales de cada Indicadora)"))
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
vs Casos Realesen los datos de prueba
(usando las probabilidades individuales de cada Indicadora)"))
points(data_comparacion_prueba2$Y_reales[or4],cex=.5,col=2,type="p")

##################################################################
#Datos para generar la gráfica de betas vs probabilidades de aparición en el modelo
probs<-probs_individuales_Indicadoras
coef_betas<-betas_promedio

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

###################################################################
#####Matriz de confusión
#se pueden cambiar el umbral (threshold) a partir del cuál se consideran casos de Ausentismo
matriz_confusion<-function(tabla_comparativa,umbral=0.5){
  
  data_real_ausentismo<-subset(tabla_comparativa,Y_reales==1)
  data_real_NO_ausentismo<-subset(tabla_comparativa,Y_reales==0)
  names(data_real_ausentismo)[2]<-"Probabilidad_estimada"
  names(data_real_NO_ausentismo)[2]<-"Probabilidad_estimada"
  
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

matriz_confusion(data_comparacion_entrena)

matriz_confusion(data_comparacion_prueba)

matriz_confusion(data_comparacion_entrenamiento2)

matriz_confusion(data_comparacion_prueba2)

