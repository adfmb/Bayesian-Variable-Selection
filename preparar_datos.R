preparar_datos<-function(tabla_datos,Entidad="NACIONAL",Concurrente=1,proporcion_entrena,
                         vector_variables,iteraciones_jags,calentamiento_jags, modelo_jags1){
  #tabla_datos<-tabla
  #Dejar solo la entidad que vamos a analizar
  if(Entidad=="NACIONAL"){
    tabla_entrena1<-subset(subset(tabla_datos,Concurrente1==Concurrente),select=c("Ausentismo2",vector_variables))
    indicador_nacional<-1
    tabla_resultados1<-subset(subset(tabla_datos,Concurrente1==Concurrente),select=c(vector_variables,"Ausentismo2",
                                                                                     "Llave.Casilla","NOMBRE_ESTADO.x","iD_ESTADO.x","ID_DISTRITO.x","SECCION","ID_CASILLA","TIPO_CASILLA","EXT_CONTIGUA"))
  }else{
    tabla_entrena1<-subset(subset(tabla_datos,NOMBRE_ESTADO.x==Entidad,select=c("Ausentismo2",vector_variables)))
    indicador_nacional<-0
    Concurrente<-unique(tabla_datos$Concurrente1[which(tabla_datos$NOMBRE_ESTADO.x==Entidad)])
    tabla_resultados1<-subset(subset(tabla_datos,NOMBRE_ESTADO.x==Entidad),select=c(vector_variables,"Ausentismo2",
                                                                                   "Llave.Casilla","NOMBRE_ESTADO.x","iD_ESTADO.x","ID_DISTRITO.x","SECCION","ID_CASILLA","TIPO_CASILLA","EXT_CONTIGUA"))
  }
  
  
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
  inits<-function(){list(alpha=0,tau=1, lnTau=c(rep(1,var_expl)),yest=rep(0,n))}
  
  #-Selecting parameters to monitor-
  parameters<-c("alpha","tau","lnTau","beta","Ind","yest")
  
  if(modelo_jags1==1){
    modelo<-jags(data2,inits,
                 parameters,
                 model.file=modelo_binomial_logit,
                 n.iter=iteraciones_jags,
                 n.chains=1,n.burnin=calentamiento_jags)
    nombre_modelo = "el modelo binomial con liga logit"
  }else{
    modelo<-jags(data2,inits,
                 parameters,
                 model.file=modelo_binomial_probit,
                 n.iter=iteraciones_jags,
                 n.chains=1,n.burnin=calentamiento_jags)
    nombre_modelo = "el modelo binomial con liga probit"
    
  }
  
  modelo.summary<-modelo$BUGSoutput$summary
  head(modelo.summary)
  
  modelo.dic<-modelo$BUGSoutput$DIC
  e1<-data.frame(DIC=modelo.dic)
  e1
  
  if(Concurrente==0){tipo_elec="Federal"}else{tipo_elec="Concurrente"}
  
  var_predic<-names(tabla_entrena2)[2]
  if(length(names(tabla_entrena2))>2){
    for(i in 2:var_expl){
      var_predic<-paste(var_predic, names(tabla_entrena2)[i+1],sep=", ")
    }
  }
  
  #tabla datos de prueba
  tabla_no_entrenada<-tabla_entrena1[-inTraining,]
  #n de tabla prueba
  n2<-nrow(tabla_no_entrenada)  
  
  #-Defining data-
  data_prueba<-list("n"=n2,"var_expl"=var_expl,"y"=tabla_no_entrenada$Ausentismo2)
  for(i in 1:var_expl){
    #i<-1
    data_prueba[[i+3]]<-as.array(tabla_no_entrenada[,i+1])
    
  }
  
  for( j in 1:var_expl){
    #j<-1
    names(data_prueba)[j+3]<-sprintf("x%i",j)
    
  }
  
  x_prueba<-matrix(nrow=n2,ncol=var_expl)
  for( j in 1:var_expl){
    
    x_prueba[,j]<-data_prueba[[j+3]]
    
  }
  
  
  # x[i, ] %*% beta 
  
  
  if(Entidad=="NACIONAL"){
    titulo<-paste("Resultado de aplicar ",nombre_modelo,", utilizando una proporcion del: ",proporcion_entrena*100,"% del total de estados con elección ", tipo_elec," y utilizando como variables predictoras -> ", var_predic,":")
  }else{
    titulo<-paste("Resultado de aplicar ",nombre_modelo,", utilizando una proporcion del: ",proporcion_entrena*100,"% en la entidad de ", Entidad," y utilizando como variables predictoras -> ", var_predic,":")
  }
  
  
  Datos_entrenamiento_exportar<-tabla_resultados1[ inTraining,]
  Datos_prueba_exportar<-tabla_resultados1[ -inTraining,]
  
  return(list("Titulo"=titulo,
              "resumen"=modelo.summary,"DIC"=e1,"matriz_X"=x,"Tabla_no_usada"=tabla_no_entrenada,"Entidad"=Entidad,"TipoElec"=tipo_elec,
              "variables_explicativas"=var_predic,"tabla_entrena2"=tabla_entrena2,"matriz_X_prueba"=x_prueba,
              "tabla_resultados1"=tabla_resultados1,"vector_variables"=vector_variables,
              "Datos_entrenamiento_exportar"=Datos_entrenamiento_exportar,
              "Datos_prueba_exportar"=Datos_prueba_exportar))
}

