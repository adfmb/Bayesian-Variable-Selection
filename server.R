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
resultados_entidades<-read.csv("sumaries.csv",header = T)
entrenamiento_entidades<-read.csv("datos_entrenamiento_entidades.csv",header=TRUE)
prueba_entidades<-read.csv("datos_prueba_entidades.csv",header=TRUE)
betas_entidades<-read.csv("betas.csv",header=TRUE)
indicadoras_entidades<-read.csv("indicadoras.csv",header=TRUE)






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
    tabla_resultados1<-subset(subset(tabla_datos,NOMBRE_ESTADO.x==Entidad,select=c(vector_variables,"Ausentismo2",
    "Llave.Casilla","NOMBRE_ESTADO.x","iD_ESTADO.x","ID_DISTRITO.x","SECCION","ID_CASILLA","TIPO_CASILLA","EXT_CONTIGUA")))
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
  inits<-function(){list(alpha=0,beta=c(rep(0,var_expl)),yest=rep(0,n))}
  
  #-Selecting parameters to monitor-
  parameters<-c("alpha","beta","yest")
  
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

plotear<-function(resumen_jags,tipo_de_liga="logit"){
  resumen_<-data.frame(resumen_jags$resumen)
  tabla_entrena2<-data.frame(resumen_jags$tabla_entrena2)
  y_estimadas<-resumen_[grep("yest",rownames(resumen_)),]
  titulo<-resumen_jags$Titulo
  
  betas<-resumen_[grep("beta",rownames(resumen_)),]
  
  coeficientes<-rbind(resumen_[1,],betas)
  coeficientes_final<-subset(coeficientes,select = c("mean","sd","X2.5.","X97.5."))
  
  names(coeficientes_final)<-c("Media","DE","cuantil 2.5%","cuantil 97.5%")
  names(coeficientes_final)<-c("Media","DE","lim.inf. 95%","lim. sup. 95%")
  
  medias_coef<-coeficientes_final$Media
  
  x<-resumen_jags$matriz_X
  x2<-matrix(cbind(rep(1,nrow(x)),x),ncol=ncol(x)+1)
  
  eta<-numeric(nrow(x))
  for(i in 1:nrow(x2))
    #i<-1
    for(j in 1:ncol(x2)){
      #j<-1
      eta[i]<-eta[i]+x2[i,j]*medias_coef[j]
      
    }
  if(tipo_de_liga=="logit"){
    
    p<-exp(eta)/(1+exp(eta))
    
  }else{
    
    p<-pnorm(eta)
  }
  
  
  tabla_comparativa<-data.frame(Y_Estimadas=y_estimadas$mean,Reales=tabla_entrena2$Ausentismo2,Combinacion_Lineal=eta)
  or2<-order(p)
  #   or3<-order(tabla_comparativa$Reales)
  #   t2<-data.frame(Probabilidad_ausentismo=p,tabla_comparativa)
  
  #   plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas",ylab="Probabilidad de Ausentismo",main=resumen_jags$Entidad)
  #   points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")  
  
  #   plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas",ylab="Probabilidad de Ausentismo",main=titulo)
  #   points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")
  
  plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas
       (ordenadas segun el modelo de menor a mayor probabilidad)",ylab="Probabilidad de Ausentismo",main=paste("Comparativa de probabilidades de Ausentismo del modelo
vs Casos Reales"))
  points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")
  #mtext(paste("(",resumen_jags$variables_explicativas,") ",tipo_de_liga,sep=""))
  
}

plotear_datos_prueba<-function(resumen_jags,tipo_de_liga="logit"){
  resumen_<-data.frame(resumen_jags$resumen)
  tabla_prueba<-data.frame(resumen_jags$Tabla_no_usada)
  #y_estimadas2<-resumen_[grep("yest",rownames(resumen_)),]
  titulo<-resumen_jags$Titulo
  
  betas<-resumen_[grep("beta",rownames(resumen_)),]
  
  coeficientes<-rbind(resumen_[1,],betas)
  coeficientes_final<-subset(coeficientes,select = c("mean","sd","X2.5.","X97.5."))
  
  names(coeficientes_final)<-c("Media","DE","cuantil 2.5%","cuantil 97.5%")
  names(coeficientes_final)<-c("Media","DE","lim.inf. 95%","lim. sup. 95%")
  
  medias_coef<-coeficientes_final$Media
  
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
    Reales=tabla_prueba$Ausentismo2,Combinacion_Lineal=eta)
  or2<-order(p)
  #   or3<-order(tabla_comparativa$Reales)
  #   t2<-data.frame(Probabilidad_ausentismo=p,tabla_comparativa)
  
  #   plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas",ylab="Probabilidad de Ausentismo",main=resumen_jags$Entidad)
  #   points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")  
  
  #   plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas",ylab="Probabilidad de Ausentismo",main=titulo)
  #   points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")
  
  plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas
       (ordenadas segun el modelo de menor a mayor probabilidad)",ylab="Probabilidad de Ausentismo",main=paste("Comparativa de probabilidades de Ausentismo del modelo
vs Casos Reales para los datos de prueba"))
  points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")
  #mtext(paste("(",resumen_jags$variables_explicativas,") ",tipo_de_liga,sep=""))
  
}

Calcular_Matriz_confusion<-function(tabla_comparativa,umbral=0.5){
  
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


################################################################
shinyServer(function(input, output,session) {
  #set.seed(122)
  output$map <- renderPlot({
    
    if(input$var=="Proporcion de Casillas con Ausentismo"){
      ggplot() + geom_polygon(data = plot_proporcion_Ausentismo, 
                              aes(x =long, 
                                  y = lat, 
                                  group = plot_proporcion_Ausentismo[[6]],
                                  fill = Proporcion_de_casillas_con_Ausentismo), 
                              color = "black", size = 0.25)+
        scale_fill_gradient(low = "yellow1", high = "firebrick3")
      
    }else if(input$var=="Cantidad de Casillas con Ausentismo"){
      
      ggplot() + geom_polygon(data = plot_cantidad_Ausentismo, 
                              aes(x =long, 
                                  y = lat, 
                                  group = plot_cantidad_Ausentismo[[6]],
                                  fill = Cantidad_de_casillas_con_Ausentismo), 
                              color = "black", size = 0.25)+
        scale_fill_gradient(low ="palegreen", high = "turquoise4")
      
    }else if(input$var=="Cantidad de Casillas"){
      ggplot() + geom_polygon(data = plot_cantidad_Casillas, 
                              aes(x =long, 
                                  y =lat, 
                                  group = plot_cantidad_Casillas[[6]],
                                  fill = Cantidad_de_casillas), 
                              color = "black", size = 0.25)+
        scale_fill_gradient(low = "lightcyan", high = "dodgerblue3")
      
    }
    
  })
  
  resultados_entidad <- reactive({
    
    a<-subset(resultados_entidades,ENTIDAD=input$Entidad)
    a$Mult_extremos_intervalos<-1-as.numeric((a$X2.50.*a$X97.50.)<0)
    
    return(a)
  })
  
  datos_entrenamiento <- reactive({
    
    subset(entrenamiento_entidades,NOMBRE_ESTADO.x=input$Entidad)
    
  })
  
  datos_prueba <- reactive({
    
    subset(prueba_entidades,NOMBRE_ESTADO.x=input$Entidad)
    
  })
  
  betas_simuladas <- reactive({
    
    subset(betas_entidades,ENTIDAD=input$Entidad)
    
  })
  
  indicadoras_simuladas <- reactive({
    
    subset(indicadoras_entidades,ENTIDAD=input$Entidad)
    
  })
  
  var_expl <- reactive({
    
    ncol(betas_simuladas())-2
    
  })
  
  probs_individuales_Indicadoras<-reactive({
    
    as.numeric(as.character(resultados_entidad()[1:var_expl()]))
                                                 
  })
  
  valores_individuales_Betas<-reactive({
    as.numeric(as.character(resultados_entidad()[(var_expl()+4):(2*var_expl()+4-1),2]))
  })
  
  frecuencias_configuraciones<-reactive({
    
    ddply(indicadoras_simuladas(),~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+
                                       V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+
                                       V24+V25+V26,summarise,
                                     Frecuencia=length(V1),
                                     Probabilidad=length(V1)/nrow(indicadoras_simuladas())) #
  })
  
  orden_freq<-reactive({
    
    frecuencias_configuraciones()[order(frecuencias_configuraciones()$Frecuencia,decreasing = T) , ]
  
    })
    
  alpha<-reactive({
    
    as.numeric(as.character(resultados_entidad()[var_expl()+3,2]))
    
    })
  
  betas_promedio<-reactive({
    
    as.numeric(as.character(resultados_entidad()[(var_expl()+4):(2*var_expl()+4-1),2]))
  
  
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
  
  
  
  
  
  
  
  plotInput <- reactive({
    if(input$liga==1){
      plotear(dataInput())
    }else if(input$liga==2){
      plotear(dataInput(),tipo_de_liga="probit")
    }else{
      plotear_MH(dataInput())
    }
  })
  
  output$plot1 <- renderPlot({
    print(plotInput())
  })
  
  ##Ploteo de los datos de prueba con el modelo

  plotInput2 <- reactive({
    if(input$liga==1){
      plotear_datos_prueba(dataInput())
    }else if(input$liga==2){
      plotear_datos_prueba(dataInput(),tipo_de_liga="probit")
    }else{
      plotear_MH(dataInput())
    }
  })
  
  output$plot2 <- renderPlot({
    print(plotInput2())
  })  
  
  output$sumario<-renderPrint({
    sumario<-dataInput()$Titulo
    sumario
  })
  
  output$tabla_resumen<-renderTable({
    
    tablasInput()$coeficientes_final
  
  })
  
  output$tabla_resumen_2_1<-renderTable({
    
    tablasInput()$coeficientes_final_2_1
    
  })
  
  output$tabla_resumen_2_2<-renderTable({
    
    tablasInput()$coeficientes_final_2_2
    
  })
  
  output$tabla_final_casillas_ordenadas<-renderDataTable({
    
    tablasInput()$Casillas_con_predicciones_de_probabilidad_de_Ausentismo_compacta
    
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  output$umbrales_1_2<-renderTable({
    
    umbralesInput()$umbrales_1_2
    
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() { 
      paste(input$Entidad,"_Casillas_con_probabilidades_de_ausentismo_",dataInput()$variables_explicativas,100*input$Proporcion,
            "-",100*(1-input$Proporcion),'.csv', sep='') 
    },
    content = function(file) {
      write.csv(tablasInput()$Casillas_con_predicciones_de_probabilidad_de_Ausentismo, file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { 
      paste(input$Entidad,"_Pesos_variables_",dataInput()$variables_explicativas,100*input$Proporcion,
            "-",100*(1-input$Proporcion),'.csv', sep='') 
    },
    content = function(file) {
      write.csv(tablasInput()$coeficientes_final_2_1, file)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() { 
      paste(input$Entidad,"_Datos_Entrenamiento_",dataInput()$variables_explicativas,100*input$Proporcion,
            "-",100*(1-input$Proporcion),'.csv', sep='') 
    },
    content = function(file) {
      write.csv(dataInput()$Datos_entrenamiento_exportar, file)
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() { 
      paste(input$Entidad,"_Datos_Prueba_",dataInput()$variables_explicativas,100*input$Proporcion,
            "-",100*(1-input$Proporcion),'.csv', sep='') 
    },
    content = function(file) {
      write.csv(dataInput()$Datos_prueba_exportar, file)
    }
  )
  
  output$downloadData5 <- downloadHandler(
    filename = function() { 
      paste(input$Entidad,"_Umbrales_Variables_para_",100*input$umbral,"%_de_prob_de_ausentismo_",
            dataInput()$variables_explicativas,100*input$Proporcion,
            "-",100*(1-input$Proporcion),'.csv', sep='') 
    },
    content = function(file) {
      write.csv(umbralesInput()$umbrales_1, file)
    }
  )
    
#   output$tabla_resumen<-renderTable({
#     if(input$liga==3){
#     tablas_coeficientes_MH(dataInput())$coeficientes_final
#   }else{
#     tablas_coeficientes(dataInput())$coeficientes_final
#   }
#   })
#   
#   output$tabla_final_casillas_ordenadas<-renderTable({
#     if(input$liga==3){
#       tablas_coeficientes_MH(dataInput())$Casillas_con_predicciones_de_probabilidad_de_Ausentismo
#     }else{
#       tablas_coeficientes(dataInput())$Casillas_con_predicciones_de_probabilidad_de_Ausentismo
#     }
#   })
#   
  
  
  prep_matrizConfu_Input<-reactive({
    
    Calcular_Matriz_confusion(dataInput(),tipo_de_liga="logit",umbral=input$umbral)
    
  })
  
  output$mat_conf<-renderTable({
    #     dataset<-data.frame(DIC=dataInput()$DIC,Probabilidad_maxima=tablas_coeficientes(dataInput())$Probabilidad_mayor,Proporcion_Casillas_Con_Ausentismo_en_el_ambito=tablas_coeficientes(dataInput())$Propocion_de_Ausentismo)
    #     names(dataset)<-c("DIC","Probabilidad predica más alta","Propoción de Casillas con Ausentismo en el ámbito de análisis")
    #     dataset
    print(prep_matrizConfu_Input())
  })
  
  comparativaInput<-reactive({
    if(input$liga==1){
      dataset<-data.frame(DIC=dataInput()$DIC,Probabilidad_maxima=tablas_coeficientes(dataInput())$Probabilidad_mayor,Proporcion_Casillas_Con_Ausentismo_en_el_ambito=tablas_coeficientes(dataInput())$Propocion_de_Ausentismo)
      names(dataset)<-c("DIC","Probabilidad predicha más alta","Propoción de Casillas con Ausentismo en el ámbito de análisis")
      dataset
    }else if(input$liga==2){
      dataset<-data.frame(DIC=dataInput()$DIC,Probabilidad_maxima=tablas_coeficientes(dataInput(),tipo_de_liga="probit")$Probabilidad_mayor,Proporcion_Casillas_Con_Ausentismo_en_el_ambito=tablas_coeficientes(dataInput(),tipo_de_liga="probit")$Propocion_de_Ausentismo)
      names(dataset)<-c("DIC","Probabilidad predicha más alta","Propoción de Casillas con Ausentismo en el ámbito de análisis")
      dataset
    }else{
      dataset<-data.frame(Probabilidad_maxima=tablas_coeficientes(dataInput())$Probabilidad_mayor,Proporcion_Casillas_Con_Ausentismo_en_el_ambito=tablas_coeficientes(dataInput())$Propocion_de_Ausentismo)
      names(dataset)<-c("Probabilidad predicha más alta","Propoción de Casillas con Ausentismo en el ámbito de análisis")
      dataset
      
    }
  })
  
  output$comparativa<-renderTable({
    #     dataset<-data.frame(DIC=dataInput()$DIC,Probabilidad_maxima=tablas_coeficientes(dataInput())$Probabilidad_mayor,Proporcion_Casillas_Con_Ausentismo_en_el_ambito=tablas_coeficientes(dataInput())$Propocion_de_Ausentismo)
    #     names(dataset)<-c("DIC","Probabilidad predica más alta","Propoción de Casillas con Ausentismo en el ámbito de análisis")
    #     dataset
    print(comparativaInput())
  })
  
  observe({
   
    if (input$radio==1) {
      updateTabsetPanel(session,inputId= "inTabset", selected = "panel1")
    }else {
      updateTabsetPanel(session,inputId= "inTabset", selected = "panel2")
    }
  })
  
  
  
  
})
  
  
  
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
  
  
  
  

################################################################
# ESTIMATION OF A MEAN
# X <- as.matrix(cbind(1,iris[,2:4]))
# Y <- iris[ ,1]
# 
# nsim <- 10000
# init <- c(1,1,1,1,10)
# mh.y <- MHBayes(nsim, theta0=init, objdens1, proposal1, X, Y)
# estims <- mh.y$theta
# 
# simulacionesY<-SimulacionesYest(estims,X)
# sim<-simulacionesY$Simulaciones
# medias_sim<-simulacionesY$Medias_Simulaciones
# 
# plot(Y,medias_sim,xlim=c(4,8),ylim=c(3,11))
# abline(a=0,b=1)ç
  