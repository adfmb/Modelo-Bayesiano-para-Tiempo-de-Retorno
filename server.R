# install.packages("Rcpp")
# install.packages("maps")
# install.packages("mapproj")
# install.packages("ggplot2")
# install.packages("shiny")
# install.packages("R2jags")
# install.packages("rjags")
# install.packages("psych")
# install.packages("caret")
# install.packages("bnclassify")
# install.packages("klaR")
# install.packages("rocc")
# install.packages("RCurl")

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
library("RCurl") 

tabla<-function(){
  
  read.table(textConnection(getURL(
    "https://s3-us-west-2.amazonaws.com/datos-simulados-para-apps/simulados_tabla_tiempo_retorno.csv"
  )), sep=",", header=TRUE)
  
}

modelo_geometric_canonic<-function(){
  
  for(i in 1:n){
    y[i] ~ dnegbin(p[i],1)
    #Funcion Liga
    p[i]<-1.0-.99*exp(z[i])
    z[i]<-alpha+x[i,]%*%beta
    
  }
  #A priori's
  
  alpha~dnorm(0,.001)
  for(i in 1:var_expl){
    
    beta[i]~dnorm(0,.001)
    
  }
  #Estimaciones de Ys conocidas con
  #La posterior que genera Jags
  
  
  for (i in 1:n) { yest[i] ~ dnegbin(p[i],1)}
  
}


preparar_datos<-function(tabla_datos,clase_NumeroOperaciones="B",proporcion_entrena,
                         vector_variables,iteraciones_jags,calentamiento_jags){
  #   tabla_datos<-tabla
  #   clase_NumeroOperaciones="B"
  #   proporcion_entrena<-.3
  #   vector_variables<-c("Monto.....Abonado.en.el.dia")
  #   iteraciones_jags<-5000
  #   calentamiento_jags<-1000
  #Dejar solo la entidad que vamos a analizar
  
  tabla_entrena1<-subset(subset(tabla_datos,
                                Clasificacion.de.acuerdo.de.numero.de.operaciones==clase_NumeroOperaciones,
                                select=c("Dias.para.el.siguiente.abono",vector_variables)))
  tabla_resultados1<-subset(tabla_datos,select=c(vector_variables,"Dias.para.el.siguiente.abono",
                                                 "Contrato","Fecha"))
  
  inTraining <- createDataPartition(tabla_entrena1$Dias.para.el.siguiente.abono, 
                                    p = proporcion_entrena, list = FALSE)
  tabla_entrena2 <- tabla_entrena1[ inTraining,]
  n<-nrow(tabla_entrena2)
  var_expl<-ncol(tabla_entrena2)-1
  
  #-Defining data-
  data<-list("n"=n,"var_expl"=var_expl,"y"=tabla_entrena2$Dias.para.el.siguiente.abono)
  for(i in 1:var_expl){
    #i<-1
    data[[i+3]]<-as.array(tabla_entrena2[,i+1]) #es "+3" porque se estan agregando columnas ademas de las 
    # tres que ya existen. Estamos agregando cada una de las variables
    # explicativas que elegimos. En ese caso tenemos el "+1" porque la 
    #primera columna de la tabla_entrena2 es la variable Y
    
  }
  
  for( j in 1:var_expl){
    #j<-1
    names(data)[j+3]<-sprintf("x%i",j)
    
  }
  
  x<-matrix(nrow=n,ncol=var_expl)
  for( j in 1:var_expl){
    
    x[,j]<-data[[j+3]]
    
  }
  
  
  
  data2<-list("n"=n,"var_expl"=var_expl,"y"=tabla_entrena2$Dias.para.el.siguiente.abono,"x"=x)
  #-Defining inits-
  inits<-function(){list(alpha=0.01,beta=c(rep(0,var_expl)),yest=rep(0,n))}
  
  #-Selecting parameters to monitor-
  parameters<-c("alpha","beta","yest")
  #inits()
  
  
  
  modelo<-jags(data2,inits,
               parameters,
               model.file=modelo_geometric_canonic,
               n.iter=iteraciones_jags,
               n.chains=1,n.burnin=calentamiento_jags)
  nombre_modelo = "el modelo geometrico con liga log(1-p)"
  
  
  modelo.summary<-modelo$BUGSoutput$summary
  head(modelo.summary)
  
  modelo.dic<-modelo$BUGSoutput$DIC
  e1<-data.frame(DIC=modelo.dic)
  e1
  
  #if(Concurrente==0){tipo_elec="Federal"}else{tipo_elec="Concurrente"}
  
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
  data_prueba<-list("n"=n2,"var_expl"=var_expl,"y"=tabla_no_entrenada$Dias.para.el.siguiente.abono)
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
  
  
  titulo<-paste("Resultado de aplicar ",nombre_modelo,", utilizando una proporcion del: ",proporcion_entrena*100,
                "% para las operaciones de clientes de clase ", clase_NumeroOperaciones,
                " y utilizando como variables predictoras -> ", var_predic,":")
  
  
  Datos_entrenamiento_exportar<-tabla_resultados1[ inTraining,]
  Datos_prueba_exportar<-tabla_resultados1[ -inTraining,]
  
  return(list("Titulo"=titulo,
              "resumen"=modelo.summary,"DIC"=e1,"matriz_X"=x,"Tabla_no_usada"=tabla_no_entrenada,
              "Clase de cliente"=clase_NumeroOperaciones,
              "variables_explicativas"=var_predic,"tabla_entrena2"=tabla_entrena2,"matriz_X_prueba"=x_prueba,
              "tabla_resultados1"=tabla_resultados1,"vector_variables"=vector_variables,
              "Datos_entrenamiento_exportar"=Datos_entrenamiento_exportar,
              "Datos_prueba_exportar"=Datos_prueba_exportar))
}

plotear<-function(resumen_jags){
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
  
  Miu_dias_estimados<-.99*exp(eta)/(1-.99*exp(eta))
  
  
  tabla_comparativa<-data.frame(Y_Estimadas=y_estimadas$mean,Reales=tabla_entrena2$Dias.para.el.siguiente.abono,
                                Combinacion_Lineal=eta)
  or2<-order(Miu_dias_estimados,decreasing=TRUE)
  #   or3<-order(tabla_comparativa$Reales)
  #   t2<-data.frame(Probabilidad_ausentismo=p,tabla_comparativa)
  
  #   plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas",ylab="Probabilidad de Ausentismo",main=resumen_jags$Entidad)
  #   points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")  
  
  #   plot(p[or2],type="l",ylim=c(0,1.2),xlab="Casillas",ylab="Probabilidad de Ausentismo",main=titulo)
  #   points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")
  
  plot(Miu_dias_estimados[or2],type="l",ylim=c(0,1.2*max(Miu_dias_estimados)),xlab="Clientes-Fecha
       (ordenadas segun el modelo de mayor a menor número de días para el siguiente depósito)",
       ylab="número de días para el siguiente depósito",
       main=paste("Comparativa de número de días para el siguiente depósito del modelo
vs Casos Reales"))
  points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")
  #mtext(paste("(",resumen_jags$variables_explicativas,") ",tipo_de_liga,sep=""))
  
}

si_distintoCero<-function(numero){if(numero!=0){numero<-numero+1} 
  return(numero)}

concatenar_intervalo<-function(vector_de_inf_y_sup){
  
  intervalo<-paste("(",round(vector_de_inf_y_sup[1],2)," , ",round(vector_de_inf_y_sup[2],2),")",sep="")
  return(intervalo)
}

tablas_coeficientes<-function(resumen_jags){
  #resumen_jags<-dataInput
  resumen_<-data.frame(resumen_jags$resumen)
  tabla_entrena2<-data.frame(resumen_jags$tabla_entrena2)
  betas<-resumen_[grep("beta",rownames(resumen_)),]
  coeficientes<-rbind(resumen_[1,],betas)
  coeficientes_final<-subset(coeficientes,select = c("mean","sd","X2.5.","X97.5."))
  names(coeficientes_final)<-c("Media","DE","cuantil 2.5%","cuantil 97.5%")
  names(coeficientes_final)<-c("Media","DE","Limite inferior del intervalo con 95% de probabilidad","Limite Superior del intervalo con 95% de probabilidad")
  
  medias_coef<-coeficientes_final$Media
  
  x<-resumen_jags$matriz_X
  x2<-matrix(cbind(rep(1,nrow(x)),x),ncol=ncol(x)+1)
  
  eta<-numeric(nrow(x))
  for(i in 1:nrow(x2)){
    #i<-1
    for(j in 1:ncol(x2)){
      #j<-1
      eta[i]<-eta[i]+x2[i,j]*medias_coef[j]
      
    }
  }
  
  
  ###Para la construccion de la tabla_resultados1
  #tabla_resultados1<-resumen_jags$tabla_resultados1
  tabla_resultados1<-as.matrix(resumen_jags$tabla_resultados1)
  tabla_resultados2<-matrix(cbind(rep(1,nrow(tabla_resultados1)),tabla_resultados1),ncol=ncol(tabla_resultados1)+1) ##Aqui ponemos 
  #colnames(tabla_resultados1)
  colnames(tabla_resultados2)<-c("constante",colnames(tabla_resultados1))
  ## una columna de 1's a la izquieda (antes) de las variables escogidas
  
  
  eta2<-numeric(nrow(tabla_resultados2))
  for(i in 1:nrow(tabla_resultados2)){
    #i<-1
    for(j in 1:ncol(x2)){       #### Aqui dejamos el x2 porque esa es la que tiene el numero de 
      ####columnas igual al numero de variables que estaremos usando
      #### mientras, que por el contrario, la tabla_resultados_2 tiene todos los campos para
      #### la identificacion de la casilla que no se utilizaran en estos ciclos.
      #j<-1
      #class(tabla_resultados2[i,j])
      eta2[i]<-eta2[i]+as.numeric(tabla_resultados2[i,j])*medias_coef[j]
      
    }
  }
  
  
  
  
  Miu_dias_estimados_entrena<-.99*exp(eta)/(1-.99*exp(eta))
  Miu_dias_estimados_todos<-.99*exp(eta2)/(1-.99*exp(eta2))
  
  #else{
  #     
  #     p<-pnorm(eta)
  #     p_total<-pnorm(eta2)
  #     
  #   }
  
  
  
  
  #tabla_comparativa<-data.frame(Y_Estimadas=y_estimadas$mean,Reales=tabla_entrena2$Ausentismo2,Combinacion_Lineal=eta)
  or2<-order(Miu_dias_estimados_entrena)
  Miu_dias_estimados_2<-Miu_dias_estimados_entrena[or2]
  
  or_total<-order(Miu_dias_estimados_todos,decreasing = TRUE)  ##Para que la tabla quede ordenada de mayor a menor el numero de dias transcurridos
  
  tabla_resultados2_1<-data.frame(tabla_resultados2)
  tabla_resultados2_1$Dias_Estimados_siguiente_Deposito<-Miu_dias_estimados_todos
  tabla_resultados3<-tabla_resultados2_1[or_total,]
  
  tabla_resultados4<-subset(tabla_resultados3,select=c("Contrato","Fecha","Dias.para.el.siguiente.abono","Dias_Estimados_siguiente_Deposito"))
  
  
  #Ahora hagamos la tabla de coeficientes más adecuada. Ademas haremos dos tablas, una que se mostrá
  # y otra que se descarfará
  tabla_med_desest_y_lim_betas<-coeficientes_final
  
  opc2<-data.frame(variables=c("Intercepto",names(g3)[-c(1,2,15)])) #todas las variables
  
  row.names(opc2)<-c("Intercepto",names(g3)[-c(1,2,15)])
  
  data_variables<-data.frame(variables=resumen_jags$vector_variables)
  
  
  opc2$match <- match(opc2$variables,data_variables$variables, nomatch=0)
  match_matrix<-as.matrix(opc2$match)
  
  match_matrix2<-apply(match_matrix,1,FUN=si_distintoCero)
  match_matrix2[1]<-1 #por porner algun numero que ayude a que el cilo de abajo NO olvide tomar en cuenta al
  # Intercepto (alpha)
  opc2$match2<-match_matrix2
  
  betas_media<-numeric(nrow(opc2))
  betas_DesEst<-numeric(nrow(opc2))
  betas_LimInf<-numeric(nrow(opc2))
  betas_LimSup<-numeric(nrow(opc2))
  for (i in 1:nrow(opc2)){
    
    if(opc2$match2[i]!=0){
      betas_media[i]<-tabla_med_desest_y_lim_betas$Media[opc2$match2[i]]
      betas_DesEst[i]<-tabla_med_desest_y_lim_betas$DE[opc2$match2[i]]
      betas_LimInf[i]<-tabla_med_desest_y_lim_betas$`Limite inferior del intervalo con 95% de probabilidad`[opc2$match2[i]]
      betas_LimSup[i]<-tabla_med_desest_y_lim_betas$`Limite Superior del intervalo con 95% de probabilidad`[opc2$match2[i]]
    }
    
  }
  opc2$betas_media<-betas_media
  opc2$betas_DesEst<-betas_DesEst
  opc2$betas_LimInf<-betas_LimInf
  opc2$betas_LimSup<-betas_LimSup
  
  matrix_limites<-matrix(cbind(opc2[[6]],opc2[[7]]),ncol=2)
  
  columna_intervalos<-apply(matrix_limites,1,FUN=concatenar_intervalo)
  opc2$columna_intervalos<-columna_intervalos
  
  names(opc2)[4]<-"Pesos de las variables" 
  names(opc2)[5]<-"Desviación estándar"
  names(opc2)[6]<-"Limite inferior del intervalo con 95% de probabilidad" 
  names(opc2)[7]<-"Limite Superior del intervalo con 95% de probabilidad" 
  names(opc2)[8]<-"Intervalos del 95% de probabilidad"
  
  opc2_1<-subset(opc2,select=c("Pesos de las variables",
                               "Limite inferior del intervalo con 95% de probabilidad",
                               "Limite Superior del intervalo con 95% de probabilidad",
                               "Intervalos del 95% de probabilidad")) #Esto es lo que se exportara con el boton de descargar respectivo
  opc2_2<-subset(subset(opc2_1,select=c("Pesos de las variables","Intervalos del 95% de probabilidad"),
                        `Pesos de las variables`!=0)) #Esto se mostrara en pantalla
  
  
  
  
  
  
  return(list("coeficientes_final"=coeficientes_final,"Periodo_mas_largo_estimado"=Miu_dias_estimados_2[length(Miu_dias_estimados_entrena)],
              "Periodo_mas_largo_real"=max(tabla_entrena2$Dias.para.el.siguiente.abono),
              "Clientes_fechas_con_dias_siguiente_abono"=tabla_resultados3,
              "Clientes_fechas_con_dias_siguiente_abono_compacta"=tabla_resultados4,
              "coeficientes_final_2_1"=opc2_1,"coeficientes_final_2_2"=opc2_2))
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
  Miu_dias_estimados_prueba<-.99*exp(eta)/(1-.99*exp(eta))
  
  tabla_comparativa<-data.frame(Reales=tabla_prueba$Dias.para.el.siguiente.abono,Combinacion_Lineal=eta)
  
  # or2<-order(Miu_dias_estimados_prueba)
  or2<-order(Miu_dias_estimados_prueba,decreasing=TRUE)
  
  
  plot(Miu_dias_estimados_prueba[or2],type="l",ylim=c(0,1.2*max(Miu_dias_estimados_prueba)),xlab="Clientes-Fechas
       (ordenados segun el modelo de menor a mayor estimacion de dias)",ylab="Dias para el siguiente abono",
       main=paste("Comparativa de Dias para el siguiente abono segun el modelo
vs Dias Reales para los datos de prueba"))
  points(tabla_comparativa$Reales[or2],cex=.5,col=2,type="p")
}

plotear_1<-function(){
  
  p <- ggplot(dt_op, aes(x = Numero_operaciones, y = porc_acum)) + geom_point()
  p+labs(title = "Porcentaje acumulado de número de clientes 
         según el número de operacones que tuvo cada uno") + ylab(
           "Cantidad de clientes acumulada en porcentaje")+xlab("Número de operaciones por cliente")
  
}

plotear_2<-function(){
  
  p <- ggplot(dt_op, aes(x = Numero_operaciones, y = porc_acum)) + geom_point()
  p2<-p+aes(colour = Clase_Num_Op) +labs(title = "Porcentaje acumulado de número de clientes 
                                         según el número de operacones que tuvo cada uno") + ylab(
                                           "Cantidad de clientes acumulada en porcentaje")+xlab("Número de operaciones por cliente")
  p2
  
}

plotear_3<-function(){
  
  or3<-order(g0_3_2$Numero_Fechas)
  
  p <- ggplot(g0_3_2, aes(x = Numero_Fechas[or3], y = Numero_operaciones[or3])) + geom_point()
  p +labs(title = "Número de Operaciones por cliente vs
          Número de fechas en las que se realizaron") + ylab(
            "Cantidad de operaciones")+xlab("Número de Fechas")
  
}

plotear_4<-function(){
  
  
  or3<-order(g0_3_2$Numero_Fechas)
  
  p <- ggplot(g0_3_2, aes(x = Numero_Fechas[or3], y = Numero_operaciones[or3])) + geom_point()
  p+aes(colour = Clase_Num_Op) +labs(title = "Número de Operaciones por cliente vs
                                     Número de fechas en las que se realizaron") + ylab(
                                       "Cantidad de operaciones")+xlab("Número de Fechas")
  
}

plotear_5<-function(){
  
  
  or5<-order(g0_3_2$rango_fechas)
  
  p <- ggplot(g0_3_2, aes(x = rango_fechas[or5], y = Numero_operaciones[or5])) + geom_point()
  p+labs(title = "Número de operaciones de acuerdo con el rango de fechas del cliente") +
    ylab("Cantidad de operaciones")+xlab("Número de días entre el primer y el último abono resistrado")
  
}

plotear_6<-function(){
  
  
  or5<-order(g0_3_2$rango_fechas)
  
  p <- ggplot(g0_3_2, aes(x = rango_fechas[or5], y = Numero_operaciones[or5])) + geom_point()
  p+aes(colour = Clase_Num_Op) +labs(title = "Número de operaciones de acuerdo con el rango de fechas del cliente") +
    ylab("Cantidad de operaciones")+xlab("Número de días entre el primer y el último abono resistrado")
  
}

plotear_7<-function(){
  
  g0_3_3<-subset(g0_3_2,Suma_Monto<1000000)
  or7<-order(g0_3_3$Suma_Monto)
  
  p <- ggplot(g0_3_3, aes(x = Suma_Monto[or7], y = Clientes_con_ese_num_op[or7])) + geom_point()
  p +labs(title = "Número de Clientes de acuerdo a las operaciones que realizaron y los monots que abonaron") +
    ylab("Número de Clientes de acuerdo a las operaciones que realizaron")+xlab("Montos totales por cliente")
  
}

plotear_8<-function(){
  
  g0_3_3<-subset(g0_3_2,Suma_Monto<1000000)
  or7<-order(g0_3_3$Suma_Monto)
  
  p <- ggplot(g0_3_3, aes(x = Suma_Monto[or7], y = Clientes_con_ese_num_op[or7])) + geom_point()
  p+aes(colour = Clase_Num_Op) +labs(title = "Número de Clientes de acuerdo a las operaciones que realizaron y los monots que abonaron") +
    ylab("Número de Clientes de acuerdo a las operaciones que realizaron")+xlab("Montos totales por cliente")
  
}

plotear_9<-function(){
  
  g0_3_3<-subset(g0_3_2,Suma_Monto<1000000)
  or7<-order(g0_3_3$Numero_Fechas)
  
  p <- ggplot(g0_3_3, aes(x = Numero_Fechas[or7], y = Suma_Monto[or7])) + geom_point()
  p +labs(title = "Cantidad de dinero depositado por número de fechas con actividad") +
    ylab("Número de Clientes de acuerdo a las operaciones que realizaron")+xlab("Montos totales por cliente")
  
}

plotear_10<-function(){
  
  g0_3_3<-subset(g0_3_2,Suma_Monto<1000000)
  or7<-order(g0_3_3$Numero_Fechas)
  
  p <- ggplot(g0_3_3, aes(x = Numero_Fechas[or7], y = Suma_Monto[or7])) + geom_point()
  p+aes(colour = Clase_Num_Op) +labs(title = "Cantidad de dinero depositado por número de fechas con actividad") +
    ylab("Número de Clientes de acuerdo a las operaciones que realizaron")+xlab("Montos totales por cliente")
  
 
}




################################################################
shinyServer(function(input, output,session) {

  tabla<-tabla()
  #g3<-g3()
  #Abonos<-Abonos()
  #g0_3_2<-g0_3_2()
  #dt_op<-dt_op()
  #g0_3<-g0_3()
  #g00_1<-g00_1()
  
  
  #### Panel 1
  
  output$tabla0_1<-renderTable({
    
    g00_1
    #tablasInput$coeficientes_final_2_1
  })
  
  output$plot0_1 <- renderPlot({
    
    plotear_9()
    
  })
  
  #### Panel 2
  
  output$plot1_1 <- renderPlot({
    
    plotear_1()
    
  })
  
  output$plot1_3 <- renderPlot({
    
    plotear_3()
    
  })
  
  output$plot1_5 <- renderPlot({
    
    plotear_5()
    
  })
  
  output$plot1_7 <- renderPlot({
    
    plotear_7()
    
  })
  
  #### Panel 3
  
  output$plot3_10 <- renderPlot({
    
    plotear_10()
    
  })
  
  output$plot2_2 <- renderPlot({
    
    plotear_2()
    
  })
  
  output$plot2_4 <- renderPlot({
    
    plotear_4()
    
  })
  
  output$plot2_6 <- renderPlot({
    
    plotear_6()
    
  })
  
  output$plot2_8 <- renderPlot({
    
    plotear_8()
    
  })
  
  
  #### Panel 4
dataInput <- reactive({
  
    preparar_datos(tabla_datos=tabla,clase_NumeroOperaciones=input$clase_NumeroOperaciones,
                   proporcion_entrena = input$Proporcion,
                   vector_variables=input$checkGroup,
                   iteraciones_jags=input$iteraciones_jags,
                   calentamiento_jags = input$CalentamientoJags)
  
})

#dataInput<-resumen_jags

tablasInput <- reactive({
  
    tablas_coeficientes(dataInput())
    #tablasInput<-tablas_coeficientes(dataInput)
})

plotInput <- reactive({

      plotear(dataInput())
      #plotInput<-plotear(dataInput)
})

output$plot1 <- renderPlot({
  print(plotInput())
})

##Ploteo de los datos de prueba con el modelo

plotInput2 <- reactive({
  
    plotear_datos_prueba(dataInput())
    #plotInput2<-plotear_datos_prueba(dataInput)
})

output$plot2 <- renderPlot({
  print(plotInput2())
})

output$sumario<-renderPrint({
  sumario<-dataInput()$Titulo
  #sumario<-dataInput$Titulo
  sumario
})

output$tabla_resumen<-renderTable({
  
  tablasInput()$coeficientes_final
  #tablasInput$coeficientes_final
})

output$tabla_resumen_2_1<-renderTable({
  
  tablasInput()$coeficientes_final_2_1
  #tablasInput$coeficientes_final_2_1
})

output$tabla_resumen_2_2<-renderTable({
  
  tablasInput()$coeficientes_final_2_2
  #tablasInput$coeficientes_final_2_2
})

output$tabla_final_clientes_fechas_ordenados<-renderDataTable({
  
  tablasInput()$Clientes_fechas_con_dias_siguiente_abono_compacta
  
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

output$downloadData1 <- downloadHandler(
  filename = function() { 
    paste(input$clase_NumeroOperaciones,"_Clientes_fechas_con_numero_dias_siguiente_abono_",dataInput()$variables_explicativas,100*input$Proporcion,
          "-",100*(1-input$Proporcion),'.csv', sep='') 
  },
  content = function(file) {
    write.csv(tablasInput()$Clientes_fechas_con_dias_siguiente_abono, file)
  }
)

output$downloadData2 <- downloadHandler(
  filename = function() { 
    paste(input$clase_NumeroOperaciones,"_Pesos_variables_",dataInput()$variables_explicativas,100*input$Proporcion,
          "-",100*(1-input$Proporcion),'.csv', sep='') 
  },
  content = function(file) {
    write.csv(tablasInput()$coeficientes_final_2_1, file)
  }
)

output$downloadData3 <- downloadHandler(
  filename = function() { 
    paste(input$clase_NumeroOperaciones,"_Datos_Entrenamiento_",dataInput()$variables_explicativas,100*input$Proporcion,
          "-",100*(1-input$Proporcion),'.csv', sep='') 
  },
  content = function(file) {
    write.csv(dataInput()$Datos_entrenamiento_exportar, file)
  }
)

output$downloadData4 <- downloadHandler(
  filename = function() { 
    paste(input$clase_NumeroOperaciones,"_Datos_Prueba_",dataInput()$variables_explicativas,100*input$Proporcion,
          "-",100*(1-input$Proporcion),'.csv', sep='') 
  },
  content = function(file) {
    write.csv(dataInput()$Datos_prueba_exportar, file)
  }
)

comparativaInput<-reactive({

    dataset<-data.frame(DIC=dataInput()$DIC,
                        Periodo_mas_largo_de_deposito_estimado=tablas_coeficientes(dataInput())$Periodo_mas_largo_estimado,
                        Promedio_real_dias_siguiente_abono=tablas_coeficientes(dataInput())$Periodo_mas_largo_real)
    names(dataset)<-c("DIC","Periodo estimado mas largo entre depósitos",
                      "Periodo real más largo entre depósitos en los datos analizados")
    dataset

    
    
})


output$comparativa<-renderTable({
  print(comparativaInput())
})

})
