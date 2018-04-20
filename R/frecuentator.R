#'Función para frecuencias y pruebas de proporciones
#'
#'Esta función obtiene frecuencias, ponderadas o no, y pruebas de proporciones para tablas tipo encuesta. La funcion necesita el paquete survey.
#'@param fTtabla La tabla principal i.e. "la base de datos"
#'@param fTvariables La variable o nombres de variables de quien se va a extraer informacion (i.e. frecuencias)
#'@param fTlevels Uso los levels de las variables en fvariables? TRUE= Uso los levels, FALSE= uso los nombres de las variables, i.e. FALSE= mi variable(s) son lógicas
#'@param fbanner Las variables que van por banner, en caso de que se necesite
#'@param fTponderador Nombre de la variable ponderador, en caso de que exista
#'@param fTsobreQuien Fijar un total para todos los cálculos de porcentaje
#'@param fTtotal Agregar una fila de total en los resultados finales?
#'@param fTprop Hacer prueba de proporciones? En vez de regresar la tabla de frecuencias se regresa una tabla de prueba de proporciones (igual a las de SPSS)
#'@param fTusarNA Frecuentator omite por default los NA de las variables que le pedimos, pero a veces se necesitan i.e. Cuando agrupo variables para Share of Mind
#'@param fTdecimales El redondeo de porcentaje a cuantos decimales debe ser? El default es 1
#'@export
#'@keywords frecuencias
#'@examples
#'frecuentator(fTtabla = iris,fTvariables = "Species",fTlevels = T,fbanner = "Species")

frecuentator<- function(
  #La tabla principal...
  fTtabla,
  #Las variables por las que va a cruzar...
  fTvariables,
  #Uso los levels de las variables en fvariables?? TRUE= Uso los levels, FALSE= uso los nombres de las variables, i.e. FALSE= mi variable(s) son lógicas
  fTlevels=T,
  #Las variables del banner
  fbanner=NULL,
  #El ponderador
  fTponderador=NULL,
  #Sobre quien voy a obtener los porcentajes? i.e. cual es el tamaño de mi base?
  fTsobreQuien= NULL,
  #Agregar una fila de total?
  fTtotal=T,
  #Hago prueba de proporciones?
  fTprop=F,
  #Utilizo los NA en el cálculo de porcentajes?
  fTusarNA=F,
  #Cuantos decimales al momento de redondear los porcentajes?
  fTdecimales=4
){
  # list.of.packages <- c("survey")
  # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  # if(length(new.packages)) install.packages(new.packages)
  # #################################################
  # frecuentator(fTtabla = datos,fTvariables = c('tom','sh1','sh2',
  #                                              'sh3','sh4','sh5',
  #                                              'sh6','sh7','ay1',
  #                                              'ay2','ay3','ay4',
  #                                              'ay5','ay6','ay7'),
  #              fTlevels = T,fbanner = bandera,
  #              fTponderador = 'ponderador',fTprop = T)
  #
  # #
  # fTtabla<-datos
  # fTvariables<-c('tom','sh1','sh2',
  #                'sh3','sh4','sh5',
  #                'sh6','sh7','ay1',
  #                'ay2','ay3','ay4',
  #                'ay5','ay6','ay7')
  # fTlevels<-T
  # fbanner <- bandera
  # fTponderador<-'ponderador'
  # fTsobreQuien<- NULL
  # fTtotal<-T
  # fTprop<-T
  # # #
  # # ############## truquitos
  # #   fTvariables<-c("NSE")
  # fTtabla$Segmentos <- sample(size = nrow(fTtabla),x = 1:6,replace = T)
  # fTtabla$A9.Hijo.1.a.os<- factor(fTtabla$A9.Hijo.1.a.os)
  # fTtabla$Segmentos<- factor(fTtabla$Segmentos)
  #   # fTtabla$sincasos<-1
  #   # fTtabla$sincasos[1]<-2
  #   #
  #   # fTtabla$sincasos<- factor(fTtabla$sincasos,labels = c("FALSE","b"))
  #   # fTponderador<-"pond"
  #   # fTtabla$pond[1]<-NA
  #   # fTtabla$P1.Banamex[17]<-NA
  # ############## truquitos FIN

  if(is.null(fbanner)){
    fTtabla$Total<- factor(rep(x = 1,nrow(fTtabla)),levels = 1,labels = "Total")
    fbanner<-"Total"
  }
  misVarz <- c(fTvariables,fbanner)
  ################################################# Supuestos...
  ################################################# Supuestos...
  ################################################# Supuestos...
  ################################################# Supuestos...

  # Existen los nombres de variables?
  if(!all(misVarz %in% names(fTtabla))){
    stop(paste("\n frecuentator Error01: No existe la variable ::",paste(misVarz[!misVarz %in% names(fTtabla)],collapse = ", "),":: en fTtabla"))
  }
  # Subseteo
  fTtabla<-subset(x = fTtabla,select = c(misVarz,fTponderador))
  # Tengo ponderador? Debo revisar que mi ponderador no tenga casos diferentes de número...
  if(is.null(fTponderador)){
    # Si no tengo ponderador hago uno artificial...
    fTponderador<- "pTemp"
    fTtabla[,fTponderador]<-1
  }else{
    # Tenemos ponderador
    if(!all(!is.na(fTtabla[,fTponderador]))){
      #Tengo NA en el ponderador...
      stop(paste("\n frecuentator Error02: El ponderador ::",fTponderador,":: tiene valores NA en la observación : ",(1:nrow(fTtabla))[is.na(fTtabla[,fTponderador])]," en fTtabla"))
    }
    if(!is.numeric(fTtabla[,fTponderador])){
      # Mi ponderador no es una variable numerica
      stop(paste("\n frecuentator Error03: El ponderador ::",fTponderador,":: no es numerico"))
    }
  }
  # Voy a probar que no tenga NA en las varaibles. Si toda la variable tiene NA, la voy a omitir y luego
  #   debo probar que aun tengo variables...
  if(!all(!sapply(X = fTtabla[,fTvariables],is.na))){
    # warning(paste("\n\n frecuentator Advertencia: hay NA en la variable ::",fTvariables[sapply(X = fTtabla[,fTvariables],FUN = function(X){!all(!is.na(X))})],":: en fTtabla \n voy a intentar corregirlo"))
    vars<- sapply(X = fTtabla,FUN = function(X){!all(!is.na(X))})
    for(vi in 1:length(names(vars)[vars])){
      # vi<-1
      if(all(is.na(fTtabla[,names(vars)[vars][vi]]))){
        warning(paste("\n frecuentator Advertencia: La variable ::",names(vars)[vars][vi],":: en fTtabla está llena de NA, voy a omitirla del análisis"))
        fTvariables<- fTvariables[-which(names(vars)[vars][vi]==fTvariables)]
      }
    }
  }
  if(length(fTvariables)==0){
    warning("\n frecuentator Error04: Se han omitido todas las variables del análisis porque estaban vacias, frecuencia no tiene sentido")
    return(data.frame(vacio="sin casos"))
  }
  # Pruebo fTlevels
  # Requiero que, si fTlevels=T, entonces todas las variables deben tener levels
  #   Si fTlevels entonces mi variable(s) objetivo es categórica
  if(fTlevels){
    # Tengo variable categórica. Debo revisar que todas tengan levels...
    if(!all(sapply(X = fTtabla[,misVarz],is.factor))){
      stop(paste("\n frecuentator Error05: No es factor la variable ::",paste(misVarz[!sapply(X = fTtabla[,misVarz],is.factor)],collapse = ", "),":: en fTtabla"))
    }
    # Ahora debo revisar que todas tengan los mismos levels...y que sean más de uno
    if(!all(sapply(X = fTtabla[,fTvariables], FUN = function(x){length(levels(x))>1}))){
      stop(paste("\n frecuentator Error06: La variable ::",paste(fTvariables[!sapply(X = fTtabla[,fTvariables], FUN = function(x){length(levels(x))>1})],collapse = ", "),":: tiene menos de 1 factor"))
    }
    for(vi in 1: length(fTvariables)){
      if(!all(levels(fTtabla[,fTvariables[1]])==levels(fTtabla[,fTvariables[vi]]))){
        # Tengo levels diferentes... lo siento mucho, no puedo proseguir así...
        stop(paste("\n frecuentator Error07: La variable ::",fTvariables[vi],":: tiene levels diferentes de la variable ::",fTvariables[1],"::"))
      }
    }
  }else{
    # Mi variable es lógica
    # Si mi variable no es lógica, forzarla...
    if(!all(sapply(X = fTtabla[,fTvariables],is.logical))){
      warning(paste("\n frecuentator Advertencia: No es logic la variable ::",paste(fTvariables[!sapply(X = fTtabla[,fTvariables],is.logical)],collapse = ", "),":: se forzará a logic"))
      if(
        is.factor(fTtabla[,fTvariables[1]])
      ){
        # Cuando uso datos desde un .sav (i.e. etiquetados) necesito convertirlos a logicos desce "character"
        fTtabla[,fTvariables]<-as.logical(apply(X = subset(fTtabla,select = fTvariables),MARGIN = 2,FUN = as.character))
      }else{
        # Cuando uso datos desde un csv (i.e. no etiquetados) necesito convertirlos a logicos desce "numeric"
        fTtabla[,fTvariables]<- sapply(X = subset(fTtabla,select = fTvariables),FUN = as.logical)
      }
      # sapply(X = fTtabla[,fTvariables],FUN = function(X){X[is.na(X)]<-F})
      fTtabla[is.na(fTtabla)]<-F
      #Ahora debo revisar que no haya NA en las variables...
      if(!all(!sapply(X = fTtabla[,fTvariables],is.na))){
        stop(paste("\n frecuentator Error07: Hubo un problema con la conversión a logic, hay NA en la variable ::",fTvariables[sapply(X = fTtabla[,fTvariables],FUN = function(X){!all(!is.na(X))})],":: en fTtabla"))
      }
    }
  }
  misVarz <- c(fTvariables,fbanner)
  fTtabla<- fTtabla[,c(misVarz,fTponderador)]
  #
  #
  #
  #
  #
  #
  #
  #
  ################################################# Supuestos Verificados
  ################################################# Supuestos Verificados
  ################################################# Supuestos Verificados
  ################################################# Supuestos Verificados
  ################################################# Supuestos Verificados
  #
  #
  #
  #
  #
  #
  #
  #

  if(fTlevels){
    FINAL<-data.frame()
    FINAL<- data.frame(Respuesta=1:length(levels(fTtabla[,fTvariables[1]])))
    FINAL$Respuesta<-levels(fTtabla[,fTvariables[1]])
    # if(fTtotal){
    FINAL[nrow(FINAL)+1,"Respuesta"] <- "Total"
    # }
    row.names(FINAL) <- FINAL$Respuesta
    # cat("\n\n\n\n\n\n\n\n INICIO PROCESO \n\n",
    #     "\nVariables: ",fTvariables,
    #     "\nBanner: ",fbanner,
    #     "\nTiempo: ",as.character(Sys.time()),
    #     "\n"
    # )
  }else{
    FINAL<-data.frame(Respuesta=fTvariables,stringsAsFactors = F)
    FINAL[nrow(FINAL)+1,"Respuesta"] <- "Total"
    row.names(FINAL) <- FINAL$Respuesta
  }
  cat("\n Consiguiendo frecuencias...")
  pb <- txtProgressBar(min = 0, max = length(fbanner), style = 3)
  for(fi in 1:length(fbanner)){
    # fi <- 1
    setTxtProgressBar(pb, fi)
    fbannerMini<- fbanner[fi]
    # cat("\nProcesando Variable ",fbannerMini,"(",fi," de ",length(fbanner),"): ")
    # Me quedo con los levels (las respuestas de cada variable banner a evaluar)
    firespuestas<-levels(fTtabla[,fbannerMini])
    # Para cada respuesta i.e. level de la variable banner a evaluar...
    for(ft in 1: length(firespuestas)){
      # ft <- 1
      # Cual es la respuesta que estoy evaluando...
      factual<-firespuestas[ft]
      # cat("\n|| Calculando respuesta  ",factual,"(",ft," de ",length(firespuestas),")")
      # Subseteo a ftabla
      suba<-fTtabla[fTtabla[,fbannerMini]==factual & !is.na(fTtabla[,fbannerMini]),]
      final<-data.frame()
      ################################################# Frecuencia
      for(zi in 1:length(fTvariables)){
        # zi <-5
        sub<-suba[!is.na(suba[,fTvariables[zi]]),]
        if(nrow(sub)<=1){
          # Tengo un solo caso, no puedo ponderaro (sin sentido)
          if(fTlevels){
            # Estoy trabajando con nominales
            a<-as.data.frame(table(sub[,fTvariables[zi]]))
            row.names(a)<- a$Var1
            # Aqui le moví para que cuando tenga un caso, me dé la suma del ponderador DE LA SUBBASE 'sub'
            a[a$Freq==1,"Freq"] <- sum(sub[,fTponderador])
            a$Var1<-a$Freq
            names(a)<-c("total","SE")
            if(nrow(final)==0){
              final<-a
            }else{
              final$total<-final$total+a$total
            }
            row.names(final)<- levels(sub[,fTvariables[1]])
          }else{
            # Estoy trabajando con lógicos
            a<-as.data.frame(table(sub[,fTvariables[zi]]))
            row.names(a)<- a$Var1
            # Obliga a usar el valor de ponderación cuando un solo caso
            if (nrow(sub) == 1) {
              a$Freq<- suba[ !is.na(suba[, fTvariables[zi]]),fTponderador]
            }
            a$Var1<-a$Freq
            names(a)<-c("total","SE")
            if("TRUE" %in% row.names(a)){
              # tengo un true...
              row.names(a) <- fTvariables[zi]
            }else{
              a <- data.frame(total=0,SE=0)
              row.names(a) <- fTvariables[zi]
            }
            if(nrow(final)==0){
              final<-a
            }else{
              final<-rbind(final,a)
            }
            # row.names(final)<- row.names(a)
          }
          if(nrow(final)!=0){
            names(final)<-c("total","SE")
          }
          # final$total <- final$SE
        }else{
          # Pondero todos mis resultados
          z<- survey::svydesign(data = sub,ids =~1, weights = sub[,fTponderador])
          # Estoy trabajando con lógicos? i.e. cómo voy a juntar las variables?
          if(fTlevels){
            # Estoy trabajando con nominales
            a<-data.frame(survey::svytotal(as.formula(paste("~",fTvariables[zi],sep="")),z,na.rm=T))
            if(nrow(final)==0){
              final<-a
            }else{
              final<-final+a
            }
            row.names(final)<- levels(sub[,fTvariables[1]])
          }else{
            # Estoy trabajando con lógicos
            # zi <- 1
            a<-data.frame(survey::svytotal(as.formula(paste("~",fTvariables[zi],sep="")),z,na.rm=T))[2,]
            ### Cuanto quieras quitar el "TRUE" de las respuestas, quita el comment a esto...
            row.names(a) <- fTvariables[zi]
            final<-rbind(final,a)
          }
        }
        if(is.null(fTsobreQuien)){
          # Por default calcula porcentajes sobre el total de casos válidos, en caso contrario, utiliza el recuento de la base
	  if(fTusarNA==T){
		sobreQuienFinal<- sum(suba[,fTponderador])
		}else{
		sobreQuienFinal<- sum(sub[,fTponderador])
		}
        }else{
          sobreQuienFinal<- fTsobreQuien
        }
      }

      final <- rbind(final, data.frame(total=sobreQuienFinal, SE=sobreQuienFinal,row.names = c("Total")))
      # }
      final<-data.frame(
        respuesta=row.names(final),
        f=final$total,
        pct=round(final$total/sobreQuienFinal*100,fTdecimales)
      )
      row.names(final) <- final$respuesta
      final <- final[,-1]
      ################################################# Frecuencia
      names(final) <- paste(fbannerMini, factual,names(final),sep=":::")
      if(nrow(final)==1){
        if(final[,1]==0){
          # No tenía casos... ._.
          repeat{
            if(nrow(FINAL)>nrow(final)){
              cat("\nEntré al do while... wish me luck \n")
              final <- rbind(final, final[1,])
            }else{
              break
            }
          }
          row.names(final) <- row.names(FINAL)
        }
      }
      # cat(paste("...FYI, nrow=",nrow(FINAL)))
      FINAL <- merge(FINAL, final,0)
      row.names(FINAL) <- FINAL$Row.names
      FINAL <- FINAL[,-1]
    }
  }
  close(pb)
  if(fTlevels){
    FINAL <- FINAL[match(c(levels(fTtabla[,fTvariables[1]]),"Total"),row.names(FINAL)),]
  }else{
    FINAL <- FINAL[match(c(fTvariables,"Total"),row.names(FINAL)),]
  }
  if(fTprop){
    # Obtener prueba de proporciones...
    FINALmirror<- data.frame(Respuesta=FINAL$Respuesta)
    cat("\n Consiguiendo pruebas de prop...")
    pb <- txtProgressBar(min = 0, max = length(fbanner), style = 3)
    for(pi in 1:length(fbanner)){
      # pi <- 1
      setTxtProgressBar(pb, fi)
      final<-FINAL
      fbannerMini<- fbanner[pi]
      # cat("\nProcesando prop.test de Variable ",fbannerMini,"(",pi," de ",length(fbanner),"): ")
      firespuestas<-levels(fTtabla[,fbannerMini])
      final <- subset(final,select = grep(pattern = fbannerMini,x = names(final)))
      final <- subset(final,select = grep(pattern = ":::f",x = names(final)))
      tablaSPMirror <- final
      for(spi in 1:(nrow(final)-1)){
        # Voy por el primer row
        # spi<-1
        tablaSPMirror[spi,] <- ""
        for(spt in 1:length(final)){
          # Voy por cada columna...
          # spt<-3
          # for(spw in (1:length(final))[!1:length(final) %in% spt]){
          for(spw in 1:length(final)){
            # spw<-1
            #Sólo cuando estoy evaluando diferentes columnas
            if(spt!=spw){
              objetivo<-round(final[spi,spt],0)
              objetivoTotal<-round(final[nrow(final),spt],0)
              competidor<-round(final[spi,spw],0)
              competidorTotal<-round(final[nrow(final),spw],0)
              if(objetivo>objetivoTotal){objetivo <- objetivoTotal}
              if(competidor>competidorTotal){competidor <- competidorTotal}
              if(objetivo>0 & competidor>0 & objetivo!=objetivoTotal & competidor != competidorTotal){
                if(prop.test(
                  # Exitos
                  x = c(
                    objetivo, competidor
                  ),
                  n = c(
                    objetivoTotal,competidorTotal
                  ),
                  alternative = "greater",
                  correct = T
                )$p.value<0.05){
                  tablaSPMirror[spi, spt]<-paste(tablaSPMirror[spi, spt]," ",LETTERS[spw]," ",sep="")
                }else{
                  tablaSPMirror[spi, spt]<-paste(tablaSPMirror[spi, spt],"",sep="")
                }
              }
            }
          }
        }
      }
      names(tablaSPMirror) <- paste(names(tablaSPMirror),"(",LETTERS[1:length(tablaSPMirror)],")",sep = "")
      FINALmirror <- cbind(FINALmirror,tablaSPMirror)
    }
    return(FINALmirror)
  }
  close(pb)
  return(FINAL)
}
