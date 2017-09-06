#'Función para basada en frecuentator para sacar promedios
#'
#'Esta función obtiene promedios, ponderados o no
#'@param fTtabla La tabla principal i.e. "la base de datos"
#'@param fTvariable La variable de quien se va a extraer informacion (i.e. promedio)
#'@param fbanner Las variables que van por banner, en caso de que se necesite
#'@param fTponderador Nombre de la variable ponderador, en caso de que exista
#'@param fTdecimales El redondeo de porcentaje a cuantos decimales debe ser? El default es 1
#'@export
#'@keywords promedio
#'@examples
#'turboP3 <- lapply(misP3, function(x){promedietor(fTtabla = datos,fTvariable = x,fTdecimales = 2)})

promedietor <- function(
  fTtabla,
  fTvariable,
  fbanner=NULL,
  fTponderador=NULL,
  fTdecimales=1
){
  if(is.null(fbanner)){
    fTtabla$Total<- factor(rep(x = 1,nrow(fTtabla)),levels = 1,labels = "Total")
    fbanner<-"Total"
  }
  misVarz <- c(fTvariable,fbanner)
  # ----------------------------------------- 
  # Supuestos...
  
  # Existen los nombres de variables?
  if(!all(misVarz %in% names(fTtabla))){
    stop(paste("\n Error01: No existe la variable ::",paste(misVarz[!misVarz %in% names(fTtabla)],collapse = ", "),":: en fTtabla"))
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
      stop(paste("\n Error02: El ponderador ::",fTponderador,":: tiene valores NA en la observación : ",(1:nrow(fTtabla))[is.na(fTtabla[,fTponderador])]," en fTtabla"))
    }
    if(!is.numeric(fTtabla[,fTponderador])){
      # Mi ponderador no es una variable numerica
      stop(paste("\n Error03: El ponderador ::",fTponderador,":: no es numerico"))
    }
  }
  # Voy a probar que no tenga NA en las varaibles. Si toda la variable tiene NA, la voy a omitir y luego
  #   debo probar que aun tengo variables...
  
  if(all(is.na(fTtabla[,fTvariable]))){
    # Toda la variable está llena de NA.... 
    warning("\n La variable objetivo está llena de NA")
    return(data.frame(vacio="sin casos"))
  }
  
  if(!all(sapply(X = fTtabla[,fbanner],is.factor))){
    stop(paste("\n No es factor la variable ::",paste(misVarz[!sapply(X = fTtabla[,fbanner],is.factor)],collapse = ", "),":: en fTtabla"))
  }
  
  misVarz <- c(fTvariable,fbanner)
  
  if(!is.numeric(fTtabla[,fTvariable])){
    stop(paste("\n No es numeric la variable ::",fTvariable,":: en fTtabla"))
  }
  
  # ----------------------------------------- 
  # Supuestos Verificados
  FINAL<-data.frame(Respuesta=fTvariable,stringsAsFactors = F)
  FINAL[nrow(FINAL)+1,"Respuesta"] <- "Total_Casos_Base"
  row.names(FINAL) <- FINAL$Respuesta
  
  pb <- txtProgressBar(min = 0, max = length(fbanner), style = 3)
  for(fi in 1:length(fbanner)){
    # fi <- 3
    setTxtProgressBar(pb, fi)
    fbannerMini<- fbanner[fi]
    # Me quedo con los levels (las respuestas de cada variable banner a evaluar)
    firespuestas<-levels(fTtabla[,fbannerMini])
    # Para cada respuesta i.e. level de la variable banner a evaluar...
    for(ft in 1: length(firespuestas)){
      # ft <- 2
      # Cual es la respuesta que estoy evaluando...
      factual<-firespuestas[ft]
      # Subseteo a ftabla
      suba<-fTtabla[fTtabla[,fbannerMini]==factual & !is.na(fTtabla[,fbannerMini]),]
      final<-data.frame()
      ################################################# Promedio
      sub<-suba[!is.na(suba[,fTvariable]),]
      if(nrow(sub)==0){
        # no tengo casos..
        a <- data.frame(mean=0,SE=0)
        row.names(a)<- 0
      }else if(nrow(sub)==1){
        # Tengo un caso...
        a<-as.data.frame(table(sub[,fTvariable]),stringsAsFactors = F)
        a$Var1 <- as.numeric(a$Var1)
        row.names(a)<- a$Var1
        # Aqui le moví para que cuando tenga un caso, me dé la suma del ponderador DE LA SUBBASE 'sub'
        # a[a$Freq==1,"Freq"] <- sum(sub[,fTponderador])
        # a$Var1<-a$Freq
        names(a)<-c("total","SE")
      }else{
        # Pondero todos mis resultados
        z<- survey::svydesign(data = sub,ids =~1, weights = sub[,fTponderador])
        a<-data.frame(survey::svymean(as.formula(paste("~",fTvariable,sep="")),z,na.rm=T))
      }
      final<-a
      row.names(final)<- fTvariable
      
      sobreQuienFinal<- sum(sub[,fTponderador])
      names(final) <- c("mean","SE")
      
      
      final <- rbind(final, data.frame(mean=sobreQuienFinal, SE=sobreQuienFinal,row.names = c("Total_Casos_Base")))
      final <- subset(x = final,select = "mean")
      final$mean=round(final$mean,fTdecimales)
      
      ################################################# Frecuencia
      names(final) <- paste(fbannerMini, factual,names(final),sep=":::")
      
      FINAL <- merge(FINAL, final,0)
      row.names(FINAL) <- FINAL$Row.names
      FINAL <- FINAL[,-1]
    }
  }
  close(pb)
  return(FINAL)
}
