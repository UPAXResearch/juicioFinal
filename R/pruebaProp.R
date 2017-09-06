#'Realiza prueba de proporciones sobre un data frame (una tabla) que sea un recuento o porcentaje de frecuencia. NECESITA UNA FILA DE "Total"
#'En total debe ir el tamaño de -la base-. Revisar ejemplos para que sea más claro.
#'
#'Realiza un prop.test con hipótesis alternativa "mayor qué" sobre un data frame tal que sea un resultado de frecuencias.
#'La primera columna de la tabla (tabla[,1]) debe ser de las respuestas de la frecuencia
#'La última fila del data frame debe ser el "Total" (de ahí obtiene n el prop.test)
#'@param tablaProp La tabla principal
#'@param simboloPct Los datos tienen simbolo de porcentaje?  A veces al leer un .csv viene el símbolo de porcentaje
#'@param echo Mantener los datos originales?
#'@export
#'@keywords prop
#'@examples pruebaProp(data.frame(nombres=c("Uno","Dos","Total"),Variable1=c(sample(1:200,1),sample(1:200,1),sample(201:400,1)),Variable2=c(sample(1:200,1),sample(1:200,1), sample(201:400,1))))
#'@examples pruebaProp(data.frame(nombres=c("Uno","Dos","Total"),Variable1=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""),sample(1:400,1)),Variable2=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""), sample(1:400,1))),simboloPct=T)
#'@examples pruebaProp(data.frame(nombres=c("Uno","Dos","Total"),Variable1=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""),"100%"),Variable2=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""), "100%")),simboloPct=T)
#'@examples pruebaProp(data.frame(nombres=c("Uno","Dos","Total"),Variable1=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""),"100%"),Variable2=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""), "100%")),simboloPct=T,echo=T)

pruebaProp <- function(
  # La tabla de datos....
  # La primera columna es la respuesta
  # La última fila debe ser la del total
  tablaProp,
  simboloPct=F,
  echo=F
){

  # consigueProp(tablaProp = listado[[i]],simboloPct = T)
  #
  # tablaProp <- data.frame(nombres=c("Uno","Dos","Total"),Variable1=c(sample(1:400,1),sample(1:400,1),sample(1:400,1)),Variable2=c(sample(1:400,1),sample(1:400,1), sample(1:400,1)))
  # tablaProp <- data.frame(nombres=c("Uno","Dos","Total"),Variable1=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""),sample(1:400,1)),Variable2=c(paste(sample(1:100,1),"%",sep=""),paste(sample(1:100,1),"%",sep=""), sample(1:400,1)))
  # simboloPct <- F
  # tablaProp <- testin
# echo=T
  ################################################# Supuestos...

  # Mi primer columna, es texto
  if(simboloPct){
    final <- as.data.frame(sapply(tablaProp[2:length(tablaProp)], function(x){as.character(x)}),stringsAsFactors = F)
    final <- as.data.frame(sapply(final, function(x){strsplit(x,split = "%")}),stringsAsFactors = F)
    final <- as.data.frame(sapply(final, function(x){as.numeric(as.character(x))}),stringsAsFactors = F)
    
    # Tengo porcentajes, debo recalcular sobre la base, que se supone que es la última fila
    tablaSPMirror <- final[nrow(final),]
    final <- final[-nrow(final),]
    # Why? because fu thats why
    final <- do.call("rbind",lapply(apply(final,1,function(x){(x/100)*tablaSPMirror}), as.data.frame))
    final <- rbind(final, tablaSPMirror)
  }else{
    final <- as.data.frame(sapply(tablaProp[2:length(tablaProp)], function(x){as.numeric(as.character(x))}))
  }

  tablaSPMirror <- final

  for(spi in 1:(nrow(final)-1)){
    # Voy por el primer row
    # spi<-1
    if(!echo){
      tablaSPMirror[spi,] <- ""
    }
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

  rowNombres <- tablaProp[,1]
  nombreN <- names(tablaProp)[1]

  tablaProp <- cbind(tablaProp[,1],tablaSPMirror)
  tablaProp[,1] <- rowNombres
  names(tablaProp)[1] <- nombreN
  return(tablaProp)
}
