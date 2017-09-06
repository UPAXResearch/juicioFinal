#'Agrupa resultados "numericos" de un data.frame de frecuentator
#'
#'Dado un data.frame generado por frecuentator, tal que la respuesta sea númerica, es posible agruparla en rangos con esta función
#'@param rtarget El data frame generado por frecuentator
#'@param rrangos Un data frame que tiene definidos los rangos en que se va a agrupar cada fila de rtarget
#'@export
#'@keywords agrupar
#'@examples
#' drangos1 <- data.frame(de=c(0,1,5,10,15,20),a=c(1,5,10,15,20,0))
#' drangos2 <- data.frame(de=c(0,1000,2000,3000,4000,5000),a=c(1000,2000,3000,4000,5000,0))
#' drangos3 <- data.frame(de=c(0,80000,160000,240000,400000),a=c(80000,160000,240000,400000,0))
#' D8_Cuál_es_el_plazo_de_su_seguro_de_vida_TOTAL=rangeador(rtarget = brostatistics::frecuentator(fTtabla = datosb,fTvariables = nombresR(datosb,"D8_.*?r1_\\d$"),fTlevels = T,fbanner = bandera,fTusarNA = T),rrangos = drangos1)

rangeador <- function(rtarget,rrangos){
  # rtarget <- segurosVida$D8_Cuál_es_el_plazo_de_su_seguro_de_vida_TOTAL
  # El cero indica mayor que o menor que
  # rrangos <- data.frame(de=c(0,1,5,10,15,20),a=c(1,5,10,15,20,0))
  vectorTotal <- rtarget[nrow(rtarget),]
  rtarget <- rtarget[-nrow(rtarget),]
  # Creo los rangos como etiquetas
  etiquetas <- paste("De", rrangos$de,"a",rrangos$a, "")
  numeros <- as.numeric(rtarget$Respuesta)
  nframe <- NULL
  for(i in 1:nrow(rrangos)){
    # i <- 6
    bajo <- rrangos[i,"de"]
    alto <- rrangos[i,"a"]
    if(bajo==0){
      # Menor que
      minitarget <- rtarget[numeros<=alto,]
    }else if(alto==0){
      minitarget <- rtarget[numeros>bajo,]
    }else{
      minitarget <- rtarget[numeros>bajo & numeros<=alto,]
    }
    minitarget <- as.data.frame(t(apply(minitarget[,-1],2,sum,na.rm=T)))
    minitarget <- cbind(data.frame(Respuesta=etiquetas[i]), minitarget)
    nframe <- rbind(nframe,minitarget)
  }
  nframe <- rbind(nframe,vectorTotal)
  return(nframe)
}
