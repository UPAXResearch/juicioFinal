#'Función auxiliar para exportar los resultados generados con frecuentator. Recibe una lista y exporta todas las tablas en un csv
#'@param reporteF La lista que tiene los resultados de frecuentator
#'@param nombreFinal El nombre final del .csv a exportar
#'@param pMacro Si quieres el formato para Macro, poner TRUE aqui
#'@export
#'@keywords exportar
#'@examples 
#'exportator("resultados","./resultados/resultados.csv")

exportator <- function(reporteF, nombreFinal,pMacro=F,separador=",",encoding="Latin1"){
  # exportator(resultados, "resultados.csv")
  # reporteF <- bannervsbanner
  # nombreFinal <- "resultados.csv"
  reporteFINAL <- data.frame()
  if(pMacro){
    for(finali in 1:length(reporteF)){
      # finali <- 1
      elTemporal <- reporteF[[finali]]
      elTemporal <- rbind(names(elTemporal),elTemporal)
      #Hago un data frame chiquito con los titulos correctos
      titulos <- names(elTemporal)
      titulosF <- NULL
      for(t in 1:length(titulos)){
        # t <- 8
        minititulo <- unlist(strsplit(x = titulos[t],split = ":::"))
        while(length(minititulo)<3){
          minititulo <- c(minititulo,minititulo[1])
        }
        titulosF <- cbind(titulosF,minititulo)
      }
      titulosF <- as.data.frame(titulosF,stringsAsFactors = F)
      names(elTemporal) <- paste(LETTERS[1:length(elTemporal)],1:length(elTemporal),sep="")
      names(titulosF) <- paste(LETTERS[1:length(titulosF)],1:length(titulosF),sep="")
      # elTemporal[1,1] <- names(reporteF)[finali]
      titulosF[2,1] <- names(reporteF)[finali]
      titulosF[1,1] <- ""
      elTemporal <- rbind(titulosF,elTemporal[-1,],NA)
      # Voy a eliminar las columnas que dicen false en el row 2
      elTemporal <- elTemporal[,!elTemporal[2,] %in% "FALSE"]
      reporteFINAL <- plyr::rbind.fill(reporteFINAL,elTemporal)
    }
  }else{
    reporteFINAL <- data.frame()
    for(finali in 1:length(reporteF)){
      # finali <- 1
      elTemporal <- reporteF[[finali]]
      elTemporal <- rbind(names(elTemporal),elTemporal)
      names(elTemporal) <- paste(LETTERS[1:length(elTemporal)],1:length(elTemporal),sep="")
      elTemporal[1,1] <- names(reporteF)[finali]
      salto <- elTemporal[0,]
      elTemporal <- rbind(elTemporal,NA)
      reporteFINAL <- plyr::rbind.fill(reporteFINAL,elTemporal)
    }
  }
  write.csv(reporteFINAL, nombreFinal,fileEncoding = encoding,na = "",sep = separador)
}