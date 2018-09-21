#'Función para hacer normalizaciones
#'
#'Descripción pendiente
#'@param conteos Pendiente
#'@param totales Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'pendiente

normalizPablo <- function (conteos, totales){
  total <- sum(conteos)
  totalMarca <- conteos %>% dplyr::summarise_each(funs(sum))
  conteost <- data.frame(t(conteos))
  totalAtributo <- conteost %>% summarise_each(funs(sum))
  probMarca <- totalMarca / total
  probAtributo <- totalAtributo / total
  conteosEsperados <- conteos
  for (i in 1:nrow(conteosEsperados)) {
    for (j in 1:length(conteosEsperados)) {
      conteosEsperados[i, j] <- probAtributo[i] * probMarca[j] *
        total
    }
  }
  conteosDiferencia <- conteos - conteosEsperados
  # for (i in 1:length(conteosDiferencia)) {
  #   conteosDiferencia[, i] <- conteosDiferencia[, i]/totales[i]
  # }
  # conteosDiferencia <- round(conteosDiferencia * 100, 2)
  #

  estandarizados <- conteosDiferencia[]
  for (i in 1:length(conteosDiferencia)) {
    estandarizados[, i] <- scale(conteosDiferencia[, i])
  }

  #   for(i in 1:nrow(conteosDiferencia)){
  #     estandarizados[i,]<-scale(conteosDiferencia[i,],)
  #   }
  #
  resultados <- cbind(conteosDiferencia, estandarizados)
  return(resultados)
}
