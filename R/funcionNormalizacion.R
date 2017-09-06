#'Función de normalización
#'
#'Esta función permite crear una matriz de normalización a partir de una matriz de conteos y de un vector de tamaños de base
#'@param conteos Matriz de conteos
#'@param totales Vector de tamaños de las bases totales para cada marca
#'@export
#'@keywords normalizacion
#'@examples load(conteosYtotales)
#'funcionNormalizacion(resultados[[1]],resultados[[2]])


funcionNormalizacion<-function(conteos,totales){
  total<-sum(conteos)
  totalMarca<-conteos %>%
    summarise_each(funs(sum))
  conteost<-data.frame(t(conteos))
  totalAtributo<-conteost %>%
    summarise_each(funs(sum))
  probMarca<-totalMarca/total
  probAtributo<-totalAtributo/total
  conteosEsperados<-conteos
  for(i in 1:nrow(conteosEsperados)){
    for(j in 1:length(conteosEsperados)){
      conteosEsperados[i,j]<-probAtributo[i]*probMarca[j]*total
    }
  }
  conteosDiferencia<-conteos-conteosEsperados
  for(i in 1:length(conteosDiferencia)){
    conteosDiferencia[,i]<-conteosDiferencia[,i]/totales[i]
  }
  conteosDiferencia<-round(conteosDiferencia*100,2)
  return(conteosDiferencia)
}
