#' Convertir columna en logica
#'
#' @param base La base a considerar
#' @param variables Las variables a convertir, meterla como vector de strings
#' @param extra Algunos caracteres/texto extra para poner FALSE en estos casos
#'
#' @return Data.frame
#' @export
#'
#' @examples

poneTruesBase<-function(base,variables, extra = c()){

  for(i in 1:length(variables)){
    base[,variables[i]]<-poneTruesVar(base[,variables[i]], extra)
    print(str(base[,variables[i]]))
  }
  return(base)
}
