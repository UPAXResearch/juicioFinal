#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param x Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'pendiente

poneTruesBase<-function(base,variables){
  for(i in 1:length(variables)){
    base[,variables[i]]<-poneTruesVar(base[,variables[i]])
    print(str(base[,variables[i]]))
  }
  return(base)
}
