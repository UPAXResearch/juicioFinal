#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param base Pendiente
#'@param variables Pendiente
#'@param identificador Pendiente
#'@param palabrasNegras Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'iris$ID<-1:nrow(iris)
#'iris$Species2<-iris$Species
#'UnionMenciones(base=iris,variables = c('Species'),identificador = 'ID', palabrasNegras = c(''))
UnionMenciones <-
  function(base,
           variables,
           identificador,
           palabrasNegras) {
    listita <- NULL
    for (i in 1:length(variables)) {
      listita[[i]] <-
        menciones(base, variables[i], palabrasNegras, identificador)
      print(variables[i])
    }
    final <-
      Reduce(function(df1, df2)
        full_join(df1, df2, by = identificador), listita)
    return(final)
  }
