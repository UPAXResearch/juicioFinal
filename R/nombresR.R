#'Regresa los nombres de variables de una tabla que coincidan con un string
#'
#'Esta función es un atajo , es una envoltura de names(x)[grep(y,names(x))] donde x es una tabla, y es un string
#'@param xx La tabla principal i.e. "la base de datos"
#'@param yy El string que se va a buscar
#'@export
#'@keywords nombres
#'@examples 
#'nombresR(iris,"Sep")
#'nombresR(iris,"Sepal.Len")

nombresR<- function(xx,yy){
  return(
    names(xx)[grep(yy,names(xx))]
  )
}