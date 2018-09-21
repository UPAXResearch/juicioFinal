#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param x Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'pendiente

poneTruesVar<-function(x){
  x<-as.character(ifelse(x%in%c('',' ','0',NA),NA,TRUE))
  # x[x==''|x=='0']<-NA
  # x[x!=''|x!='0']<-TRUE
  x<-as.logical(x)
  return(x)
}
