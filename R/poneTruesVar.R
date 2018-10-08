#' Función interta que usa poneTruesBase
#'
#' @param x
#' @param extra
#'
#' @return
#' @export
#'
#' @examples
poneTruesVar<-function(x, extra){
  extra = c(c('',' ','0',NA), extra)
  x<-as.character(ifelse(x%in%c('',' ','0',NA),NA,TRUE))
  # x[x==''|x=='0']<-NA
  # x[x!=''|x!='0']<-TRUE
  x<-as.logical(x)
  return(x)
}
