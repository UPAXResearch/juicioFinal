#'Función matrices de conteos
#'
#'Crea la matriz de conteos a partir de variables de atribución de características
#'@param marcas Lista cuyas entradas son vectores de longitud 2, donde cada vector tiene como primer entrada el nombre de las marcas como viene escrito en las variables, y la segunda entrada se refiere al valor dentro de dichas variables.
#'@param variablesFiltro vector de tipo carácter con los nombres de las variables que se toman como filtro
#'@param datos dataframe con el que se trabajará
#'@export
#'@keywords conteos tabla tabladeconteos
#'@examples
#'data(datosFuncionConteo)
#'datos<-datos %>%
#'select(contains('P1Conoce'),contains('P1bConoce'),contains('P3Logos'),contains('P11'),contains('P12'))
#'marcas<-list(
#'  c('Banamex','Banamex'),
#'c('Banco.Azteca','Banco Azteca'),
#'c('Banco.Compartamos','Banco Compartamos'),
#'c('Banco.Famsa','Famsa'),
#'c('Banco.Walmart','Walmart'),
#'c('Bancoppel','Bancoppel'),
#'c('Banorte','Banorte'),
#'c('BBVA.Bancomer','BBVA Bancomer'),
#'c('HSBC','HSBC'),
#'c('Inbursa','Inbursa'),
#'c('IXE','IXE'),
#'c('Santander','Santander'),
#'c('Scotiabank','Scotiabank')
#')

#'resultado<-funcionTablaConteos(marcas=marcas,
#'                                variablesFiltro=c("P1ConoceBancos.","P1bConoceBancos.","P3Logos."),
#'                                datos=datos)



funcionTablaConteos<-function(marcas,variablesFiltro,datos){
  require(dplyr)
  #función de conteos atributoxmarca
  funcionConteoPorAtributo<-function(n){
    resultado<-sum(!is.na(n))
    return(resultado)
  }
  #función que calcula los conteos de todos los atributos para una marca
  funcionConteoPorMarca<-function(Marca,variablesFiltro,datos){
    b1<-paste0("filter(",paste0(paste0(variablesFiltro,Marca[1],collapse=paste0('=="',Marca[2],'"|')),paste0('=="',Marca[2],'"')),")")
    b2<-paste0('select(contains("',Marca[1],'"))')
    b3<-paste0("select(",paste0("-contains('",variablesFiltro,"')",collapse=','),")")
    eval(parse(text=paste(
      'datosMarca<-datos %>% \n',b1,'%>% \n',b2,'%>% \n',b3
      ,sep='')))
    resultados<-list()
    resultados$conteos<-apply(datosMarca,2,funcionConteoPorAtributo)
    resultados$base<-nrow(datosMarca)
    resultados$max<-max(resultados$conteos)
    resultados$proporcion<-resultados$max/resultados$base
    return(resultados)
  }

  a<-lapply(marcas,funcionConteoPorMarca,
            variablesFiltro=variablesFiltro,
            datos=datos)
  b<-cbind(a[[1]]$conteos,a[[2]]$conteos)
  for(i in 3:length(a)){
    b<-cbind(b,a[[i]]$conteos)
  }
  c<-c(a[[1]]$base,a[[2]]$base)
  for(i in 3:length(a)){
    c<-c(c,a[[i]]$base)
  }
  resultado<-list()
  resultado$matriz<-b
  resultado$bases<-c
  return(resultado)
}
