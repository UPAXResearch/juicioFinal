#'Función para crear combinaciones maxdiff
#'
#'Crea la batería de combinaciones para realizar un maxdiff a partir de un conjunto de atributos
#'@param Atributos vector de tipo caracter que contiene a los atributos
#'@param nBloques número de bloques en el maxdiff
#'@param AlternativasPorBloque número de atributos que entrarán dentro de cada bloque
#'@param Iteraciones número de veces que el modelo buscará la combinación óptima, se recomienda utilizar una iteración para definir el número de bloques y número de atributos por bloque. Una vez definidos estos el número de iteraciones mínimas recomendadas es de 50
#'@export
#'@keywords maxdiff combinaciones
#'@examples combinaciones<-maxdiffCombinaciones(Atributos=letters[1:10], nBloques=10, AlternativasPorBloque=3, Iteraciones = 2)
#'combinaciones


maxdiffCombinaciones <- function(Atributos,nAtributos=length(Atributos), nBloques, AlternativasPorBloque, Iteraciones = 5){
  require(AlgDesign)
  # checo que los parametros sean apropiados
  # mínimo se recomienda nBloques >= 3 * nAtributos / AlternativasPorBloque
  if (nBloques < 3 * nAtributos / AlternativasPorBloque)
    warning("Se recomienda que nBloques >= 3 * nAtributos / AlternativasPorBloque");
  require(AlgDesign)
  mejorResultado = NULL
  mejorDiseno = -Inf
  for (i in 1:Iteraciones){
    algResultados <- optBlock(~.,withinData=factor(1:nAtributos),blocksizes=rep(AlternativasPorBloque,nBloques), nRepeats=5000) #BIB
    if (algResultados$D > mejorDiseno){
      mejorResultado = algResultados
      mejorDiseno = algResultados$D
    }
  }
  diseno <- matrix(NA,nBloques,AlternativasPorBloque, dimnames= list(bloque = 1:nBloques, alternativa = 1:AlternativasPorBloque))
  matrizBinaria <- matrix(0,nBloques,nAtributos, dimnames= list(bloque = 1:nBloques, alternative = 1:nAtributos))
  contador <- 0
  for (bloque in mejorResultado$Blocks){
    contador <- contador + 1
    blck <- unlist(bloque)
    diseno[contador,] <- blck
    for (a in blck)
      matrizBinaria[contador,a] <- 1
  }
  nAparicionesPorAlternativa <- table(as.numeric(diseno))
  combinacionesDeAlternativas <- crossprod(table(c(rep(1:nBloques, rep(AlternativasPorBloque,nBloques))), mejorResultado$design[,1]))
  resultado<-list(matrizBinaria = t(matrizBinaria), diseno = t(diseno), frequencies = nAparicionesPorAlternativa, frecuenciasPorPares=combinacionesDeAlternativas, correlacionesBinarias = round(cor(matrizBinaria),2))


  funcionEntrega<-function(modelo,atributos){
    vectorB<-NULL
    for(i in 1:nrow(modelo)){
      vectorB<-c(vectorB,as.character(atributos[c(modelo[i,])]))
    }
    matrizB<-matrix(vectorB,ncol=ncol(modelo),byrow=T)
    return(matrizB)
  }

  resultado$entregable<-funcionEntrega(resultado$diseno,Atributos)
  return(resultado)

}

