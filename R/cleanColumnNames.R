#'Función para limpiar el nombre de las columnas
#'
#'Esta función limpia a partir de ciertas reglas el nombre de las funciones, unicamente regresa un vector con el nombre de las columnas ya limpias
#'@param datos La tabla principal i.e. "la base de datos"
#'@export
#'@keywords limpiar
#'@examples
#'#pending

cleanColumnNames<-function(datos){

  columnas <- colnames(datos)

  m <- regexpr("^P\\d+(?:[\\._]\\d)*", columnas, perl=T)

  p <- regmatches(columnas, m)

  preg <- as.data.frame(table(p))

  unicas <- filter(preg, Freq == 1 )
  multip <- filter(preg, Freq != 1)

  preguntas <- columnas
  preguntas <- gsub("\t", " ", preguntas, fixed = T )

  intersectStrings <- function(x,y) {

    a<-strsplit(x,NULL)


    b<-strsplit(y,NULL)

    n<-match(FALSE,do.call("==", list(a[[1]],b[[1]])))-1
    inter<-ifelse(n==0, character(0), substr(x,1,n))
    return(inter)
  }
  voting <- function(x, n=200) {
    m<-length(x)

    l<-vector("list", n)
    for (i in 1:n) {
      a<-sample(1:m,2)
      u<-x[a[1]]
      v<-x[a[2]]
      l[[i]]<-intersectStrings(u,v)
    }
    t<-sort(table(unlist(l)),decreasing=TRUE)
    name<-names(t)
    return(name)
  }
  mx<-100
  error<-0
  for (i in unicas$p) {
    i<-gsub(".", "\\.", i, fixed = T)

    ts<-paste0("^",i,"(?:\\.)?[^0-9\\._]")#.*")
    tr<-paste0("^",i,"(?:\\.)?[^0-9\\._].*")

    mt <- regexpr(ts, preguntas, perl=T)
    pt <- regmatches(preguntas, mt)

    preguntas <- gsub(tr, i, preguntas, perl=T)

    if(length(preguntas[mt!=-1]) > 1) {
      print("****ERROR, NOT PROPERLY PARSED****")
      error<-error+1
    }
    cat("\n")
  }
  print(paste0("# of errors: ", error))

  error<-0
  preguntas2<-preguntas

  for (i in multip$p) {

    i<-gsub(".", "\\.", i, fixed = T)

    ts<-paste0("^",i,"(?:\\.)?[^0-9\\._]")#.*")
    tr<-paste0("^",i,"(?:\\.)?[^0-9\\._].*")

    c <- regexpr(ts, preguntas2, perl=T)
    d <- regmatches(preguntas2, c)

    mt <- regexpr(tr, preguntas2, perl=T)
    pt <- regmatches(preguntas2, mt)
    cols<-columnas[mt!=-1]

    p<-voting(pt)
    q<-p[1]
    r<-p[-1]
    r<-r[r!=gsub("\\.", ".", i, fixed = T)&r!=paste0(gsub("\\.", ".", i, fixed = T),".")]

    # if (i=="P29" | i=="P32" | i=="P39" | i=="P42") {
    #   print(q)
    #   print(r)
    #   cat("\n")
    # }


    i<-gsub("\\.", ".", i, fixed = T)
    if (q!=i){
      colsp<-gsub(q,paste0(i,"_"),cols,fixed=T)
    } else {
      colsp<-gsub(q,paste0(i,""),cols,fixed=T)
    }
    # colsp
    for (pat in r){
      colsp<-gsub(pat,paste0(i,"_"),colsp,fixed=T)
    }
    # colsp
    colsp<-gsub("(?i)(\\d)([¿a-z])","\\1\\.\\2",colsp)

    preguntas2[mt!=-1]<-colsp

  }

  preguntas3<-gsub("(?i)[^a-zA-Z0-9\\.\\sáéíóúüñ_]", "", preguntas2, perl=T)
  preguntas3<-gsub("\\s+", "\\.", preguntas3, perl=T)
  preguntas3<-gsub("\\.+", "\\.", preguntas3, perl=T)
  preguntas3<-gsub("\\._", "_", preguntas3, perl=T)
  preguntas3<-gsub("_+", "_", preguntas3, perl=T)

  l<-40
  k<-length(columnas)
  for (i in k:10) {
    col<-columnas[i]
    colbis<-preguntas3[i]
    num<-nchar(col)
    numbis<-nchar(colbis)
    if(num>=mx) {
      print(paste0(substr(col,1,l-3),
                   "...",
                   substr(col,num-(mx-l),num)))
    } else {
      print(col)
    }
    print("turns into:")
    if(numbis>=mx) {
      print(paste0(substr(colbis,1,l-3),
                   "...",
                   substr(colbis,numbis-(mx-l),numbis)))
    } else {
      print(colbis)
    }
    cat("\n")
  }

  return(preguntas3)
}
