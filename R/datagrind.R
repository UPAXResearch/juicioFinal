#'Titulo pendiente
#'
#'Descripción pendiente
#'@param lista Descripción pendiente
#'@param esAnidado Descripción pendiente
#'@param etiquetas Descripción pendiente
#'@param tablaUnica Descripción pendiente
#'@param variablesIguales Descripción pendiente
#'@param eVariables Descripción pendiente
#'@param colapsarPor Descripción pendiente
#'@export
#'@keywords Pending
#'@examples
#'#pendiente

datagrind<-function(lista, esAnidado=F, etiquetas=NULL, tablaUnica=T, variablesIguales=T, eVariables = NULL, colapsarPor = NULL){
  #lista con dataframes hechos por frecuentator con la mismas dimensiones

  if(!is.null(colapsarPor)){
    tablaUnica=T
  }

  elementos<-names(lista)

  #Tengo que verificar que tengo las mismas filas y columnas
  columna<-colnames(lista[[elementos[1]]])
  filas<-rownames(lista[[elementos[1]]])

  ccolumna<-NULL
  cfilas<-NULL
  cuantosRow<-NULL

  ####verificar dimensiones
  if(!length(unique(lengths(lista)))==1){
    stop(paste("\n datagrind Error01-A: el número de columnas de algún o varios elementos de la lista no son iguales a las demás, no es posible continuar"))
  }


  for (i in 1:length(lista)) {
    cuantosRow<-c(cuantosRow, nrow(lista[[i]]))
  }

  if(!length(unique(cuantosRow))==1){
    stop(paste("\n datagrind Error01-B: el número de filas de algún o varios elementos de la lista no son iguales a las demás, no es posible continuar"))
  }

  ####verificar nombres
  for(i in  1:length(lista)){
    ccolumna<-c(ccolumna, colnames(lista[[elementos[i]]]))
    cfilas<-c(cfilas, rownames(lista[[elementos[i]]]))
  }

  ccolumna<-unique(ccolumna)
  cfilas<-unique(cfilas)

  veri<-ccolumna[-which(ccolumna%in%columna)]

  if(!length(veri)==0){
    stop(paste("\n datagrind Error02-A: la(s) columna(s) de algún o varios elementos de la lista no son iguales a las demás, no es posible continuar:: Revisar, ", veri))
  }

  if(variablesIguales){
    veri<-cfilas[-which(cfilas%in%filas)]
    if(!length(veri)==0){
      stop(paste("\n datagrind Error02-B: la(s) filas(s) de algún o varios elementos de la lista no son iguales a las demás, no es posible continuar:: Revisar, ", veri))
    }
  }

  #verifico tener un nombre para cada elemento de la lista
  if(!is.null(etiquetas)){
    if(!length(etiquetas)==length(lista)){
      stop(paste("\n datagrind Error03: 'etiquetas' tiene ::",length(etiquetas),":: elementos; no son la misma cantidad de entradas en 'lista'"))
    }
  }

  #sub(":::.{1,4}$", "\\1", colnames(lista[[elementos[1]]]), perl = T)

  if(esAnidado){
    #Si es anidado quita el primer segmento con ::: ya que es la etiqueta del valor de anidado
    nombres <-sub("^(.*?):::.*", "\\1", colnames(lista[[elementos[1]]]))
  }else{
    #Si no es anidado debe quitar el último segmento de :::
    nombres <-sub(":::.{1,4}$", "\\1", colnames(lista[[elementos[1]]]), perl = T)
  }

  nombres <-unique(nombres)

  FINAL<-list()

  for (i in c(nombres[-1])) {
    nueva<- lista[[elementos[1]]][1]

    for (j in 1:length(elementos)) {

      nueva<- cbind(nueva,
                    lista[[elementos[j]]][grep(paste0("^(",i,")"), x = colnames(lista[[elementos[j]]]))])

      #Que columnas acabo de pegar
      sobre<-grep(paste0("^",i), x = colnames(nueva))
      if(!is.null(etiquetas)){
        names(nueva)[sobre]<-paste0(etiquetas[j],":::",colnames(nueva)[sobre])
      }else{
        names(nueva)[sobre]<-paste0(elementos[j],":::",colnames(nueva)[sobre])
      }
    }
    if(length(eVariables)==(nrow(nueva)-1)){
      nueva[-nrow(nueva),1]<-eVariables
    }

    if(!is.null(colapsarPor)){
      if(any(nueva$Respuesta%in%colapsarPor)){
        nueva<-nueva[nueva$Respuesta==colapsarPor,]
        nueva$Respuesta<-i
        if(esAnidado){
          colnames(nueva)<-sub(pattern = paste0(i,":::.*?:::"), replacement = "", x = colnames(nueva))
        }else{
          colnames(nueva)<-sub(pattern = paste0(":::",i), replacement = "", x = colnames(nueva))
        }
      }else{
        warning(paste("\n datagrind: 'colapsarPor'::",colapsarPor,":: no existe en columna de respuestas, se omitirá en el acomodo"))
        colapsarPor=NULL
      }
    }
    FINAL[[i]]<-nueva
  }

  #Lo devuelvo como una lista o como una única tabla?

  if(tablaUnica){
    nlistas<-length(FINAL)
    prueba<-FINAL[[1]]
    if(nlistas>1){
      prueba$Respuesta<-as.character(prueba$Respuesta)
      #i=2
      for (i in 2:nlistas) {
        colnames(FINAL[[i]])<-colnames(FINAL[[1]])
        if(!is.null(colapsarPor)){
        }else{
          prueba[nrow(prueba)+1,] <- NA
          prueba<-rbind(prueba, colnames(FINAL[[i]]))
        }
        prueba<-rbind(prueba, FINAL[[i]])

      }
    }
    #regreso<-list(neto=neto,subneto=subneto)
    regreso<-prueba
    FINAL<-prueba
  }
  ####
  return(FINAL)
}
