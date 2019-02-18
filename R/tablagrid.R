


#' Creación de tablas ponderadas o no
#'
#' @param base el data frame que contiene la base.
#' @param filas La variable o variables que se mostraran en las filas, para anidar variables se utilza ":"\code{filas = "Género:Edad"} , puede ser NA cuando se incluye el esquema.
#' @param columnas La variable o variables que se mostraran en las columnas, para anidar variables se utilza ":"\code{columnas = "Género:Edad"} , puede ser NA cuando se incluye el esquema.
#' @param tipo El tipo de tabla resultante, puede generar tablas de frecuencias \code{tipo = "F"}, porcentajes \code{tipo = "P"},
#' diferencias \code{tipo = "D"}, o tablas combinadas frecuencias y porcentajes \code{tipo =  "FP"}, porcentajes y diferencias \code{tipo = "PD"}.
#' @param ponderador Nombre de la variable ponderador, en caso de que exista.
#' @param ordenar Se puede ordenar la tabla de forma descentente \code{ordenar = "D"} o ascendente \code{ordenar = "A"},
#' para ordenar requiere que exista una variable de total en las columnas.
#' @param esquema Las especificaciones para construir tablas grid.
#' @param prop en este parametro se especifica si la prueba de proporciones se realizara sobre los niveles internos de cada variable
#' \code{diferencias = NIVELES} o sobre todas las variables ingresadas (ignorando el total en caso de que exista) \code{diferencias = COLUMNAS}.
#' @param filtro.filas niveles para remover de las filas ingresadas, de forma predeterminada \code{filtro.filas = c(""," ","-")}.
#' @param filtro.columnas niveles para remover de las columnas ingresadas.
#' @param base.natural usar \code{base.natural = TRUE} en caso de que se desee tener la base natural en la tabla resultante.
#' @param usarNA si se van a mostrar los valores \code{NA} en la tabla.
#' @param remover.duplicados si se van a remover los valores duplicados en las filas.
#' @param total.int determina el metodo que se usara para calcular el total.
#' #'
#' @return devuelve una tabla de frecuencias de acuerdo a lo especificado en el parametro "tipo".
#' @export
#'
#' @examples
#' tablagrid(iris,filas = "Species",columnas ="Species",ordenar = "D")
#' #Para usar el esquema suponiendo que se tiene lo siguiente:
#' # filas<-c("P9","P14","P19","P24","P30")
#' # columnas<-c("Total","P8","P13","P18","P23","P29")
#' # esquema<-c(paste0("Total|",paste0(filas, collapse=",")),"P8|P9","P13|P14","P18|P19","P23|P24","P29|P30")
#'
tablagrid<-function(base,filas = NA,columnas = NA,tipo = "FP",ponderador = NA, ordenar = FALSE ,esquema=NA ,prop = "NIVELES",filtro.filas = c(""," ","-"),filtro.columnas=NA,base.natural=FALSE,usarNA=FALSE,remover.duplicados = TRUE,total.int=TRUE,decimales=4){

    ## Revisamos que la base tenga casos
  if (nrow(base) == 0) {
    FINAL <- data.frame()
    FINAL <- data.frame(row.names = c("Vacio"))
    FINAL$Respuesta <- as.factor("Base sin Casos")
    return(FINAL)
  }

  ### Validamos los argumentos
  if (all(is.na(filas))&all(is.na(columnas))&all(is.na(esquema))){
    stop(paste("No se encuentran las filas/columnas o el esquema"))
  } else if(all(!is.na(filas))&all(is.na(columnas))&all(is.na(esquema))){
    #Si solo hay filas pero no hay columnas o esquema
    base$TOTAL<-"TOTAL"
    base$TOTAL<-as.factor(base$TOTAL)
    columnas<-"TOTAL"
  }else if(all(is.na(filas))&all(!is.na(columnas))&all(is.na(esquema))){
    base$TOTAL<-"TOTAL"
    base$TOTAL<-as.factor(base$TOTAL)
    filas<-"TOTAL"
  }

  ### Revisamos si el ponderador existe y si no lo creamos
  if(is.na(ponderador)){
    base$pond<-1
    ponderado<-FALSE
  }else{
    ponderado<-TRUE
    base$pond<-base[,ponderador]
  }


  ########### Se preparan las variables anidadas

  ###Función para trabajar con variables anidadas
  #El primer argumento devuelve la base con las modificaciones
  #El segundo argumento los nombres de las columnas o las filas actualizado
  #El tercer indica si existen variables anidadas

  anidado<-function(base,variables,separador=":::",esquema = F){
    variables.anidadas<-0
    variables2<-NA
    for(variable in variables){
      variable<-unlist(strsplit(variable,":"))
      # Validamos que la variable exista en la base
      if (!all(variable %in% names(base))&esquema == F) {
        stop(paste("No existe la variable ::",
                   paste(variable[!variable %in% names(base)],
                         collapse = ", ")))
      }
      if(length(variable)>1){


        ##Pendiente construir un metodo que preserve los niveles en variables anidadas
        # ##Aqui unificamos los niveles
        # cfactor<-0
        # #Revisamos que todas las variables sean factor
        # for(var.aux in  variable){
        #   if(is.factor(base[,var.aux]))
        #     cfactor<-cfactor+1
        # }
        #
        # #Si todas las variables anidadas son factor creamos sus niveles
        # niveles.aux<-NA
        #
        # if(cfactor == length(variable)){
        #   for(i in 1:(length(variable)-1)){
        #     niveles.aux<-base[,variable[i]]
        #     for(j in 1:length(niveles.aux)){
        #
        #
        #     }
        #
        #
        #     }
        #
        #   }
        # }


        #seguimos anidando

        variables.anidadas<-max(variables.anidadas,length(variable))
        base[,paste0(variable,collapse = "_")]<- apply( base[ ,variable] , 1 , paste , collapse = separador )
        variables2<-c(variables2,paste0(variable,collapse = "_"))


      }else{
        variables2<-c(variables2,variable)
      }
    }
    variables<-variables2[-1]
    resultado <- list(base, variables,variables.anidadas)
    return(resultado)
  }


  if(all(is.na(esquema))){
    ###Filas
    lista.filas<-anidado(base,filas)
    base<-as.data.frame(lista.filas[1])
    filas<-unlist(lista.filas[2])
    anidado.filas<-unlist(lista.filas[3])

    ###Columnas
    lista.columnas<-anidado(base,columnas)
    base<-as.data.frame(lista.columnas[1])
    columnas<-unlist(lista.columnas[2])
    anidado.columnas<-unlist(lista.columnas[3])

    ###Esquema -- Reescribimos el esquema para trabajar con filas y columnas anidadas
  }else if(all(!is.na(esquema))){
    filas.esquema<-NA
    columnas.esquema<-NA
    for(i in 1:length(esquema)){
      #Pendiente añadir un error si el vector resultante del unlist tiene más de dos elementos
      esquema1<-unlist(strsplit(esquema[i],"\\|"))
      columnas.aux<-unlist(strsplit(esquema1[1],","))
      filas.aux<-unlist(strsplit(esquema1[2],","))

      ###Guardamos todos los valores de filas y columnas que hay en el esquema
      if(all(is.na(filas.esquema))){
        filas.esquema<-filas.aux
        columnas.esquema<-columnas.aux
      }else{
        filas.esquema<-c(filas.esquema,filas.aux)
        columnas.esquema<-c(columnas.esquema,columnas.aux)
      }
    }

    ###Filas
    filas<-unique(filas.esquema)
    lista.filas<-anidado(base,filas,T)
    base<-as.data.frame(lista.filas[1])
    filas<-unlist(lista.filas[2])
    anidado.filas<-unlist(lista.filas[3])

    ###Columnas
    columnas<-unique(columnas.esquema)
    lista.columnas<-anidado(base,columnas,T)
    base<-as.data.frame(lista.columnas[1])
    columnas<-unlist(lista.columnas[2])
    anidado.columnas<-unlist(lista.columnas[3])

    esquema<- gsub(pattern = ":",replacement = "_",x = esquema)
  }


  #### Creamos la base solo con las variables de interes
  ### Limpiamos los duplicados de las filas
  if(remover.duplicados==T & length(filas)>1){
    for(i in 1:nrow(base)){
      unicos.aux<-unique(t(base[i,filas]))
      filas.aux<-filas[!filas%in%rownames(unicos.aux)]
      base[i,filas.aux]<-NA
    }
  }

  variables<-unique(c(filas,columnas))
  base<-base[,c("pond",variables)]
  base$ID<-rep(1:nrow(base))


  ### Calculo del total inteligente
  if(total.int == TRUE){
    base.aux<-reshape2::melt(base[,c("ID",filas)],id = c("ID"))
    #Metodo para identificar valores vacios en una sola variable
    base.aux$smart<-ifelse(base.aux$value%in%c(""," ","-")|is.na(base.aux$value),FALSE,TRUE)
    #Metodo para identificar valores vacios en variables anidadas
    filas.vacias.smart<-grep(":::NA:::|::::::|^NA:::|^:::|:::$|:::NA$",base.aux$value)
    if(length(filas.vacias.smart)>0){
      base.aux<-base.aux[-filas.vacias.smart,]
    }
    base.aux<-base.aux[base.aux$smart==TRUE,]
    unique.ids<-unique(base.aux$ID)
    base.aux<-NULL
    base<-base[base$ID%in%unique.ids,]
  }


  ### Se filtran los valores indicados

  if(all(!is.na(filtro.filas))){
    for(fila in filas){
      if(is.factor(base[,fila])){
        levels(base[,fila])[which(levels(base[,fila])%in%filtro.filas)]<-NA
      }else{
        base[,fila]<-ifelse(base[,fila]%in%filtro.filas,NA,base[,fila])
      }
    }
  }

  if(all(!is.na(filtro.columnas))){
    for(columna in columnas){
      if(is.factor(base[,columna])){
        levels(base[,columna])[which(levels(base[,columna])%in%filtro.columnas)]<-NA
      }else{
        base[,columna]<-ifelse(base[,columna]%in%filtro.columnas,NA,base[,columna])
      }
    }
  }


  #### Total

  Frecuencias<-function(base,filas,columnas,value.var = "pond"){
    frecuencias<-NA
    frecuencias.columnas<-NA

    for(fila in filas){
      for(columna in columnas){
        #dplyr version
        # frecuencias.aux<- base %>% count(PLAZA_Genero,Tipo_Paquete_Genero) %>% spread(Tipo_Paquete_Genero, n, fill=0)
        # frecuencias.aux <- base %>% group_by(PLAZA_Genero,Tipo_Paquete_Genero) %>% summarise(Conteo = n())
        ## Reshape2
        ### Calculo de las frecuencias por fila y por columna
        frecuencias.aux<-reshape2::dcast(base, list(fila,columna), value.var = value.var, fun.aggregate = sum ,drop = T)
        #Se eliminan las columnas que solo contienen NA
        columnaNA<-which(colnames(frecuencias.aux)%in%"NA")
        if(length(columnaNA)>=1){
          frecuencias.aux<-frecuencias.aux[,-columnaNA]
        }
        #Se renombran las columnas
        if(ncol(frecuencias.aux)>1){
          colnames(frecuencias.aux)<-c("Filas",paste0(columna,":::",colnames(frecuencias.aux)[-1]))}

        #Se anexan las frecuencias de la primer columna al total
        if(is.null(nrow(frecuencias.columnas))){
          frecuencias.columnas<-frecuencias.aux
        }else{
          frecuencias.columnas<-cbind(frecuencias.columnas,frecuencias.aux %>% dplyr::select(-"Filas"))
          # frecuencias<-cbind(frecuencias,frecuencias.aux %>% dplyr::select(-!!sym(fila)))
        }
      }

      # Se une a las filas
      if(is.null(nrow(frecuencias))){
        frecuencias<-frecuencias.columnas
      }else{
        frecuencias<-rbind(frecuencias,frecuencias.columnas)
      }
      frecuencias.columnas<-NA
    }

    return(frecuencias)
  }


  ######### Frecuencias

  ## Revisamos si se ingreso un esquema
  if(all(!is.na(esquema))){
    #### Cuando se ingresa más de una fila, se nivelan sus niveles
    frecuencias<-NA
    for(i in 1:length(esquema)){
      #Pendiente añadir un error si el vector resultante del unlist tiene más de dos elementos
      esquema1<-unlist(strsplit(esquema[i],"\\|"))
      columnas.aux<-unlist(strsplit(esquema1[1],","))
      filas.aux<-unlist(strsplit(esquema1[2],","))
      frecuencias.aux<-Frecuencias(base,filas.aux,columnas.aux)
      frecuencias.aux<-reshape2::melt(frecuencias.aux,"Filas")
      #validamos si el data frame existe
      if(is.null(nrow(frecuencias))){
        frecuencias<-frecuencias.aux
      }else{
        frecuencias<-rbind(frecuencias,frecuencias.aux)
      }
    }
    frecuencias<- reshape2::dcast(frecuencias, Filas ~ variable, value.var = "value", fun.aggregate = sum)
  }else{
    #Si el esquema no existe las frecuencias se calculan usando las filas y columnas directamente
    frecuencias<-Frecuencias(base,filas,columnas)
    ## En caso de que haya niveles duplicados se unifican los niveles de las filas
    if(length(unique(frecuencias$Filas))!=length(frecuencias$Filas)){
      frecuencias.m<-reshape2::melt(frecuencias,id = c("Filas"))
      frecuencias<-reshape2::dcast(frecuencias.m, Filas ~ variable, value.var = "value", fun.aggregate = sum)
    }
  }


  #########Codigo para ordenar la tabla


  if(ordenar != FALSE){
    ordenar<-gsub(" ","",toupper(ordenar))

    #Buscamos la variable por la cual se va a ordenar, de no existir se toma la primera pendiente de implementar
    if(length(grep("^TOTAL:::", toupper(colnames(frecuencias))))>0){
      if(ordenar == "D"){
        frecuencias <- frecuencias[order(-frecuencias[,which(toupper(colnames(frecuencias))%in%c(toupper(colnames(frecuencias))[grep("^TOTAL:::", toupper(colnames(frecuencias)))] ))]),]
      }else if(ordenar == "A"){
        frecuencias <- frecuencias[order(frecuencias[,which(toupper(colnames(frecuencias))%in%c(toupper(colnames(frecuencias))[grep("^TOTAL:::", toupper(colnames(frecuencias)))] ))]),]
      }else{
        stop(paste("No se puede ordenar: el parametro ",ordenar, " no es válido" ))
      }
    }else{
      stop(paste("Para ordenar se requiere una variable de total en el banner" ))
    }
  }

  #### Remover NA
  if(usarNA == F){
    #Limpiamos las filas
    frecuencias<-frecuencias[!is.na(frecuencias$Filas),]
    filas.vacias<-grep(":::NA:::|::::::|^NA:::|^:::|:::$|:::NA$",frecuencias$Filas)
    if(length(filas.vacias)>0){
      frecuencias<-frecuencias[-filas.vacias,]
    }

    ##limpiamos las columnas
    columnas.vacias<-grep(":::NA:::|::::::|^NA:::|^:::|:::$|:::NA$",colnames(frecuencias))
    if(length(columnas.vacias)>0){
      frecuencias<-frecuencias[,-columnas.vacias]
    }
    if(any(is.na(colnames(frecuencias)))){
      frecuencias<-frecuencias[,-which(colnames(frecuencias)%in%"NA")]
    }
  }

#####Hacemos el calculo del total

    ### Añadimos el total ponderado
    base$variable_de_conteo<-rep(1)
    if(is.na(ponderador)){
      total.ponderado<-Frecuencias(base,"variable_de_conteo",columnas)
      total.ponderado[1]<-"Total"
      frecuencias<-rbind(frecuencias,total.ponderado)
    }else{
      total.ponderado<-Frecuencias(base,"variable_de_conteo",columnas)
      total.ponderado[1]<-"Total"
      frecuencias<-rbind(frecuencias,total.ponderado)
    }

    ## Calculamos el total natural

    if(base.natural == TRUE){
      total.natural<-Frecuencias(base,"variable_de_conteo",columnas,"variable_de_conteo")
      total.natural[1]<-"Total Natural"
      numero.totales<-2
    }else{
      numero.totales<-1
    }





  ###########El resultado lo determina el argumento fTtipo.

  tipo<-gsub(" ","",toupper(tipo))
  #Completamos las tablas basicas

  if(tipo %in% c("P","FP","PF","DP","PD")){
    porcentajes<-as.data.frame(sweep(as.matrix(frecuencias[-1]), 2, c(unlist(total.ponderado[-1])), `/`))
    porcentajes<-porcentajes*100
    # porcentajes<-rbind(porcentajes,total.ponderado)

    ##Aqui hacemos que el total de los porcentajes sea la suma
    porcentajes[nrow(porcentajes),]<-colSums(porcentajes[-nrow(porcentajes),,drop = FALSE])

    #Añadimos el nombre de las filas
    porcentajes<-cbind(frecuencias[1],porcentajes)


    if(base.natural == TRUE){
      porcentajes<-rbind(porcentajes,total.natural)
    }

    #Se redondean los porcentajes
    porcentajes[,2:ncol(porcentajes)]<-round(porcentajes[,2:ncol(porcentajes)],decimales)

    #Se añade el tipo al nombre de las columnas para la macro de formato
    colnames(porcentajes)<-c("Filas",paste0(colnames(porcentajes)[-1],":::pct"))
  }

  if(tipo %in% c("F","FP","PF","DF","FD")){

    #Se añade la base natural
    if(base.natural == TRUE){
      frecuencias<-rbind(frecuencias,total.natural)
    }

    #Se redondean los porcentajes
    frecuencias[,2:ncol(frecuencias)]<-round(frecuencias[,2:ncol(frecuencias)],decimales)



    #Se añade el tipo al nombre de las columnas para la macro de formato
    colnames(frecuencias)<-c("Filas",paste0(colnames(frecuencias)[-1],":::f"))
  }

  if(tipo %in% c("D","DP","PD","DF","FD") & prop%in%c("NIVELES","COLUMNAS")){

    ###Función para calcular las Diferencias

    Diferencias<-function(frecuencias,seleccion){
      letras = c(LETTERS, letters)

      diferencias.aux<-as.data.frame(matrix("",nrow = nrow(frecuencias)-1, ncol = length(seleccion)),stringsAsFactors = FALSE)

      if(length(seleccion)==1){
        colnames(diferencias.aux)<-seleccion
      }

      if(length(seleccion)>1){
        totales<-frecuencias[nrow(frecuencias),seleccion]
        minibase<-frecuencias[,seleccion]
        colnames(diferencias.aux)<-names(minibase)

        for(nfila in 1:(nrow(frecuencias)-1)){
          for(ncolumna in 1:length(seleccion)){
            for(comp.columna in 1:length(seleccion)){
              if (ncolumna != comp.columna) {

                #Hacemos la primer comparación
                objetivo<-round(minibase[nfila,ncolumna],0)
                objetivoTotal <- unlist(round(totales[ncolumna], 0))

                competidor <- round(minibase[nfila,comp.columna],0)
                competidorTotal <- unlist(round(totales[comp.columna], 0))

                if (objetivo > 0 & competidor > 0 & objetivo !=
                    objetivoTotal & competidor != competidorTotal) {
                  if (prop.test(x = c(objetivo, competidor),
                                n = c(objetivoTotal, competidorTotal),
                                alternative = "greater", correct = T)$p.value <
                      0.05) {
                    diferencias.aux[nfila, ncolumna] <- paste(diferencias.aux[nfila, ncolumna], " ", letras[comp.columna], " ", sep = "")
                  }
                  else {
                    diferencias.aux[nfila, ncolumna] <- paste(diferencias.aux[nfila, ncolumna], "", sep = "")  }  }  }}
          }
        }
      }
      #Pegamos el total
      diferencias.aux<-rbind(diferencias.aux,frecuencias[nrow(frecuencias),seleccion])
      return(diferencias.aux)
    }


    #### Calculamos las diferencias para los niveles de cada variable por separado
    letras = c(LETTERS, letters)
    prop<-gsub(" ","",toupper(prop))
    diferencias<-NA
    if(prop == "NIVELES"){
      for(columna in columnas){
        #Se eligen las variables para calculas las diferencias
        seleccion<-nombresR(frecuencias,paste0("^",columna,":::"))
        nombresdiferencias.aux<-paste0(seleccion,":::f(",letras[1:length(seleccion)],")")

        if(is.null(nrow(diferencias))){
          nombresdiferencias<-nombresdiferencias.aux
          diferencias<-Diferencias(frecuencias,seleccion)
        }else{
          nombresdiferencias<-c(nombresdiferencias,nombresdiferencias.aux)
          diferencias<-cbind(diferencias,Diferencias(frecuencias,seleccion))
        }
      }
      #Pegamos los nombres de las filas
      diferencias<-cbind(frecuencias[,"Filas"],diferencias)
    }

    #### Calculamos las diferencias para las columnas

    if(prop == "COLUMNAS"){
      #Se eligen las variables para calculas las diferencias

      if(length(which(toupper(colnames(frecuencias))%in%c("FILAS",toupper(colnames(frecuencias))[grep("^TOTAL", toupper(colnames(frecuencias)))] )))>0){

        seleccion<-names(frecuencias)[-which(toupper(colnames(frecuencias))%in%c("FILAS",toupper(colnames(frecuencias))[grep("^TOTAL", toupper(colnames(frecuencias)))] ))]
        nom.total<-names(frecuencias)[which(toupper(colnames(frecuencias))%in%c(toupper(colnames(frecuencias))[grep("^TOTAL", toupper(colnames(frecuencias)))] ))]
        nombresdiferencias<-c(paste0(nom.total,":::f(",letras[1:length(nom.total)],")"),paste0(seleccion,":::f(",letras[1:length(seleccion)],")"))
        diferencias.total<-Diferencias(frecuencias,nom.total)
        diferencias<-Diferencias(frecuencias,seleccion)
        #Pegamos los nombres de las filas
        diferencias<-cbind(frecuencias[,"Filas"],diferencias.total,diferencias)

      }else{
        seleccion<-names(frecuencias)[-which(toupper(colnames(frecuencias))%in%c("FILAS",toupper(colnames(frecuencias))[grep("^TOTAL", toupper(colnames(frecuencias)))] ))]
        nombresdiferencias<-paste0(seleccion,":::f(",letras[1:length(seleccion)],")")
        diferencias<-Diferencias(frecuencias,seleccion)
        #Pegamos los nombres de las filas
        diferencias<-cbind(frecuencias[,"Filas"],diferencias)

      }
    }

    ######## Añadimos la base natural
    if(base.natural == TRUE){
      colnames(total.natural)<-colnames(diferencias)
      diferencias<-rbind(diferencias,total.natural)
    }
    colnames(diferencias)<-c("Filas",nombresdiferencias)
  }

  if(tipo == "F"){
    return(frecuencias)
  }else if(tipo == "P"){
    return(porcentajes)
  }else if(tipo == "D"){
    return(diferencias)
  }

  #######Apartir de aqui hacemos las tablas convinadas

  # Frecuencias y porcentajes, es el argumento predeterminado
  if(tipo %in% c("FP","PF")){
    #Intercalamos los nombres de las columnas
    for(i in 1:length(colnames(frecuencias))){
      if(i == 1){
        orden.columnas<-colnames(frecuencias)[1]
      }else{
        orden.columnas<-c(orden.columnas,colnames(frecuencias)[i],colnames(porcentajes)[i])
      }
    }
    resultado<-cbind(frecuencias,porcentajes[,-1])
    colnames(resultado)<-c(names(frecuencias),names(porcentajes)[-1])
    resultado<-resultado[,orden.columnas]
    return(resultado)
  }

  # Porcentajes con diferencias
  if(tipo %in% c("PD","DP")){

    if(numero.totales == 1){
      resultado<-round(porcentajes[-nrow(porcentajes),-1],0)
      resultado<-cbind(porcentajes[-nrow(porcentajes),1],resultado)
      colnames(resultado)<-colnames(diferencias)
      resultado<-rbind(resultado,diferencias[nrow(diferencias),])

    }else if(numero.totales == 2){
      resultado<-round(porcentajes[-c(nrow(porcentajes),nrow(porcentajes)-1),-1],0)
      resultado<-cbind(porcentajes[-c(nrow(porcentajes),nrow(porcentajes)-1),1],resultado)
      colnames(resultado)<-colnames(diferencias)
      resultado<-rbind(resultado,diferencias[c(nrow(diferencias)-1,nrow(diferencias)),])
    }

    for(i in 2:ncol(resultado)){
      for(j in 1:(nrow(resultado)-numero.totales)){
        resultado[j,i]<-paste0(resultado[j,i],"%",diferencias[j,i])
      }
    }
    return(resultado)
  }


  ## Resultado tablas convinadas

  ###Funcion para dar formato a la tabla
  # form<-function(tabla,num.niveles,formato){
  #   if(num.niveles>1){
  #     frecuencias.formato<-NA
  #     frecuencias<-tidyr::separate(frecuencias,Filas,paste0("Filas",num.niveles:1),sep=":::",remove = TRUE)
  #
  #     for(i in 2:num.niveles){
  #       niveles<-unique(frecuencias[,paste0("Filas",i)])
  #       for(nivel in niveles){
  #         frecuencias.aux<-frecuencias[frecuencias[,paste0("Filas",i)]%in%nivel,]
  #         #Aqui debo poner si el formato es para una columna o para muchas
  #         #Es mejor desarrollar suponiendo 3 filas y probar concatenando dos de ellas
  #         if(metodo%in%"unico"){
  #
  #
  #         }else if(metodo%in%"multiple"){
  #
  #
  #         }
  #
  #         if(is.null(nrow(frecuencias.formato))){
  #           frecuencias.formato<-frecuencias.aux
  #         }else{
  #           frecuencias.formato<-rbind(frecuencias.formato,frecuencias.aux)
  #         }
  #       }
  #     }
  #   }
  # }

}


