#'Titulo pendiente
#'
#'Descripción pendiente
#'@param data Descripción pendiente
#'@param variable Descripción pendiente
#'@param filtro Descripción pendiente
#'@param banner Descripción pendiente
#'@param fTanidado Descripción pendiente
#'@param fTprop Descripción pendiente
#'@param fTponderador Descripción pendiente
#'@param fTtipo Descripción pendiente
#'@param fTunion Descripción pendiente
#'@param fTpctConDif Descripción pendiente
#'@export
#'@keywords Pending
#'@examples
#'netoSubneto(data, variable, filtro, banner, fTanidado=NULL , fTprop=FALSE, fTponderador=NULL,  fTtipo=NULL, fTunion=F, fTpctConDif=F)

netoSubneto <- function(data, variable, filtro, banner, fTanidado=NULL , fTprop=FALSE, fTponderador=NULL,  fTtipo=NULL, fTunion=F, fTpctConDif=F){
  ###Para cable
  # data <- base
  # variable<-'P11.(1|2)_'
  # filtro<-'AdvTotal.cable_izzi'
  # # # ####

  tmp<-data[,grep(variable,names(data))]
  tmp<-tmp%>%dplyr::mutate_each(funs(as.character))

  menciones<-tmp[,grepl(".*SubCateg",names(tmp))]
  netos <- tmp[,grepl("_Categ",names(tmp))]

  d<-as.data.frame(matrix(ncol=length(menciones),nrow = nrow(menciones)))
  d2<-as.data.frame(matrix(ncol=length(netos),nrow = nrow(netos)))
  ###Quito duplicados sólo en menciones y pego en "d"  ------

  for(i in 1:nrow(menciones)){
    for(j in 1:length(menciones)){
      if(j==1){
        d[[1]]<-menciones[[1]]
      }else if(!is.na(menciones[i,j]) & match(menciones[i,j],d[i,],nomatch = -1)<0){
        d[i,j]<-menciones[i,j]
      }else d[i,j]<-NA
    }
  }

  ###Quito duplicados en los Netos y pego en d2  -----
  for(i in 1:nrow(netos)){
    for(j in 1:length(netos)){
      if(j==1){
        d2[[1]] <- netos[[1]]
      }else if(!is.na(netos[i,j]) & match(netos[i,j],d2[i,],nomatch = -1)<0){
        d2[i,j] <- netos[i,j]
      }else d2[i,j] <- NA
    }
  }

  names(d2) <- paste(names(netos),"REC",sep = "_")

  ###Concateno menciones limpias con su respectivo Neto (los Netos originales, aún repetidos)   -----
  nvasMenciones <- data.frame(matrix(ncol=0,nrow = nrow(menciones)))

  for(i in 1:length(d)){
    alfa <- paste(netos[,i],d[,i],sep = "_")
    nvasMenciones<-cbind(nvasMenciones,alfa)
  }

  names(nvasMenciones)<-paste(names(menciones),"menciones",sep="_")
  nvasMenciones <- nvasMenciones %>%dplyr::mutate_each(funs(as.character))

  #### Obtengo los niveles de los NETOS ----

  niveles <- NULL
  for (i in 1:length(d2)) {
    niveles <- c(niveles, d2[,i])
  }

  niveles <- unique(niveles)
  niveles <- na.omit(niveles)
  niveles <- as.character(niveles)

  ###Convierto a factor con los mismos levels-------
  for(i in 1:length(d2)){
    d2[,i]<-factor(d2[,i],levels = niveles)
  }


  #### Obtengo los niveles de las menciones concatenadas ----

  niveles <- NULL
  for (i in 1:length(nvasMenciones)) {
    niveles <- c(niveles, nvasMenciones[,i])
  }

  niveles <- unique(niveles)
  niveles <- na.omit(niveles)
  niveles <- as.character(niveles)

  ###Convierto a factor con los mismos levels-------
  for(i in 1:length(nvasMenciones)){
    nvasMenciones[,i]<-factor(nvasMenciones[,i],
                              levels = niveles,
                              exclude = c('_',"_NA"," ","",'NA_NA', grep(pattern = '_NA$', x = niveles, value = T, perl = T)))
    levels(nvasMenciones[,i])
  }

  finales <- cbind(nvasMenciones,d2)

  misVars <- cbind(data,finales)

  bandera=banner

  eval(parse(text=paste0( 'subneto<-frecuentator(misVars[misVars$',filtro,'==1,]',",nombresR(misVars,'",variable,".+?[menciones]$')",',fTlevels = T,fbanner =bandera, fTanidado = fTanidado, fTusarNA=T, fTdecimales=2, fTprop=fTprop, fTponderador=fTponderador, fTtipo=fTtipo, fTunion=fTunion, fTpctConDif=fTpctConDif)')))

  eval(parse(text=paste0( 'neto<-frecuentator(misVars[misVars$',filtro,'==1,]',",nombresR(misVars,'",variable,".+?[R][E][C]$')",',fTlevels = T,fbanner =bandera, fTanidado = fTanidado,fTusarNA=T, fTdecimales=2,fTprop=fTprop, fTponderador=fTponderador, fTtipo=fTtipo, fTunion=fTunion, fTpctConDif=fTpctConDif)')))

  prueba<-neto
  prueba[nrow(prueba)+1,] <- NA

  prueba$Respuesta<-as.character(prueba$Respuesta)
  prueba<-rbind(prueba, colnames(prueba))
  prueba<-rbind(prueba, subneto)

  #regreso<-list(neto=neto,subneto=subneto)
  regreso<-prueba
  return(regreso)
}
