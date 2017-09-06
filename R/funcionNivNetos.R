
##Funcion para hacer que dos variables tengan los mismos niveles aunque en principio tengan distintos
funcionNivNetos<-function(nNdatos,nNVars){
  # nNdatos <- datotes
  # nNVars <- c("P5caj1","P5caj2")
  for(i in 1:length(nNVars)){
    # i <- 2
    nN <- c(nNVars[i], nNVars[!nNVars %in% nNVars[i]])
    myLevels <- NULL
    for(t in 1:length(nN)){
      myLevels <- c(myLevels, levels(x = nNdatos[,nNVars[t]]))
    }
    levels(nNdatos[,nNVars[i]])<-myLevels
    nNdatos[,nNVars[i]]<-factor(nNdatos[,nNVars[i]],levels=c(levels(nNdatos[,nNVars[1]])),ordered = TRUE)
  }
  return(nNdatos)
}