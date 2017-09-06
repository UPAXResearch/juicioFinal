####Funcion topshare----------------
funcionConteosAcumulados<-function(datos,bloques,nombre){
  require(stringr)
  base<-datos
  
  #programo el pedazo de código que usa al parámetro bloques
  b<-paste0('dplyr::select(',paste0('contains("',bloques,'")',collapse=','),')')
  
  #subconjunto con las variables
  eval(parse(text=paste(
    'bs<-base %>%',b
  )))
  
  #comienzo el conteo
  for(i in 1:(length(bs)/length(bloques))){
    eval(parse(text=paste(
      'bss<-bs %>%
      dplyr::select(contains(str_sub(names(bs)[i],4))) %>%
      mutate_each(funs(as.numeric)) %>%
      mutate(',paste0("conteo",str_sub(names(bs)[i],4)),'=rowSums(.[1:length(bloques)]))'
    )))
    base<-cbind(base,bss[,(length(bloques)+1)])
    names(base)[length(base)]<-paste0('bss',i)
  }
  
  #me quedo con  las que si quiero, es decir las de conteo
  base<-base %>%
    dplyr::select(contains('bss'))
  
  #pongo los nombres
  names(base)<-paste0(nombre, names(bs)[1:(length(bs)/length(bloques))])
  
  #me quedo sólo con ceros y unos
  base<-base-length(bloques)
  for(i in 1:nrow(base)){
    for(j in 1:length(base)){
      base[i,j]<-min(1,base[i,j])
    }
  }
  
  return(base)
  
}

