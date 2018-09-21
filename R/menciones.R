#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param data Pendiente
#'@param variable Pendiente
#'@param palabrasNegras Pendiente
#'@param identificador Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'pendiente

menciones <- function(data,
                      variable,
                      palabrasNegras,
                      identificador) {
  ##palabrasNegras se mete en forma c('NO SABE','otracosa')
  #Filtro variables de interés
  tmp <- data[, grepl(variable, names(data))]

  #Construyo un dataframe donde copiaré los datos limpios
  d <- as.data.frame(matrix(ncol = length(tmp), nrow = nrow(tmp)))


  ####Quito códigos repetidos
  for (i in 1:nrow(tmp)) {
    for (j in 1:length(tmp)) {
      if (j == 1) {
        d[[1]] <- tmp[[1]]
      } else if (!is.na(tmp[i, j]) &
                 match(tmp[i, j], d[i, ], nomatch = -1) < 0) {
        d[i, j] <- tmp[i, j]
      } else
        d[i, j] <- NA
    }
  }

  names(d) <- names(tmp)


  ######quitar NO SABE, NO SABE/NO RECUERDA

  conteos <- apply(d, 1, function(x)
    length(x[!is.na(x)]))
  for (i in 1:nrow(d)) {
    for (j in 1:length(d)) {
      if (!is.na(d[i, j])) {
        #if(d[i,j]%in%c("NO CONTESTÓ","NO SABE/NO RECUERDA","NO SABE/NO CONTESTÓ")&(j>1)&conteos[[i]]>1){
        if (d[i, j] %in% palabrasNegras & (j > 1) & conteos[[i]] > 1) {
          d[i, j] <- NA
        }
      }
    }
  }

  #### Obtengo los niveles de las menciones

  niveles <- NULL
  for (i in 1:length(d)) {
    niveles <- c(niveles, d[, i])
  }

  niveles <- unique(niveles)
  niveles <- na.omit(niveles)
  niveles <- as.character(niveles)

  ###Convierto a factor cada variable con los mismos levels obtenidos-------
  for (i in 1:length(d)) {
    d[, i] <- factor(d[, i], levels = niveles)
  }

  # Uno el ID con las variables limpias ------

  final <- cbind(data[, identificador], d)
  names(final)[1] <- identificador

  return(final)
}
