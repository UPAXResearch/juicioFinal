#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param datos Pendiente
#'@param variables Pendiente
#'@param muestra Pendiente
#'@param proporciones Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'muestrator(base,c("estado", "lada"),1200)

muestrator <- function(datos, variables, muestra, proporciones = NULL) {
  vars <- rlang::syms(variables)

  if (is.null(proporciones)) {
    # Se construye una variable para hacer las agrupaciones con dplyr
    datos <- mutate(datos, seleccion = paste(!!!vars, sep = "-"))
    proporciones <- datos
  } else{
    datos <- mutate(datos, seleccion = paste(!!!vars, sep = "-"))
    proporciones <-
      mutate(proporciones, seleccion = paste(!!!vars, sep = "-"))
  }

  # Usando dplyr se calcula la tabla de frecuencias para la muestra
  tabla <- proporciones %>%
    group_by(seleccion) %>%
    summarise(Conteo = n()) %>%
    mutate(Procentaje = Conteo / sum(Conteo)) %>%
    mutate(Muestra = Procentaje * muestra)

  # Se convierte a data.frame y se redondean los valores para la muestra
  tabla <- as.data.frame(tabla)
  tabla$MuestraEntera <- round(tabla$Muestra)
  # tabla[nrow(tabla)+1,] <- c("Total", colSums(tabla[,2:5]))

  # Se seleccionan los datos para la muestra
  datos.muestra <- data.frame()
  for (i in 1:nrow(tabla)) {
    muestra1 <- as.data.frame(datos %>% filter(seleccion == tabla[i, 1]))
    datos.muestra.aux <- sample_n(muestra1, tabla[i, 5], replace = FALSE)
    datos.muestra <- rbind(datos.muestra, datos.muestra.aux)
  }
  # se almacena en una lista las tablas del diseño de la muestra y la base con la muestra
  resultado <- list(tabla, datos.muestra)

  return(resultado)
}
