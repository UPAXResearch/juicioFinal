#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param datos Pendiente
#'@param libro.codigos Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'# Se carga el libro de codigos
#'# libro.codigos<-read_xlsx("Base Abiertas Final/Relación Otros con sus columnas nuevas.xlsx")
#'# la estructura del archivo libro de codigos es de 5 columnas: Expresion regular otros|COD.|Desc|Columna|Count
#'# "Expresion regular otros" se pone la expresion regular para que el codigo encuentre la columna que contiene la codificación de otros
#'# "COD" es el codigo de los otros
#'# "Desc" es la descripción del codigo
#'# "Columna" es la variable donde el codigo va a distribuir a los otros
#'# "Count" en esta columna la funcion indica cuantos valores de otros movio a la columna indicada
#'# otros.acomodado<-acomodo.otros(datos,libro.codigos)

acomodo_otros <- function(datos, libro.codigos) {
  #Armamos los bloques de preguntas
  libro.codigos$Count <- 0
  libro.codigos$Duplicados <- 0
  boque.count <- 0
  libro.codigos$bloque <- 0

  for (i in c(1:nrow(libro.codigos))) {
    if (!is.na(libro.codigos[i, 1])) {
      boque.count <- boque.count + 1
      libro.codigos$bloque[i] <- boque.count
    } else{
      libro.codigos$bloque[i] <- boque.count
    }
  }

  for (j in c(1:boque.count)) {
    bloque.activo <- subset(libro.codigos, bloque == j)
    columna.otros <-
      which(colnames(datos) == as.character(bloque.activo[1, 1]))

    for (i in c(1:nrow(datos))) {
      ### iteramos sobre la columna otros, revisamos que si tiene algo

      if (!is.na(datos[i, columna.otros]) &
          datos[i, columna.otros] != "") {
        #buscamos el codigo de la columna otros en el libro de codigos
        posicion.bloque <-
          which(bloque.activo$COD. == as.character(datos[i, columna.otros]))

        columna.datos <-
          which(colnames(datos) == bloque.activo$Columna[posicion.bloque])

        if (length(columna.datos) == 0) {
          ## se tiene que abrir otra columna

          eval(parse(text = paste0(
            "datos$", bloque.activo$Columna[posicion.bloque], "<- NA"
          )))
          columna.datos <-
            which(colnames(datos) == bloque.activo$Columna[posicion.bloque])

          #Ponemos un true en el valor de la columna que acabamos de crear
          datos[i, columna.datos] <- 1
          bloque.activo[posicion.bloque, "Count"] <-
            bloque.activo[posicion.bloque, "Count"] + 1
        } else{
          if (!is.na(datos[i, columna.datos])) {
            bloque.activo[posicion.bloque, "Duplicados"] <-
              bloque.activo[posicion.bloque, "Duplicados"] + 1
          }
          datos[i, columna.datos] <- 1
          bloque.activo[posicion.bloque, "Count"] <-
            bloque.activo[posicion.bloque, "Count"] + 1
        }
      }
    }

    if (j == 1) {
      bloque.inactivo <- bloque.activo
    } else{
      bloque.inactivo <- rbind(bloque.inactivo, bloque.activo)
    }
  }

  a <- list(datos, bloque.inactivo)
  return(a)
}
