#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param fTtabla La tabla principal i.e. "la base de datos"
#'@param fTvariables La variable o nombres de variables de quien se va a extraer informacion (i.e. frecuencias)
#'@param fTlevels Uso los levels de las variables en fvariables? TRUE= Uso los levels, FALSE= uso los nombres de las variables, i.e. FALSE= mi variable(s) son lógicas
#'@param fbanner Las variables que van por banner, en caso de que se necesite
#'@param fTponderador Nombre de la variable ponderador, en caso de que exista
#'@param fTsobreQuien Fijar un total para todos los cálculos de porcentaje
#'@param fTtotal Agregar una fila de total en los resultados finales?
#'@param fTprop Hacer prueba de proporciones? En vez de regresar la tabla de frecuencias se regresa una tabla de prueba de proporciones (igual a las de SPSS)
#'@param fTusarNA Frecuentator omite por default los NA de las variables que le pedimos, pero a veces se necesitan i.e. Cuando agrupo variables para Share of Mind
#'@param fTdecimales El redondeo de porcentaje a cuantos decimales debe ser? El default es 1
#'@export
#'@keywords frecuencias
#'@examples
#'frecuentatorNPS_viejo(fTtabla, fTvariables, fTlevels = T, fbanner = NULL, fTponderador = NULL, fTsobreQuien = NULL, fTtotal = T, fTprop = F, fTusarNA = F, fTdecimales = 1)

frecuentatorNPS_viejo <-
  function(fTtabla,
           fTvariables,
           fTlevels = T,
           fbanner = NULL,
           fTponderador = NULL,
           fTsobreQuien = NULL,
           fTtotal = T,
           fTprop = F,
           fTusarNA = F,
           fTdecimales = 1) {
    tabla <-
      frecuentator(
        fTtabla,
        fTvariables,
        fTlevels = T,
        fbanner = NULL,
        fTponderador = NULL,
        fTsobreQuien = NULL,
        fTtotal = T,
        fTprop = F,
        fTusarNA = F,
        fTdecimales = 1
      )
    categorias <-
      c("1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "10",
        "Total",
        "1a6",
        "7a8",
        "9a10",
        "NPS")
    tabla <- round(tabla[c(1:11), c(2:length(tabla))])
    for (k in 1:(length(tabla))) {
      tabla[12, k] <- sum(tabla[1:6, k])
      tabla[13, k] <- sum(tabla[7:8, k])
      tabla[14, k] <- sum(tabla[9:10, k])
    }

    for (i in seq(2, length(tabla), 2))  {
      if (!all(is.na(tabla[12:14, i]))) {
        if (sum(tabla[12:14, i]) > 100) {
          tabla[13, i] = (tabla[13, i] - (sum(tabla[12:14, i]) - 100))
        } else
        {
          if (sum(tabla[12:14, i]) < 100)   {
            tabla[13, i] = (tabla[13, i] - (sum(tabla[12:14, i]) - 100))
          }
          else{
            tabla[13, i] == tabla[13, i]
          }
        }
      }
    }
    for (k in 1:(length(tabla))) {
      tabla[15, k] <- tabla[14, k] - tabla[12, k]
    }
    rownames(tabla) <- categorias
    tabla <- cbind(categorias, tabla)
    return(tabla)

  }
