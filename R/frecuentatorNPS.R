#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param fTtabla La tabla principal i.e. "la base de datos"
#'@param fTvariables La variable o nombres de variables de quien se va a extraer informacion (i.e. frecuencias)
#'@param fTlevels Uso los levels de las variables en fvariables? TRUE= Uso los levels, FALSE= uso los nombres de las variables, i.e. FALSE= mi variable(s) son lógicas
#'@param fbanner Las variables que van por banner, en caso de que se necesite
#'@param fTanidado Pendiente
#'@param fTponderador Nombre de la variable ponderador, en caso de que exista
#'@param fTsobreQuien Fijar un total para todos los cálculos de porcentaje
#'@param fTtotal Agregar una fila de total en los resultados finales?
#'@param fTprop Hacer prueba de proporciones? En vez de regresar la tabla de frecuencias se regresa una tabla de prueba de proporciones (igual a las de SPSS)
#'@param fTusarNA Frecuentator omite por default los NA de las variables que le pedimos, pero a veces se necesitan i.e. Cuando agrupo variables para Share of Mind
#'@param fTdecimales El redondeo de porcentaje a cuantos decimales debe ser? El default es 1
#'@param fTtipo Pendiente
#'@param fTunion Pendiente
#'@param fTescala Pendiente
#'@param fTpctConDif Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'pendiente

frecuentatorNPS <-
  function(fTtabla,
           fTvariables,
           fTlevels = T,
           fbanner = NULL,
           fTanidado = NULL,
           fTponderador = NULL,
           fTsobreQuien = NULL,
           fTtotal = T,
           fTprop = F,
           fTusarNA = F,
           fTdecimales = 4,
           fTtipo = NULL,
           fTunion = F,
           fTescala = NULL,
           fTpctConDif = F) {
    if (fTpctConDif == T) {
      fTdecimales = 0
      fTtipo = "P"
      fTunion = T
    }

    if (is.null(fTescala)) {
      fTescala = 10
    }

    #tengo base?
    if (nrow(fTtabla) == 0) {
      FINAL <- data.frame()
      FINAL <- data.frame(row.names = c("Vacio"))
      FINAL$Respuesta <- as.factor("Base sin Casos")
      return(FINAL)
    } else{
      #Bloque de If donde creamos los rangos de escalas
      if (fTescala == 7) {
        rangos <- c('1 a 3', '1 a 3', '1 a 3', '4 a 5', '4 a 5', '6 a 7', '6 a 7')
      } else if (fTescala == 5) {
        rangos <- c('1 a 2', '1 a 2', '3', '4 a 5', '4 a 5')
      } else if (fTescala == 11) {
        rangos <-
          c('0 a 6',
            '0 a 6',
            '0 a 6',
            '0 a 6',
            '0 a 6',
            '0 a 6',
            '0 a 6',
            '7 a 8',
            '7 a 8',
            '9 a 10',
            '9 a 10')
      } else if (fTescala == 10) {
        rangos <-
          c('1 a 6',
            '1 a 6',
            '1 a 6',
            '1 a 6',
            '1 a 6',
            '1 a 6',
            '7 a 8',
            '7 a 8',
            '9 a 10',
            '9 a 10')
      } else{
        stop(
          paste(
            "\n frecuentatorNPS Error01: fTescala = ::",
            fTescala,
            ":: no es una escala válida, usa como valor 5, 7, 10 u 11"
          )
        )
      }

      #Si es numérica mi variable la vuelvo factor usando escala numérica
      if (is.numeric(fTtabla[, fTvariables])) {
        warning(
          paste(
            "\n frecuentatorNPS Advertencia: La variable ::",
            fTvariables,
            ":: en fTtabla es numérica, voy a nivelarla y escalar a ",
            fTescala
          )
        )

        #Solución chafa a escala 11 que inicia con 0
        if (fTescala == 11) {
          fTtabla[, fTvariables] <-
            factor(fTtabla[, fTvariables], c(0:(fTescala - 1)))
        } else{
          fTtabla[, fTvariables] <- factor(fTtabla[, fTvariables], c(1:fTescala))
        }
      }


      if (!nlevels(fTtabla[, fTvariables]) == fTescala) {
        stop(
          paste(
            "\n frecuentatorNPS Error02: No tienes en ::",
            fTvariables,
            ":: los niveles requeridos"
          )
        )
      }

      #Base es un dataframe con el que calcularé los segmentos
      base <- fTtabla
      levels(base[, fTvariables]) <- rangos

      #Frecuentator normal a los niveles pedidos
      tabla <-
        frecuentator(
          fTtabla,
          fTvariables,
          fTlevels,
          fbanner,
          fTanidado,
          fTponderador,
          fTsobreQuien,
          fTtotal,
          fTprop,
          fTusarNA,
          fTdecimales,
          fTtipo,
          fTunion
        )

      tabla <- tabla[-nrow(tabla), ]

      #Frecuentator normal a los segmentos
      tabla2 <-
        frecuentator(
          base,
          fTvariables,
          fTlevels,
          fbanner,
          fTanidado,
          fTponderador,
          fTsobreQuien,
          fTtotal,
          fTprop,
          fTusarNA,
          fTdecimales,
          fTtipo,
          fTunion
        )

      totales <- tabla2[nrow(tabla2), ]
      tabla2 <- tabla2[-nrow(tabla2), ]

      i = 2
      #Calculos del NPS a la mala, como siempre se tienen tres niveles nada raro debe pasar
      NPS <- NULL
      for (i in 1:length(tabla)) {
        if (is.numeric(tabla2[1, i])) {
          NPS <- c(NPS, tabla2[3, i] - tabla2[1, i])
        } else{
          NPS <- c(NPS, "")
        }
      }

      NPS[1] <- 'NPS'

      FINAL <- rbind(tabla, tabla2)
      FINAL[, 1] <- as.character(FINAL[, 1])
      FINAL <- rbind(FINAL, NPS)

      FINAL <- rbind(FINAL, totales)

      ####Forzado de union de diferencias

      if (fTpctConDif) {
        totales <-
          FINAL[nrow(totales), grep(pattern = "pct$",
                                    x = colnames(FINAL),
                                    perl = T)]
        FINAL[nrow(FINAL), grep(pattern = "pct$",
                                x = colnames(FINAL),
                                perl = T)] <- totales

        #guardo las bases para luego pegarlas bajo las columnas de dif.sig. si se quiere frecuencias todo el siguiente chuck debe comentarse, en un momento lo haré TRUE/FALSE
        bases <-
          FINAL[nrow(FINAL), grep(pattern = "f\\([[:upper:]]\\)$",
                                  x = colnames(FINAL),
                                  perl = T)]

        #FINAL[nrow(FINAL), grep(pattern = "(f)$", x = colnames(FINAL), perl = T)]<-bases
        FINAL[nrow(FINAL), grep(pattern = "pct$",
                                x = colnames(FINAL),
                                perl = T)] <- bases

        ######Final

        #i=2
        for (i in 1:length(FINAL[grep(pattern = "pct$",
                                      x = colnames(FINAL),
                                      perl = T)])) {
          #Pegado de simbolo % a frecuencias

          pegadoPct <-
            c(paste0(FINAL[grep(pattern = "pct$",
                                x = colnames(FINAL),
                                perl = T)][, i], "%"))
          #FINAL[grep(pattern = "pct$", x = colnames(FINAL), perl = T)][i]<-pegadoPct
          FINAL[-nrow(FINAL) , grep(pattern = "pct$",
                                    x = colnames(FINAL),
                                    perl = T)][i] <- pegadoPct[-length(pegadoPct)]



          #Elimina espacios entre diferencias significativas
          #NOTA, LAS DIFERENCIAS ESTAN ALMACENADAS COMO LISTAS
          colDif <-
            (FINAL[grep(pattern = "f\\([[:upper:]]\\)$",
                        x = colnames(FINAL),
                        perl = T)][[i]])

          pegadoDif <-
            gsub(
              pattern = " ",
              replacement = "",
              x = colDif ,
              fixed = T
            )

          #Pegado de diferencias significativas a porcentajes
          FINAL[-nrow(FINAL) , grep(pattern = "pct$",
                                    x = colnames(FINAL),
                                    perl = T)][i] <-
            paste0(FINAL[-nrow(FINAL) , grep(pattern = "pct$",
                                             x = colnames(FINAL),
                                             perl = T)][, i], " ", pegadoDif[-length(pegadoDif)])

        }

        nombres <-
          names(FINAL[grep(pattern = "f\\([[:upper:]]\\)$",
                           x = colnames(FINAL),
                           perl = T)])
        FINAL <-
          FINAL[c(1, grep(
            pattern = "pct$",
            x = colnames(FINAL),
            perl = T
          ))]
        colnames(FINAL)[-1] <- nombres
      }

      return(FINAL)
    }
  }
