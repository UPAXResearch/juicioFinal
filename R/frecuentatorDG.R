#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param fTtabla Pendiente
#'@param fTvariables Pendiente
#'@param fTlevels Pendiente
#'@param fbanner Pendiente
#'@param fTanidado Pendiente
#'@param fTponderador Pendiente
#'@param fTsobreQuien Pendiente
#'@param fTtotal Pendiente
#'@param fTprop Pendiente
#'@param fTusarNA Pendiente
#'@param fTdecimales Pendiente
#'@param fTtipo Pendiente
#'@param fTunion Pendiente
#'@param fTpctConDif Pendiente
#'@param bases Pendiente
#'@param etiquetas Pendiente
#'@param usarbases Pendiente
#'@param tablaUnica Pendiente
#'@param variablesIguales Pendiente
#'@param eVariables Pendiente
#'@export
#'@keywords frecuencias
#'@examples
#'pendiente

frecuentatorDG <-
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
           fTpctConDif = F,
           bases = NULL,
           etiquetas = NULL,
           usarbases = F,
           tablaUnica = T,
           variablesIguales = T,
           eVariables = NULL) {
    #anotacion de ser una serie de frecuentators anidados o no
    if (!is.null(fTanidado)) {
      esAnidado = T
    } else{
      esAnidado = F
    }

    #Revisa que tenga un elemento para fungir de base
    if (is.null(bases)) {
      stop(
        paste(
          "\n datagrind Error01: No hay variables para bases, usar frecuentator regular"
        )
      )
    }

    #Revisa que los elementos en base existan dentro de base
    if (all(!bases %in% names(fTtabla))) {
      stop(
        paste(
          "\n datagrind Error02: Las variables para base::",
          paste(bases[!bases %in% names(fTtabla)], collapse = ", "),
          ":: no existen en fTtabla"
        )
      )
    }

    if (length(bases) > 1) {
      esfactor = F
      cuantos = length(bases)
      nomLista = bases
    } else{
      esfactor = T
      cuantos = nlevels(as.factor(fTtabla[, bases]))
      nomLista = levels(as.factor(fTtabla[, bases]))
    }

    lista <- list()

    for (i in 1:cuantos) {
      base <- fTtabla
      if (esfactor == F) {
        base$filtroBase <- as.factor(base[, bases[i]])
        levels(base$filtroBase) <- rep(TRUE, nlevels(base$filtroBase))
        variable = bases[i]
      } else{
        base$filtroBase <- factor(base[, bases], nomLista[i])
        levels(base$filtroBase) <- TRUE
        variable = bases
      }

      if (usarbases == F) {
        lista[[nomLista[i]]] = frecuentator(
          fTtabla = base[base$filtroBase %in% T, ],
          fTvariables = fTvariables,
          fTlevels = fTlevels,
          fbanner = fbanner,
          fTanidado = fTanidado,
          fTponderador = fTponderador,
          fTsobreQuien = fTsobreQuien,
          fTtotal = fTtotal,
          fTprop = fTprop,
          fTusarNA = fTusarNA,
          fTdecimales = fTdecimales,
          fTtipo = fTtipo,
          fTunion = fTunion,
          fTpctConDif = fTpctConDif
        )
      } else{
        lista[[nomLista[i]]] = frecuentator(
          fTtabla = base[base$filtroBase %in% T, ],
          fTvariables = variable,
          fTlevels = fTlevels,
          fbanner = fbanner,
          fTanidado = fTanidado,
          fTponderador = fTponderador,
          fTsobreQuien = fTsobreQuien,
          fTtotal = fTtotal,
          fTprop = fTprop,
          fTusarNA = fTusarNA,
          fTdecimales = fTdecimales,
          fTtipo = fTtipo,
          fTunion = fTunion,
          fTpctConDif = fTpctConDif
        )
      }

    }

    Resultado <-
      datagrind(
        lista = lista,
        esAnidado = esAnidado,
        etiquetas = etiquetas,
        tablaUnica = tablaUnica,
        variablesIguales = variablesIguales,
        eVariables = eVariables,
        colapsarPor = colapsarPor
      )
    return(Resultado)
  }
