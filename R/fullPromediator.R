#'Titulo Pendiente
#'
#'Descripción pendiente
#'@param fTtabla La tabla principal i.e. "la base de datos"
#'@param fTvariable La variable o nombres de variables de quien se va a extraer informacion (i.e. frecuencias)
#'@param fbanner Las variables que van por banner, en caso de que se necesite
#'@param fTanidado Pendiente
#'@param fTponderador Nombre de la variable ponderador, en caso de que exista
#'@param fTdecimales El redondeo de porcentaje a cuantos decimales debe ser? El default es 1
#'@export
#'@keywords frecuencias
#'@examples
#'pendiente

fullPromediator <-
  function (fTtabla,
            fTvariable,
            fbanner = NULL,
            fTanidado = NULL,
            fTponderador = NULL,
            fTdecimales = 1) {
    if (nrow(fTtabla) == 0) {
      FINAL <- data.frame()
      FINAL <- data.frame(row.names = c("Vacio"))
      FINAL$Respuesta <- as.factor("Base sin Casos")
      warning("\n La base objetivo está vacia")
      return(FINAL)
    } else{
      #Antiguos espíritus del mal, transformen esta función decadente en MonRa, el inmortal!!! (o algo medianamente funcional)
      if (is.null(fbanner)) {
        fTtabla$Total <- factor(rep(x = 1, nrow(fTtabla)), levels = 1,
                                labels = "Total")
        fbanner <- "Total"
      }
      misVarz <- unique(c(fTvariable, fbanner, fTanidado))
      if (!all(misVarz %in% names(fTtabla))) {
        stop(
          paste(
            "\n fullPromediator Error01: No existe la variable ::",
            paste(misVarz[!misVarz %in% names(fTtabla)], collapse = ", "),
            ":: en fTtabla"
          )
        )
      }


      ####Forzado pegado de código anidado
      if (!is.null(fTanidado)) {
        nuevas <- NULL
        #Descomentar si se quiere respetar el total
        #fbanner2<-fbanner[!fbanner=="Total"]
        fbanner2 <- fbanner
        #i="F01_Genero"
        for (i in fTanidado) {
          nAnid <- levels(fTtabla[, i])
          #j="PLAZA"
          for (j in fbanner2) {
            if (!is.factor(fTtabla[, j]))
            {
              fTtabla[, j] <- as.factor(fTtabla[, j])
            }
            nBann <- levels(fTtabla[, j])
            #k="Oaxaca"
            for (k in nBann) {
              columna <- factor(apply(fTtabla[, c(j, i)], 1, function(x) {
                if (!any(is.na(x))) {
                  if ((x[1] == k & x[2] %in% c(nAnid))) {
                    return(as.character(paste0(x[1], ":::", x[2])))
                  } else{
                    return(NA)
                  }
                } else{
                  return(NA)
                }
              }), c(paste0(k, ":::", levels(
                fTtabla[, i]
              ))))
              fTtabla[, ncol(fTtabla) + 1] <- columna
              names(fTtabla)[ncol(fTtabla)] <-
                paste0(k, "_", j, "_", fTanidado)
              nuevas <- c(nuevas, paste0(k, "_", j, "_", fTanidado))
            }
          }
        }
        fbanner <- fbanner[which(!fbanner %in% fbanner2)]
        fbanner <- c(fbanner, nuevas)
        misVarz <- unique(c(fTvariable, fbanner, fTanidado))

        #fTtabla<-subset(x = fTtabla,select = c(misVarz,fTponderador))

        ###A la mala vuelvo factor las variables por si se requieren

        # if(any(!is.factor(fTtabla[,fTvariables]))){
        #   for (i in fTvariables) {
        #     fTtabla[,i]<-as.factor(fTtabla[,i])
        #   }
        # }
      }

      #################

      fTtabla <-
        subset(x = fTtabla, select = c(misVarz, fTponderador))
      if (is.null(fTponderador)) {
        fTponderador <- "pTemp"
        fTtabla[, fTponderador] <- 1
      } else {
        if (!all(!is.na(fTtabla[, fTponderador]))) {
          stop(
            paste(
              "\n fullPromediator Error02: El ponderador ::",
              fTponderador,
              ":: tiene valores NA en la observación : ",
              (1:nrow(fTtabla))[is.na(fTtabla[, fTponderador])],
              " en fTtabla"
            )
          )
        }
        if (!is.numeric(fTtabla[, fTponderador])) {
          stop(
            paste(
              "\n fullPromediator Error03: El ponderador ::",
              fTponderador,
              ":: no es numerico"
            )
          )
        }
      }
      if (all(is.na(fTtabla[, fTvariable]))) {
        warning("\n La variable objetivo está llena de NA")
        return(data.frame(vacio = "sin casos"))
      }
      if (!all(sapply(X = fTtabla[, fbanner], is.factor))) {
        stop(
          paste(
            "\n fullPromediator Error04: No es factor la variable ::",
            paste(misVarz[!sapply(X = fTtabla[,
                                              fbanner], is.factor)], collapse = ", "),
            ":: en fTtabla"
          )
        )
      }
      misVarz <- c(fTvariable, fbanner)
      if (!is.numeric(fTtabla[, fTvariable])) {
        stop(
          paste(
            "\n fullPromediator Error05: No es numeric la variable ::",
            fTvariable,
            ":: en fTtabla"
          )
        )
      }

      #Apuntar los elementos que quieres generar
      FINAL <-
        data.frame(
          Respuesta = c(
            'Promedio',
            'Mediana',
            'Varianza',
            'DesvEst',
            'Maximo',
            'Minimo'
          ),
          stringsAsFactors = F
        )
      #  FINAL <- data.frame()
      FINAL[nrow(FINAL) + 1, "Respuesta"] <- "Total_Casos_Base"
      # row.names(FINAL) <- FINAL$Respuesta
      #FINAL<-NULL
      pb <- txtProgressBar(min = 0,
                           max = length(fbanner),
                           style = 3)
      #fi=1
      for (fi in 1:length(fbanner)) {
        setTxtProgressBar(pb, fi)
        fbannerMini <- fbanner[fi]
        firespuestas <- levels(fTtabla[, fbannerMini])
        #ft=1
        for (ft in 1:length(firespuestas)) {
          factual <- firespuestas[ft]
          suba <- fTtabla[fTtabla[, fbannerMini] == factual &
                            !is.na(fTtabla[, fbannerMini]),]
          final <- data.frame()

          #Que pasa cuando no hay casos, debo dar 0
          sub <- suba[!is.na(suba[, fTvariable]),]
          if (nrow(sub) == 0) {
            a <- 0
            b <- 0
            c1 <- 0
            c <- 0
            e <- 0
            f <- 0
          }
          #Que pasa cuando solo hay un caso, debe ir lo normal
          else if (nrow(sub) == 1) {
            a <-  sub[1, fTvariable]
            b <-  sub[1, fTvariable]
            c1 <-  0
            c <-  0
            e <-  sub[1, fTvariable]
            f <-  sub[1, fTvariable]
          }
          else {
            z <- survey::svydesign(data = sub,
                                   ids = ~ 1,
                                   weights = sub[, fTponderador])
            a <- data.frame(survey::svymean(as.formula(paste(
              "~",
              fTvariable, sep = ""
            )), z, na.rm = T))

            a <- a$mean

            b <-
              ifelse(
                length(suba) > 2,
                b <- as.data.frame(survey::svyquantile(
                  as.formula(paste("~",
                                   fTvariable, sep = "")), z, c(.5), na.rm = T
                )),
                b <- data_frame('NA')
              )

            b <- b[[1]]

            c <- data.frame(survey::svyvar(as.formula(paste(
              "~",
              fTvariable, sep = ""
            )), z, na.rm = T))

            c1 <- c$variance
            c <- sqrt(c$variance)

            #Este es Maximo
            e <-
              ifelse(
                length(suba) > 2,
                e <- as.data.frame(survey::svyquantile(
                  as.formula(paste("~",
                                   fTvariable, sep = "")), z, c(1), na.rm = T
                )),
                e <- data_frame('NA')
              )

            e <- e[[1]]

            #Este es Minimo
            f <-
              ifelse(
                length(suba) > 2,
                f <- as.data.frame(survey::svyquantile(
                  as.formula(paste("~",
                                   fTvariable, sep = "")), z, c(0), na.rm = T
                )),
                f <- data_frame('NA')
              )

            f <- f[[1]]
          }
          d <-
            data.frame(var1 = c(
              Promedio = a,
              Mediana = b,
              Varianza = c1,
              DesvEst = c,
              Máximo = e,
              Mínimo = f
            ))

          final <- d

          sobreQuienFinal <- sum(sub[, fTponderador])
          final['Total', ] <- sobreQuienFinal
          #final<-final[,c('Promedio','Mediana','DesvEst','SE')]
          # final <- cbind(final, data.frame(Promedio = sobreQuienFinal,
          #                                  Mediana  = sobreQuienFinal,
          #                                  DesvEst  = sobreQuienFinal,
          #                                  SE = sobreQuienFinal,row.names = c("Total_Casos_Base")))
          # final <- subset(x = final, select = c('Promedio','Mediana','DesvEst'))


          final['Promedio', ] = round(final['Promedio', ], fTdecimales)
          final['Varianza', ] = round(final['Varianza', ], fTdecimales)
          final['DesvEst', ] = round(final['DesvEst', ], fTdecimales)

          names(final) <- paste(fbannerMini, factual,
                                sep = ":::")
          quiensoy <- paste(fbannerMini, factual,
                            sep = ":::")
          FINAL[quiensoy] <- final

          #row.names(FINAL) <- FINAL$Row.names
          #FINAL <- FINAL[, -1]
          #print(fbanner[fi])
        }
      }
      #FINAL<-do.call(cbind,FINAL)
      rownames(FINAL) <-
        c('Promedio',
          'Mediana',
          'Varianza',
          'DesvEst',
          'Máximo',
          'Mínimo',
          'TotalBase')
      close(pb)
      return(as.data.frame(FINAL))
    }
  }
