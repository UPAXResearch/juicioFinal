#'Genera una tabla (base de datosgs de encuesta) una vez que se le pasa un .csv y un "catálogo" (en excel) que se hayan generado en "soda
#'@param xfile El .csv descargado de soda
#'@param yfile El excel de catálogo descargado desde soda
#'@export
#'@keywords soda
#'@examples 
#'datosgs <- gaseosa(xfile = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS 13-09-2016 18-23-09.csv",
#'yfile1 = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS_DataMap_13-09-2016 18_24_04.xlsx")

gaseosa <- function(xfile, yfile){
  # Donde
  # xfile Es el .csv
  # yfile Es el excel que baja de soda
  # xfile <- list.files("./datos",full.names = T)[1]
  # yfile <- list.files("./datos",full.names = T)[2]
  # 
  # xfile <- "./02Datos/SegundoLevantamiento/seguros_new12 03-03-2017 00-20-53.csv"
  # yfile <- "./02Datos/SegundoLevantamiento/seguros_new12_DataMap_01-03-2017 23_05_12.xlsx"
  # 
  #########################
  # Previos...
  
  # La base no se puede descargar en spss, voy a etiquetar a mano...
  
  datosgs <- read.csv(
    xfile,stringsAsFactors = F,na.strings = c("","NA")
  )

  etiquetasPregunta <- readxl::read_excel(path = yfile,sheet = 1)
  etiquetasVariable <- readxl::read_excel(path = yfile,sheet = 2)

  pb <- txtProgressBar(min = 0, max = length(datosgs), style = 3)
  for(i in 1:length(datosgs)){
    # i <- sample(1:ncol(datosgs),1)
    setTxtProgressBar(pb, i)
    names(datosgs[i])
    table(datosgs[,i])
    miDato <- names(datosgs)[i]
    # Uso subset porque quiero respetar la estructura de mis datosgs i.e. un data frame
    subdatosgs <- subset(datosgs, select = miDato)
    misEtiquetas <- etiquetasPregunta[etiquetasPregunta$Variable==miDato,]
    misEtiquetasVariable <- etiquetasVariable[etiquetasVariable$Variable==miDato,]

    # Qué tipo de dato tengo?
    if(nrow(misEtiquetas)>1){
      stop("Tengo nrow(misEtiquetas)>1 = ",nrow(misEtiquetas),"; tengo variables duplicadas?")
    }
    
    if(nrow(misEtiquetas)>0){
      tr <- as.character(misEtiquetas$Type)
      # 
      if(tr=="NUMERIC"){
        subdatosgs[,1] <- as.factor(as.character(subdatosgs[,1]))
      }else if(tr=="TEXT"){
        subdatosgs[,1] <- as.factor(as.character(subdatosgs[,1]))
      }else if(tr=="DATE"){
        subdatosgs[,1] <- as.factor(as.character(subdatosgs[,1]))
      }else if(tr=="CATEGORICAL"){
        miVector <- as.character(unlist(misEtiquetasVariable[,3]))
        names(miVector) <- as.numeric(unlist(misEtiquetasVariable[,2]))
        # Esta asignación es así de chistosa porque no siempre las respuestas tienen orden consecutivo (e.g. 1: Hombre, 3:Mujer)
        subdatosgs[,1] <- factor(x = subdatosgs[,1],levels = as.numeric(unlist(misEtiquetasVariable[,2])),labels = as.character(unlist(misEtiquetasVariable[,3])))
      }else if(tr=="BOOLEAN"){
        # Aquí no recuerdo porqué deje TRUE y FALSE como factores en vez de valores lógicos? Es por frecuentator ?
        subdatosgs[,1] <- as.logical(subdatosgs[,1])
        # subdatosgs[,1] <- factor(x = subdatosgs[,1],levels = c(1,0),labels = c("TRUE","FALSE"))
      }else{
        stop("No me programaron para procesar el tipo de dato ", tr, "( me tienes que editar en gaseosa.R)")
      }
      datosgs[,i] <- subdatosgs
      subdatosgs[,1] <- set_label(subdatosgs[,1],misEtiquetas$Label)
    }
  }
  close(pb)
  return(datosgs)
  ##########################
}


