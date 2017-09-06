#'Función para crear un syntax de spss desde un datamap generado por soda
#'
#'Esta función crea un syntax de spss a partir de un DataMap de Soda
#'@param sPath El archivo descargado de soda, un "dataMap"
#'@param sFile El nombre del archivo a generar, recordar usar la terminación ".sps"
#'@export
#'@keywords spss

creaSyntaxSPSS<- function(sPath, sFile){
  # sPath <- "~/Descargas/U&A Seguros V2_DataMap_22-02-2017 15_59_05.xlsx"
  # sFile <- "~/Descargas/magic.sps"

  syntax <- NULL
  # Primera parte: El etiquetado de values
  sDatos <- readxl::read_excel(sPath,col_names = T)
  sDatos <- sDatos[!is.na(sDatos$Label),]
  cadena <- sDatos$Label
  cadena <- gsub("<.*?>"," ",cadena)
  cadena <- gsub("\\[.*?\\]"," ",cadena)
  cadena <- strtrim(cadena,200)
  
  lastchar <- substr(sDatos$Variable, nchar(sDatos$Variable), nchar(sDatos$Variable))
  while(sum(lastchar %in% "_")>0){
    cadenas <- sDatos$Variable[lastchar %in% "_"]
    cadenas <- substr(cadenas, 1, nchar(cadenas)-1)
    sDatos$Variable[lastchar %in% "_"] <- cadenas
    lastchar <- substr(sDatos$Variable, nchar(sDatos$Variable), nchar(sDatos$Variable))
  }
  
  sDatos$syntax <- paste0("VARIABLE LABELS ",sDatos$Variable," '",cadena,"'.")
  syntax <- sDatos$syntax
  
  # Segunda parte: El value labels 
  sDatos <- readxl::read_excel(sPath,sheet = "Value Labels",col_names = T)
  sDatos$`Value Label` <- gsub(pattern = '\"',replacement = "",x = sDatos$`Value Label`)
  sDatos$`Value Label` <- gsub(pattern = "\'",replacement = "",x = sDatos$`Value Label`)
  variables <- unique(sDatos$Variable)

  pb <- txtProgressBar(min = 0, max = length(variables), style = 3)
  for(i in 1:length(variables)){
    # i <- 1
    setTxtProgressBar(pb, i)
    variablemini <- variables[i]
    sDatosmini <- sDatos[sDatos$Variable %in% variablemini,]
    sDatosmini$`Value Label` <- strtrim(sDatosmini$`Value Label`,100)
    sDatosmini$syntax <- paste0(sDatosmini$Value," '",sDatosmini$`Value Label`,"' ")
    syntax <- c(syntax,"VALUE LABELS",variablemini,sDatosmini$syntax,".")
  }
  close(pb)
  syntax <- c(syntax,"EXECUTE.")
  write.table(syntax,file = sFile,quote = F,row.names = F,col.names = F,fileEncoding = "utf-8")
  cat(paste("\nSyntax creado en :",sFile))
}
 

