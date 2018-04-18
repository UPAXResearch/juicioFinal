library(juicioFinal)
#install.packages(c("plyr", "dplyr", "survey", "AlgDesign", "readxl", "multcomp", "mlogit"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pp = readRDS("BaseFactorLimpia.rds")

bandera1 <- c("Total","PLAZA","V3.PerfilesUsuarios","PERFIL","GENERO","P8.CasaEmpeno_MayorUso","NUEVO.RANGO.EDAD","NSE")
pp$Pond<-1
ppe<-subset(pp, Status_campo != "Incidencia" & ajuste == "VALIDO")

perfil.estudio<-list(
  plaza = frecuentator(ppe,"PLAZA",fTponderador='Pond',fbanner = bandera1),
  plazadp = frecuentator(ppe,"PLAZA",fTponderador='Pond',fbanner = bandera1, fTprop = T))

exportator(perfil.estudio, "Resultados/estoesunaprueba.csv", encoding = "Latin1")




############################
reporteFINAL <- data.frame()
reporteF = perfil.estudio
finali = 2


elTemporal <- reporteF[[finali]]
elTemporal <- rbind(names(elTemporal),elTemporal)
names(elTemporal) <- paste(LETTERS[1:length(elTemporal)],1:length(elTemporal),sep="")
elTemporal[,1] = as.character(elTemporal[,1])
elTemporal[1,1] <- names(reporteF)[finali]
salto <- elTemporal[0,]
elTemporal <- rbind(elTemporal,NA)
reporteFINAL <- plyr::rbind.fill(reporteFINAL,elTemporal)


for(finali in 1:length(reporteF)){
  elTemporal <- reporteF[[finali]]
  elTemporal <- rbind(names(elTemporal),elTemporal)
  names(elTemporal) <- paste(LETTERS[1:length(elTemporal)],1:length(elTemporal),sep="")
  elTemporal[1,1] <- names(reporteF)[finali]
  salto <- elTemporal[0,]
  elTemporal <- rbind(elTemporal,NA)
  reporteFINAL <- plyr::rbind.fill(reporteFINAL,elTemporal)
}
