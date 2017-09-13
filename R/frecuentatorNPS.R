
frecuentatorNPS<-function(fTtabla, fTvariables, fTlevels = T, fbanner = NULL, 
                          fTponderador = NULL, fTsobreQuien = NULL, fTtotal = T, fTprop = F, 
                          fTusarNA = F, fTdecimales = 1){
tabla<-frecuentator(fTtabla, fTvariables, fTlevels = T, fbanner = NULL, 
                   fTponderador = NULL, fTsobreQuien = NULL, fTtotal = T, fTprop = F, 
                   fTusarNA = F, fTdecimales = 1)
categorias<-c("1","2","3","4","5","6","7","8","9","10","Total", "1a6", "7a8","9a10","NPS")
tabla<- round(tabla[c(1:11),c(2:length(tabla))])
for (k in 1:(length(tabla) )) {
  tabla[12,k]<-sum(tabla[1:6,k])
  tabla[13,k]<-sum(tabla[7:8,k])
  tabla[14,k]<-sum(tabla[9:10,k])
}

for (i in seq(2,length(tabla),2))  {
  if(!all(is.na(tabla[12:14,i]))){
    if (sum(tabla[12:14,i]) > 100) { tabla[13,i]= (tabla[13,i]-(sum(tabla[12:14,i])-100))
    } else
    { if (sum(tabla[12:14,i]) < 100)   { tabla[13,i]= (tabla[13,i]-(sum(tabla[12:14,i])-100))}
      else{ tabla[13,i]== tabla[13,i]
      }
    }
  }
}
for (k in 1:(length(tabla) )) {
  tabla[15,k]<- tabla[14,k]-tabla[12,k]
}
rownames(tabla)<-categorias  
tabla<-cbind(categorias,tabla)
return(tabla)
}
