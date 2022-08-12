

divide_and_window<-function(dadosmodal) {
  library(signal)
  df<-diff(as.numeric(row.names(dadosmodal)))
  interrups<-which(df>median(df)*1.1)
  lista_sinais<-list()
  for (ii in 1:length(interrups)) {if (ii==1) {lista_sinais[[ii]]<-1:(interrups[ii]-1)} else if (ii==length(interrups)) {lista_sinais[[ii+1]]<-interrups[ii]:(dim(dadosmodal)[1])} else {lista_sinais[[ii]]<-interrups[ii-1]:(interrups[ii]-1)}}
  lista_sinais<-lista_sinais[!sapply(lista_sinais,is.null)]
  for (jj in 1:dim(dadosmodal)[2]) {for (ii in 1:length(lista_sinais)) {dadosmodal[lista_sinais[[ii]],jj]<-dadosmodal[lista_sinais[[ii]],jj]*hanning(length(lista_sinais[[ii]]))}}
  rm(lista_sinais,interrups)
  return(dadosmodal)
}
dadosmodal<-divide_and_window(dadosmodal)