
random_decrement<-function(dados,janela_tempo,data_rate) {
  janela<-janela_tempo*data_rate
  dadosrd<-array(NA, dim=c(janela,dim(dados)[2],dim(dados)[2]))
  for (k in 1:dim(dados)[2]) {
    a<-(2)^0.5*(sd(dados[,k],na.rm=TRUE))
    sups_anteriores<-which(dados[,k]>a)+1
    passagens<-sups_anteriores[which(dados[sups_anteriores,k]<a)]
    infs_anteriores<-which(dados[,k]<a)-1
    passagens<-c(passagens,infs_anteriores[which(dados[infs_anteriores,k]>a)]+2)
    passagens<-passagens[which(passagens+janela<dim(dados)[1])]
    for (kk in 1:dim(dados)[2]) {
      for (m in 1:length(passagens)) {
        if (m==1) {
          soma<-dados[passagens[m]:(passagens[m]+janela-1),kk]
        } else {
          soma<-soma+dados[passagens[m]:(passagens[m]+janela-1),kk]
        }
      }
      dadosrd[,k,kk]<-soma/length(passagens)
    }
  } 
  for (k in 1:dim(dadosrd)[2]) {
    for (kk in 1:dim(dadosrd)[3]) {
      dadosrd[,k,kk]<-dadosrd[,k,kk]-mean(dadosrd[,k,kk],na.rm=TRUE) 
    }
  }
  return(dadosrd)
}