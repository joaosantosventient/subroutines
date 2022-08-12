
difmat<-function(ssi,col,divideby) {
  dif<-matrix(NA,nrow=dim(ssi)[1],ncol=dim(ssi)[1])
  dadosdif<-as.numeric(ssi[,col])
  if (divideby==T) {for (kkk in 1:length(dadosdif)) {dif[,kkk]<-abs((dadosdif-dadosdif[kkk]))/pmax(abs(dadosdif),abs(dadosdif[kkk]))}}
  if (divideby==F) {for (kkk in 1:length(dadosdif)) {dif[,kkk]<-abs((dadosdif-dadosdif[kkk]))}}
  return(dif)
}
