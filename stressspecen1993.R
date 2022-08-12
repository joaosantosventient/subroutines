stressspecen1993<-function(classe,stress) {
  
  ciclos<-c()
  for (i in 1:length(stress)) {
    tensao<-stress[i]
    if (tensao<=0.404*classe) {N<-1e50}
    if (tensao>0.404*classe & tensao<=0.737*classe) {N=5e6*(0.737*classe/tensao)^5}
    if (tensao>0.737*classe) {N=2e6*classe^3/tensao^3}
    ciclos[i]<-N
  }
  
  strspecdf<-as.data.frame(cbind(ciclos,tensao=stress))
  
  return(strspecdf)
  
}