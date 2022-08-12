
# classe<-50
# stress_values<-seq(1,120,1)


stressspecen1993<-function(classe,stress) {
  
  ciclos<-c()
  for (i in 1:length(stress)) {
    tensao<-stress[i]
    if (tensao<=0.404*classe) {N<-1e50}
    if (tensao>0.404*classe & tensao<=0.737*classe) {N=5e6*(0.737*classe/tensao)^5}
    if (tensao>0.737*classe) {N=2e6*classe^3/tensao^3}
    ciclos[i]<-N
  }
  
  strspecdf<-as.data.frame(cbind(ciclos,stress))
  
  return(strspecdf)

}



# 
# cycle_values<-seq(1,1e10,1e5)
# S_values<-c()
# for (i in 1:length(cycle_values)) {
#   N<-cycle_values[i]
#   if (N>=0 & N<=5e6) {tensao<-((2e6/N)^(1/3))*classe}
#   if (N>5e6 & N<=1e8) {tensao<-((5e6/N)^(1/5))*((2/5)^(1/3))*classe}
#   if (N>1e8) {tensao<-((5/100)^(1/5))*((2/5)^(1/3))*classe}
#   S_values[i]<-tensao
#   print(i)
# }
