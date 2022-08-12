
standardize_real_modes<-function(ssi,acelpositions) {
  
  # for (qq in 1:nrow(ssi)) {
  #   valor<-Re(as.matrix(ssi[qq,acelpositions]))
  #   valor<-valor[which.max(abs(valor))]
  #   if (valor>=0) {argo<-0} else {argo=3.141593}
  #   ssi[qq,acelpositions]<-ssi[qq,acelpositions]*complex(modulus=1,argument=argo)
  # }
  # 
  negativos<-as.numeric(apply(Re(as.matrix(ssi[,acelpositions])),1,function(x) x[which.max(abs(x))]))<0
  ssi[,acelpositions][negativos,]<-(ssi[,acelpositions][negativos,])*complex(modulus=1,argument=3.141593)
  
  return(ssi)
}