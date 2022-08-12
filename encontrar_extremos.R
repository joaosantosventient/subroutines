

encontrar_extremos<-function(sinal,fator_decim=1) {
  posits<-which(diff(sinal,lag=1)>0)
  negats<-which(diff(sinal,lag=1)<0)
  if (fator_decim>1) {
    maxs<-intersect(posits+1,negats)*fator_decim+1
    mins<-intersect(negats+1,posits)*fator_decim+1   
  } else if (fator_decim==1) {
    maxs<-intersect(posits+1,negats)
    mins<-intersect(negats+1,posits)    
  }
  valores_maxs<-sinal[maxs]
  valores_mins<-sinal[mins]
  return(list(maxs=maxs,mins=mins,valores_maxs=valores_maxs,valores_mins=valores_mins))
}

