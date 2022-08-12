
mpcvec<-function(ssi,colinit,colend) {
  mpc<-c()
  for (k in 1:dim(ssi)[1]) {
    modo<-as.complex(ssi[k,colinit:colend])
    modo<-modo[complete.cases(modo)]
    modo<-modo-mean(modo,na.rm=T)
    empc<-(sum(Im(modo)^2)-sum(Re(modo)^2))/(2*sum(Re(modo)*Im(modo)))
    tetampc<-atan(abs(empc)+sign(empc)*sqrt(1+empc^2))
    mpc[k]<-(sum(Re(modo)^2)+1/empc*sum(Re(modo)*Im(modo))*(2*(empc^2+1)*(sin(tetampc))^2-1))/(sum(Im(modo)^2)+sum(Re(modo)^2))
  }
  return(mpc)
}