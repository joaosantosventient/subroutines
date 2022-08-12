
mpdvec<-function(ssi,colinit,colend) {
  mpd<-mp<-c()
  for (k in 1:dim(ssi)[1]) {
    modo<-as.complex(ssi[k,colinit:colend])
    modo<-modo[complete.cases(modo)]
    v<-svd(cbind(Re(modo),Im(modo)))$v
    mp[k]<-atan(-v[1,2]/v[2,2])
      mpdvec<-c()
      for (kk in 1:length(modo)) {mpdvec[kk]<-(Mod(modo[kk])*acos(abs((Re(modo[kk])*v[2,2]-Im(modo[kk])*v[1,2])/(sqrt(v[1,2]^2+v[2,2]^2)*abs(modo[kk])))))}
      mpd[k]<-sum(mpdvec)/length(Mod(modo))
  }
  return(list(mpd=mpd,mp=mp))
}