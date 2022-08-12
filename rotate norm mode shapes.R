
rotate_norm<-function(ssi,colinit,mp,rotate,normalize,colend=dim(ssi)[2])  {
  if (rotate==T) {for (i in 1:dim(ssi)[1]) {ssi[i,colinit:colend]<-ssi[i,colinit:colend]*complex(modulus=1,argument=-mp[i])}}
  if (normalize==T) {for (i in 1:dim(ssi)[1]) {ssi[i,colinit:colend]<-as.complex(ssi[i,colinit:colend])/max(Mod(as.complex(ssi[i,colinit:colend])),na.rm=T)}}
  return(ssi)
}

