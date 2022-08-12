
rainflow<-function(var,stress_interval) {
  
  # Call functions ----------------------------------------------------------
  library(plyr)
  
  # Remove NA ---------------------------------------------------------------
  var<-var[complete.cases(var)]
  
  # Remove outliers greater than yelding ------------------------------------
  var<-var[which(var<235)]
  
  # Maxmin ------------------------------------------------------------------
  posits<-which(diff(var,lag=1)>0)
  negats<-which(diff(var,lag=1)<0)
  maxs<-intersect(posits+1,negats)
  mins<-intersect(negats+1,posits)
  maxmin_positions<-sort(c(maxs,mins))
  maxmin<-var[maxmin_positions]
  
  # Ciclos e variacoes de tensao -----------------------------------------------------
  tensao<-c(); ciclos<-c()
  num_cycles_removed<-1
  while (num_cycles_removed>0) {
    dif1_0<-diff(maxmin,lag=1)
    dif2_0<-c(diff(maxmin,lag=2),0)
    dif1_1<-c(dif1_0[2:(length(dif1_0))],0)
    dif2_1<-c(dif2_0[2:(length(dif2_0))],0)
    mult1=dif2_0*dif1_1
    mult2=dif1_1*dif2_1
    num_cycles_removed<-length(which(mult1<0 & mult2<0))
    if (num_cycles_removed>0) {
      ciclosbeg<-which(mult1<0 & mult2<0)+1
      ciclosend<-which(mult1<0 & mult2<0)+2
      tensao<-c(tensao,abs(maxmin[ciclosbeg]-maxmin[ciclosend]))
      ciclos<-c(ciclos,rep(1,length(abs(maxmin[ciclosbeg]-maxmin[ciclosend]))))
      maxmin<-maxmin[-c(ciclosbeg,ciclosend)]
    } else {
      tensao<-c(tensao,diff(maxmin))
      ciclos<-c(ciclos,rep(0.5,length(diff(maxmin))))
    }
  }
  ciclos_tensao<-cbind(ciclos=ciclos,tensao=abs(tensao))
  
  # Cnstruir espectro ----------------------------------------------------------------
  ciclos_tensao_classes<-cbind(ciclos=ciclos_tensao[,1],tensao=round_any(ciclos_tensao[,2],accuracy=stress_interval))
  espectro<-matrix(nrow=0,ncol=2)
  for (i in as.numeric(names(table(ciclos_tensao_classes[,2])))) {
    temp<-ciclos_tensao_classes[ciclos_tensao_classes[,2] %in% i,]
    if (is.null(dim(temp))) {
      espectro<-rbind(espectro,temp)
    } else {
      espectro<-rbind(espectro,c(sum(temp[,1],na.rm=T),mean(temp[,2],na.rm=T)))  
    }
  }
  
  # Output ------------------------------------------------------------------
  return(espectro)
  
}

