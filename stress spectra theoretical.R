teospectra<-function(espectro_list,dados,details) {
  
  # LOOP OVER DETAILS -------------------------------------------------------
  for (j in 1:nrow(details)) {
    
    # Moments and axial force -------------------------------------------------
    dadosdet<-as.data.frame(matrix(NA,nrow=nrow(dados),ncol=3))
    names(dadosdet)<-c("Mx","My","n")
    dadosdet$Mx<-dados$MxA+(dados$MxB-dados$MxA)/(hB-hA)*(details[j,"H"]*1e-3-hA)
    dadosdet$My<-dados$MyA+(dados$MyB-dados$MyA)/(hB-hA)*(details[j,"H"]*1e-3-hA)
    dadosdet$n<-dados$nA+(dados$nB-dados$nA)/(hB-hA)*(details[j,"H"]*1e-3-hA)
    
    # Stresses ----------------------------------------------------------------
    I<-(0.25*pi*((details[j,"D"]*1e-3/2+details[j,"t"]*1e-3)^4-(details[j,"D"]*1e-3/2)^4))
    A<-pi*((details[j,"D"]*1e-3/2+details[j,"t"]*1e-3)^2-(details[j,"D"]*1e-3/2)^2)
    mx<-dadosdet$Mx/I*(details[j,"D"]*1e-3/2+details[j,"t"]*1e-3/2)
    my<-dadosdet$My/I*(details[j,"D"]*1e-3/2+details[j,"t"]*1e-3/2)
    dadosdet$S1<-(mx*(-1)+my*(0)+dadosdet$n/A)*1e-3
    dadosdet$S2<-(mx*(-0.707)+my*(0.707)+dadosdet$n/A)*1e-3
    dadosdet$S3<-(mx*(0)+my*(1)+dadosdet$n/A)*1e-3
    dadosdet$S4<-(mx*(0.707)+my*(0.707)+dadosdet$n/A)*1e-3
    dadosdet$S5<-(mx*(1)+my*(0)+dadosdet$n/A)*1e-3
    dadosdet$S6<-(mx*(0.707)+my*(-0.707)+dadosdet$n/A)*1e-3
    dadosdet$S7<-(mx*(0)+my*(-1)+dadosdet$n/A)*1e-3
    dadosdet$S8<-(mx*(-0.707)+my*(-0.707)+dadosdet$n/A)*1e-3
    
    # Spectra -----------------------------------------------------------------
    stress_posicoes<-which(substr(names(dadosdet),1,1) %in% stress_prefix)
    templist<-list()
    iaux<-1
    for (i in stress_posicoes) {
      templist[[iaux]]<-rainflow(dadosdet[,i],5)
      names(templist)[iaux]<-names(dadosdet)[i]
      iaux<-iaux+1
    }
    espectro_list[[details$Name[j]]]<-templist
    
    # Print Progress ----------------------------------------------------------
    print(details$Name[j])
    
  }
  
  # OUTPUT ------------------------------------------------------------------
  return(espectro_list)
  
}
