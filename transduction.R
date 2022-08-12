
transduction<-function(dados,statistics_output,datahora,structure,rms_acel=T) {
  

  # Fucntions ---------------------------------------------------------------
  source("//de-noe-comp1/shm/routines/subroutines/which quantile.R")
  
  #  25 ABRIL------------------------------------------------------------------------
  if (structure=="25 abril") {
    
    #*** Acceleration ------------------------------------------------------------
    sensores_a<-which(substr(names(dados),0,1)=="a"); 
    if (!!length(sensores_a)) {
      if (!is.na(as.numeric(datahora))) {if (datahora>=as.POSIXlt("2014082304",format="%Y%m%d%H")) {dados[,sensores_a]<-dados[,sensores_a]*50}}
      #****** Center and square -------------------------------------------------------------------
      dados[,sensores_a]<-scale(dados[,sensores_a],center=apply(dados[,sensores_a],2,median,na.rm=TRUE),scale=FALSE)
      if (rms_acel==T) {dados[,sensores_a]<-dados[,sensores_a]^2}
    }
    
    #*** Temperature ------------------------------------------------------------
    sensores_T<-which(substr(names(dados),0,1)=="T")
    if (!is.na(as.numeric(datahora))) {if (!!length(sensores_T)) {if (datahora>=as.POSIXlt("2015030415",format="%Y%m%d%H")) {dados[,sensores_T]<-31.67-3.73*dados[,sensores_T]}}}
    
    #*** Strain_Stress ------------------------------------------------------------
    sensores_e<-which(substr(names(dados),0,1)=="e")
    sensores_ef<-which(substr(names(dados),0,2)=="ef")
    sensores_e<-setdiff(sensores_e,sensores_ef)
    #****** sensores_e ---------------------------------------------------------
    if (!!length(sensores_e)) {
      dados[,sensores_e]<-dados[,sensores_e]*498.2; dados[,sensores_e]<-dados[,sensores_e]*0.21
      #********* section 66N -----------------------------------------------------------------
      if (datahora<=as.POSIXlt("2014082304",format="%Y%m%d%H")) {
        if("e1.66N" %in% names(dados)) {dados[,"e1.66N"]<-dados[,"e1.66N"]/498.2-1}
        if("e2.66N" %in% names(dados)) {dados[,"e2.66N"]<-dados[,"e2.66N"]/498.2-1}
        if("e3.66N" %in% names(dados)) {dados[,"e3.66N"]<-dados[,"e3.66N"]/498.2-1}
        if("e4.66N" %in% names(dados)) {dados[,"e4.66N"]<-dados[,"e4.66N"]/498.2-1}
        if("e5.66N" %in% names(dados)) {dados[,"e5.66N"]<-dados[,"e5.66N"]/498.2-1}
        if("e6.66N" %in% names(dados)) {dados[,"e6.66N"]<-dados[,"e6.66N"]/498.2-1}
        if("e7.66N" %in% names(dados)) {dados[,"e7.66N"]<-dados[,"e7.66N"]/498.2-1}
        if("e8.66N" %in% names(dados)) {dados[,"e8.66N"]<-dados[,"e8.66N"]/498.2-1}
      }
      #********* section P3mA -----------------------------------------------------------------
      if (datahora>=as.POSIXlt("2019112016",format="%Y%m%d%H") & datahora<=as.POSIXlt("2021052016",format="%Y%m%d%H")) {
        if("e1.P3mA" %in% names(dados)) {dados[,"e1.P3mA"]<-dados[,"e1.P3mA"]*(-1)}
        if("e2.P3mA" %in% names(dados)) {dados[,"e2.P3mA"]<-dados[,"e2.P3mA"]*(-1)}
        if("e3.P3mA" %in% names(dados)) {dados[,"e3.P3mA"]<-dados[,"e3.P3mA"]*(-1)}
        if("e4.P3mA" %in% names(dados)) {dados[,"e4.P3mA"]<-dados[,"e4.P3mA"]*(-1)}
      } else if (datahora>as.POSIXlt("2021052016",format="%Y%m%d%H")) {
        if("e1.P3mA" %in% names(dados)) {dados[,"e1.P3mA"]<-dados[,"e1.P3mA"]/966.2}
        if("e2.P3mA" %in% names(dados)) {dados[,"e2.P3mA"]<-dados[,"e2.P3mA"]/966.2}
        if("e3.P3mA" %in% names(dados)) {dados[,"e3.P3mA"]<-dados[,"e3.P3mA"]/966.2}
        if("e4.P3mA" %in% names(dados)) {dados[,"e4.P3mA"]<-dados[,"e4.P3mA"]/966.2}
      }
      #********* section P3mB -----------------------------------------------------------------
      if (datahora>=as.POSIXlt("2019112016",format="%Y%m%d%H")) {
        if("e1.P3mB" %in% names(dados)) {dados[,"e1.P3mB"]<-dados[,"e1.P3mB"]*(-1)}
        if("e2.P3mB" %in% names(dados)) {dados[,"e2.P3mB"]<-dados[,"e2.P3mB"]*(-1)}
        if("e3.P3mB" %in% names(dados)) {dados[,"e3.P3mB"]<-dados[,"e3.P3mB"]*(-1)}
        if("e4.P3mB" %in% names(dados)) {dados[,"e4.P3mB"]<-dados[,"e4.P3mB"]*(-1)}
      }
      #********* section P3jB -----------------------------------------------------------------
      if (datahora>=as.POSIXlt("2019112016",format="%Y%m%d%H")) {
        if("e1.P3jB" %in% names(dados)) {dados[,"e1.P3jB"]<-dados[,"e1.P3jB"]*(-1)}
        if("e2.P3jB" %in% names(dados)) {dados[,"e2.P3jB"]<-dados[,"e2.P3jB"]*(-1)}
        if("e3.P3jB" %in% names(dados)) {dados[,"e3.P3jB"]<-dados[,"e3.P3jB"]*(-1)}
        if("e4.P3jB" %in% names(dados)) {dados[,"e4.P3jB"]<-dados[,"e4.P3jB"]*(-1)}
      }
    }
    #****** sensores_ef ---------------------------------------------------------
    if (!!length(sensores_ef)) {dados[,sensores_ef]<-dados[,sensores_ef]*0.21}
    #*** Displacement ------------------------------------------------------------
    sensores_d<-which(substr(names(dados),0,2)=="dm" | substr(names(dados),0,2)=="dj")
    if (!!length(sensores_d)) {dados[,sensores_d]<-(-375)+93.75*dados[,sensores_d]}
    
    #*** Displacement  ------------------------------------------------------------
    sensores_dd<-which(substr(names(dados),0,1)=="d" & substr(names(dados),0,8)!="datahora")
    sensores_dd<-sensores_dd[!sensores_dd %in% sensores_d]
    if (!!length(sensores_dd)) {dados[,sensores_dd]<-15*dados[,sensores_dd]}
    
    #*** Tiltmeters------------------------------------------------------------
    sensores_c<-which(substr(names(dados),0,1)=="c")
    if (!!length(sensores_c)) {
      dados[,sensores_c]<-2160*dados[,sensores_c]
      if (datahora>=as.POSIXlt("2019112016",format="%Y%m%d%H")) {if("cl.P3jD" %in% names(dados)) {dados[,"cl.P3jD"]<-dados[,"cl.P3jD"]*(-1)}}
    }
    
    #*** Anemometers ------------------------------------------------------------
    sensores_w<-which(substr(names(dados),0,1)=="w")
    if (!!length(sensores_w)) {
      dados[,sensores_w]<-20*dados[,sensores_w]
    }
    
    #*** Railway pads ------------------------------------------------------------
    sensores_p<-which(substr(names(dados),0,1)=="p");
    if (!!length(sensores_p)) {
      dadostemp<-dados[,sensores_p]
      for (ii in 1:dim(dadostemp)[2]) {dadostemp[complete.cases(dadostemp[,ii]),ii]<-abs(runmed(dadostemp[complete.cases(dadostemp[,ii]),ii],60*samplerate)-dadostemp[complete.cases(dadostemp[,ii]),ii])}
      if("pm.P1" %in% names(dadostemp)) {dadostemp[,"pm.P1"]<-dadostemp[,"pm.P1"]+0.195}; if("pme.P1" %in% names(dadostemp)) {dadostemp[,"pme.P1"]<-dadostemp[,"pme.P1"]+0.195};
      if("pj.P1" %in% names(dadostemp)) {dadostemp[,"pj.P1"]<-dadostemp[,"pj.P1"]+0.116}; if("pje.P1" %in% names(dadostemp)) {dadostemp[,"pje.P1"]<-dadostemp[,"pje.P1"]+0.116};
      if("pmi.P1" %in% names(dadostemp)) {dadostemp[,"pmi.P1"]<-dadostemp[,"pmi.P1"]+0.0394};
      if("pji.P1" %in% names(dadostemp)) {dadostemp[,"pji.P1"]<-dadostemp[,"pji.P1"]+0.0672};
      if("pm.P7" %in% names(dadostemp)) {dadostemp[,"pm.P7"]<-dadostemp[,"pm.P7"]+0.01};
      if("pj.P7" %in% names(dadostemp)) {dadostemp[,"pj.P7"]<-dadostemp[,"pj.P7"]+0.002};
      for (ii in 1:dim(dadostemp)[2]) {dadostemp[,ii]<-(-8.546)*log(dadostemp[,ii])+269.864*sqrt(dadostemp[,ii])-56.403}
      if("pm.P1" %in% names(dadostemp)) {dadostemp[,"pm.P1"]<-dadostemp[,"pm.P1"]-76.74}; if("pme.P1" %in% names(dadostemp)) {dadostemp[,"pme.P1"]<-dadostemp[,"pme.P1"]-76.74};
      if("pj.P1" %in% names(dadostemp)) {dadostemp[,"pj.P1"]<-dadostemp[,"pj.P1"]-53.92}; if("pje.P1" %in% names(dadostemp)) {dadostemp[,"pje.P1"]<-dadostemp[,"pje.P1"]-53.92};
      if("pmi.P1" %in% names(dadostemp)) {dadostemp[,"pmi.P1"]<-dadostemp[,"pmi.P1"]-24.80};
      if("pji.P1" %in% names(dadostemp)) {dadostemp[,"pji.P1"]<-dadostemp[,"pji.P1"]-36.63};
      if("pm.P7" %in% names(dadostemp)) {dadostemp[,"pm.P7"]<-dadostemp[,"pm.P7"]-9.94};
      if("pj.P7" %in% names(dadostemp)) {dadostemp[,"pj.P7"]<-dadostemp[,"pj.P7"]-8.78};
      dados[,sensores_p]<-dadostemp
      if (!is.na(as.numeric(datahora))) {if (datahora<as.POSIXlt("2015121016",format="%Y%m%d%H")) {if(c("pm.P7","pj.P7") %in% names(dadostemp)) {vartempor<-dados[,c("pm.P7")]; dados[,c("pm.P7")]<-dados[,c("pj.P7")]; dados[,c("pj.P7")]<-vartempor}}}
    }
    
    # Output type --------------------------------------------------------------------------
    if (statistics_output==T) {
      
      #*** Obtain statistical quantities ----------------------------------------------------------------------
      dadosrawvento<-dados
      dados<-as.matrix(rbind(apply(dados[2:ncol(dados)],2,quantile,probs=c(0.25,0.5,0.75),na.rm=T),
                             apply(dados[2:ncol(dados)],2,min,na.rm=TRUE),
                             apply(dados[2:ncol(dados)],2,max,na.rm=TRUE)))
      dados<-cbind(datahora,as.data.frame(dados),idinfo=c("q25","median","q75","min","max"))
      dados$idinfo<-as.character(dados$idinfo)
      if (rms_acel==T) {dados[,sensores_a]<-sqrt(dados[,sensores_a])}
      # 
      #*** Obtain statistical wind quantities ----------------------------------------------------------------
      if (!!length(sensores_w)) {
        dadosvento<-dadosrawvento[complete.cases(dadosrawvento[,sensores_w]),sensores_w]
        sensores_w0<-which(substr(names(dadosvento),4,5)=="0" & substr(names(dadosvento),0,1)=="w")
        sensores_w22<-which(substr(names(dadosvento),4,7)=="22S" & substr(names(dadosvento),0,1)=="w")
        if (!!length(sensores_w0)) {
          srss<-rowSums(dadosvento[,names(dadosvento)[sensores_w0]]^2)
          wind0<-c(sort(sqrt(srss),na.last=NA)[floor(dim(dadosvento)[1]/4)],
                   sort(sqrt(srss),na.last=NA)[floor(dim(dadosvento)[1]/2)],
                   sort(sqrt(srss),na.last=NA)[floor(dim(dadosvento)[1]*3/4)],
                   min(sqrt(srss),na.rm=T),
                   max(sqrt(srss),na.rm=T))
          whichwind0<-c(); for (jj in 1:length(wind0)) {whichwind0[jj]<-which(wind0[jj]==sqrt(srss))}
          wind0<-dadosvento[whichwind0,names(dadosvento)[sensores_w0]]
        }
        if (!!length(sensores_w22)) {
          srss<-rowSums(dadosvento[,names(dadosvento)[sensores_w22]]^2)
          wind22<-c(sort(sqrt(srss),na.last=NA)[floor(dim(dadosvento)[1]/4)],
                    sort(sqrt(srss),na.last=NA)[floor(dim(dadosvento)[1]/2)],
                    sort(sqrt(srss),na.last=NA)[floor(dim(dadosvento)[1]*3/4)],
                    min(sqrt(srss),na.rm=T),
                    max(sqrt(srss),na.rm=T))
          whichwind22<-c(); for (jj in 1:length(wind22)) {whichwind22[jj]<-which(wind22[jj]==sqrt(srss))}
          wind22<-dadosvento[whichwind22,names(dadosvento)[sensores_w22]]
        }
        rm(dadosrawvento)
        if (!!length(sensores_w)) {if (!!length(sensores_w0)) {dados[,c("wX.0","wY.0","wZ.0")]<-wind0}}
        if (!!length(sensores_w)) {if (!!length(sensores_w22)) {dados[,c("wX.22S","wY.22S","wZ.22S")]<-wind22}}
      }
      
    }
  }
  
  #  SADO------------------------------------------------------------------------
  if (structure=="sado") {
    
    #*** Acceleration -------------------------------------------------------
    sensores_a<-which(substr(names(dados),0,1)=="a") 
    if (!!length(sensores_a)) {
      #****** Piezotronics 1V/g ---------------------------------------------
      sensores_a_PCB<-sensores_a[is.element(names(dados)[sensores_a],c("al.p1","al.p2","al.p3","al.p4"))] # quais dos acelerometros sao PCB
      dados[,sensores_a_PCB]<-dados[,sensores_a_PCB]*1000
      #****** Episensor 0.5g, 20V/g -----------------------------------------
      sensores_a_EPI<-sensores_a[!is.element(names(dados)[sensores_a],c("al.p1","al.p2","al.p3","al.p4"))] # quais dos acelerometros nao sao PCB, sao episensor
      dados[,sensores_a_EPI]< dados[,sensores_a_EPI]*50 # episensors a 0.5g
      if ("avsm.t2m" %in% names(dados)) {dados[,"avsm.t2m"]<-dados[,"avsm.t2m"]*4} # o acelerometro superior esta a 2g
      #****** Center and square -----------------------------------------------
      dados[,sensores_a]<-scale(dados[,sensores_a],center=apply(as.data.frame(dados[,sensores_a]),2,median,na.rm=TRUE),scale=FALSE)
      #dados[,sensores_a]<-scale(dados[,sensores_a],center=apply(dados[,sensores_a],2,median,na.rm=TRUE),scale=FALSE)
      if (rms_acel==T) {dados[,sensores_a]<-dados[,sensores_a]^2}
    }
    #*** Temperatures --------------------------------------------------------
    sensores_T<-names(dados)[which(substr(names(dados),0,1)=="T")]
    sensores_T<-sensores_T[which(substr(sensores_T,5,5)=="a")] #filtra so os arcos.
    sensores_T<-names(dados)[which(is.element(names(dados),sensores_T))]
    if (!!length(sensores_T)) {dados[,sensores_T]<-142.985-0.179571*(dados[,sensores_T])^1+0.000153804*(dados[,sensores_T])^2-7.77949E-8*(dados[,sensores_T])^3+1.98156E-11*(dados[,sensores_T])^4-2.00646E-15*(dados[,sensores_T])^5}
    #*** Stress --------------------------------------------------------------
    sensores_e<-names(dados)[which(substr(names(dados),0,1)=="e")]
    if (!!length(sensores_e)) {
      #****** Steel Stress ---------------------------------------
      sensores_ess<-names(dados)[which(substr(names(dados),0,2)=="es" | substr(names(dados),0,2)=="ei")]
      sensores_est<-sensores_ess[which(substr(sensores_ess,4,5)==".t")]
      sensores_esa<-sensores_ess[which(substr(sensores_ess,4,5)==".a")]
      sensores_est<-names(dados)[which(is.element(names(dados),sensores_est))]
      sensores_esa<-names(dados)[which(is.element(names(dados),sensores_esa))]
      if (!is.na(as.numeric(datahora))) {
        if(!!length(sensores_est)){
          if (datahora<as.POSIXlt("2019071001",format="%Y%m%d%H")) {
            dados[,sensores_est]<-9.6618e2*dados[,sensores_est]
          } else {
            dados[,sensores_est]<-1243.07*dados[,sensores_est]
          }
          dados[,sensores_est]<-dados[,sensores_est]*0.21
        }
        if(!!length(sensores_esa)){
          dados[,sensores_esa]<-9.6618e2*dados[,sensores_esa]
          dados[,sensores_esa]<-dados[,sensores_esa]*0.21
        }
      }
      #****** Vibrating wire based stress -------------------------------------
      sensores_evw<-names(dados)[which(substr(names(dados),0,2)=="ee" | substr(names(dados),0,2)=="er" | substr(names(dados),0,2)=="ef")]
      sensores_evw<-names(dados)[which(is.element(names(dados),sensores_evw))]
      if(!!length(sensores_evw)){
        dados[,sensores_evw]<-3.025e-3*(dados[,sensores_evw])^2
        dados[,sensores_evw]<-0.04*(dados[,sensores_evw])
      }
      sensores_erf<-names(dados)[which(substr(names(dados),0,2)=="er" | substr(names(dados),0,2)=="ef")]
      if (datahora<as.POSIXlt("2019120917",format="%Y%m%d%H")) {if(!!length(sensores_erf)) {dados[,sensores_erf]<-(dados[,sensores_erf])/0.04}}
    }
    #*** Horizontal displacement ----------------------------------------------
    sensores_dh<-names(dados)[which(substr(names(dados),0,2)=="dh")]
    sensores_dh<-names(dados)[which(is.element(names(dados),sensores_dh))]
    if (!!length(sensores_dh)) {dados[,sensores_dh]<-20*dados[,sensores_dh]}
    #*** Vertical displacement -----------------------------------
    sensores_dv<-names(dados)[which(substr(names(dados),0,2)=="dv")]
    sensores_dv<-names(dados)[which(is.element(names(dados),sensores_dv))]
    if (!!length(sensores_dv)) {dados[,sensores_dv]<-(-2.5e2)+6.25e1*(dados[,sensores_dv])}
    if ("dvj.p2" %in% sensores_dv) {
      if (datahora<as.POSIXlt("2019120917",format="%Y%m%d%H")) {
        for (ijk in 1:length(sensores_dv)) {
          if (sensores_dv[ijk]!="dvj.p2") {
            dados[,sensores_dv[ijk]]<-dados[,sensores_dv[ijk]]-dados$dvj.p2
          }
        }
      }
    }
    #*** Tiltmeters -------------------------------------------
    sensores_cl<-names(dados)[which(substr(names(dados),0,2)=="cl")]
    sensores_cl<-names(dados)[which(is.element(names(dados),sensores_cl))]
    if (!!length(sensores_cl)) {dados[,sensores_cl]<-2160*dados[,sensores_cl]}
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {
      dados<-as.matrix(rbind(apply(dados[2:ncol(dados)],2,quantile,probs=c(0.25,0.5,0.75),na.rm=T),
                             apply(dados[2:ncol(dados)],2,min,na.rm=T),
                             apply(dados[2:ncol(dados)],2,max,na.rm=T)))
      dados<-cbind(datahora,as.data.frame(dados),idinfo=c("q25","median","q75","min","max"))
      dados$idinfo<-as.character(dados$idinfo)
      if (rms_acel==T) {dados[,sensores_a]<-sqrt(dados[,sensores_a])}
    }
  }
  
  #  CARTAXO ------------------------------------------------------------------------
  if (structure=="cartaxo") {
    #*** Acceleration ------------------------------------------------------------
    sensores_a<-which(substr(names(dados),0,2)=="AC"); 
    if (!!length(sensores_a)) {
      dados[,sensores_a]<-dados[,sensores_a]*0.025 
      #****** Center and square -------------------------------------------------------------------
      dados[,sensores_a]<-scale(dados[,sensores_a],center=apply(dados[,sensores_a],2,median,na.rm=T),scale=F)
      if (rms_acel==T) {dados[,sensores_a]<-dados[,sensores_a]^2}
    }
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {
      dados<-as.matrix(rbind(apply(dados[2:ncol(dados)],2,quantile,probs=c(0.25,0.5,0.75),na.rm=T),
                             apply(dados[2:ncol(dados)],2,min,na.rm=T),
                             apply(dados[2:ncol(dados)],2,max,na.rm=T)))
      dados<-cbind(datahora,as.data.frame(dados),idinfo=c("q25","median","q75","min","max"))
      dados$idinfo<-as.character(dados$idinfo)
      if (rms_acel==T) {dados[,sensores_a]<-sqrt(dados[,sensores_a])}
    }
    
    
  }
  
  #  PIRAMIDES ------------------------------------------------------------------------
  if (structure=="piramides") {
    #*** Acceleration ------------------------------------------------------------
    sensores_a<-which(substr(names(dados),0,1)=="a")
    if (!!length(sensores_a)) {
      dados[,sensores_a]  <-  dados[,sensores_a]*((6/7.2)*1000)    #6g -> 7.2V
      #****** Center and square -------------------------------------------------------------------
      dados[,sensores_a]<-scale(dados[,sensores_a],center=apply(dados[,sensores_a],2,median,na.rm=T),scale=F)
    }
    #*** Horizontal displacement ------------------------------------------------------------
    sensores_dh<-which(substr(names(dados),0,2)=="dh")
    if (!!length(sensores_dh))  {dados[,sensores_dh] <-  (dados[,sensores_dh]-0.4)*125}        #[0.4-2]V -> [0-200]mm
    
    #*** Vertical displacement ------------------------------------------------------------
    sensores_dv<-which(substr(names(dados),0,2)=="dv")
    if (!!length(sensores_dv))  {dados[,sensores_dv] <-  (dados[,sensores_dv]-0.4)*637.50}     #[0.4-2]V -> [0-1020]mm
    #calculo dos deslocamentos dos niveis liquidos em relacao a celula de referencia
    if(all(c("dv.1m","dv.r.P115")  %in% names(dados))){dados[,"dv.1m"] <-dados[,"dv.1m"] - dados[,"dv.r.P115"]}
    if(all(c("dv.2m","dv.r.P115")  %in% names(dados))){dados[,"dv.2m"] <-dados[,"dv.2m"] - dados[,"dv.r.P115"]}
    if(all(c("dv.3m","dv.r.P115")  %in% names(dados))){dados[,"dv.3m"] <-dados[,"dv.3m"] - dados[,"dv.r.P115"]}
    if(all(c("dv.4m","dv.r.P115")  %in% names(dados))){dados[,"dv.4m"] <-dados[,"dv.4m"] - dados[,"dv.r.P115"]}
    if(all(c("dv.5m","dv.r.P115")  %in% names(dados))){dados[,"dv.5m"] <-dados[,"dv.5m"] - dados[,"dv.r.P115"]}
    if(all(c("dv.6m","dv.r.P118")  %in% names(dados))){dados[,"dv.6m"] <-dados[,"dv.6m"] - dados[,"dv.r.P118"]}
    if(all(c("dv.7m","dv.r.P118")  %in% names(dados))){dados[,"dv.7m"] <-dados[,"dv.7m"] - dados[,"dv.r.P118"]}
    if(all(c("dv.8m","dv.r.P118")  %in% names(dados))){dados[,"dv.8m"] <-dados[,"dv.8m"] - dados[,"dv.r.P118"]}
    if(all(c("dv.9m","dv.r.P118")  %in% names(dados))){dados[,"dv.9m"] <-dados[,"dv.9m"] - dados[,"dv.r.P118"]}
    if(all(c("dv.10m","dv.r.P118") %in% names(dados))){dados[,"dv.10m"]<-dados[,"dv.10m"]- dados[,"dv.r.P118"]}
    
    #*** Rotation ------------------------------------------------------------
    sensores_cl<-which(substr(names(dados),0,2)=="cl")
    if (!!length(sensores_cl))  {dados[,sensores_cl] <-  dados[,sensores_cl]*(10800/5)}        #[-5,+5]V -> [-10800-10800]"sex
    
    #*** Relative humidity ------------------------------------------------------------
    sensores_hr<-which(substr(names(dados),0,2)=="hr")
    if (!!length(sensores_hr))  {dados[,sensores_hr] <-  (dados[,sensores_hr]-0.4)*62.5}       #[0.4-2]V -> [0-200]mm
    
    #*** Air temperature ------------------------------------------------------------
    sensores_tar<-which(substr(names(dados),0,4)=="T.ar")
    if (!!length(sensores_tar)) {dados[,sensores_tar] <- ((dados[,sensores_tar]-0.4)*62.5)-30} #[0.4-2]V -> [0-200]mm
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {}
    
  }
  
  #  FIGUEIRA ------------------------------------------------------------------------
  if (structure=="figueira") {
    
    #*** Strain correction from full bridge - Extension from bending = strain lower flange + flange upper flange
    #the full bridge scheme returns the average strain from one flange
    sensores_E<-which(substr(names(dados),0,1)=="E")
    if (!!length(sensores_E)) {
      dados[,sensores_E] <- dados[,sensores_E]*2
    }
    
    #*** Wind Direction ------------------------------------------------------------
    sensores_wd<-which(substr(names(dados),0,2)=="WD")
    if (!!length(sensores_wd)) {
      
      dadosrawvento <- dados[,sensores_wd]
      dadosrawvento <- dadosrawvento[complete.cases(dadosrawvento)]
      dadosrawvento <- dadosrawvento[dadosrawvento>=0 & dadosrawvento<=360]
    }
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {
      dados_aux<-as.matrix(rbind(apply(dados[2:ncol(dados)],2,quantile,probs=c(0.25,0.5,0.75),na.rm=T),
                             apply(dados[2:ncol(dados)],2,min,na.rm=T),
                             apply(dados[2:ncol(dados)],2,max,na.rm=T)))
      dados_aux<-cbind(datahora,as.data.frame(dados_aux),idinfo=c("q25","median","q75","min","max"))
      
      #subsituti os valores vector WD pela direacao do vento correspondente aos maximos, min.... de velocidade
      sensores_w<-which(substr(names(dados),0,1)=="W")
      if (!!length(sensores_wd)) {
        indices <- c(which.quantile(x = dados$WS.7,probs = c(0.25,0.5,0.75),na.rm = T),which.min(dados$WS.7),which.max(dados$WS.7))
        dados_aux$WD.7 <- dados$WD.7[indices]
      }
      
      #conversoes
      dados_aux$idinfo<-as.character(dados_aux$idinfo)
      dados <- dados_aux
    }
    #*** Obtain statistical WIND quantities (mode)----------------------------------------------------------------
    if (statistics_output==T) {
      if (!!length(sensores_wd)) {
        #Divisão da direção do vento em 16 intervalos, one o primeiro varia entre 348.75 e 11.25 e é representado pelo valor 0º
        
        #16 intervalos
        dadosrawvento <- dadosrawvento-11.25
        range(dadosrawvento[complete.cases(dadosrawvento)])
        hist <- hist(dadosrawvento,breaks = 22.5*0:16-11.25,plot = F)
        
        #8 intervalos
        # dadosrawvento <- dadosrawvento - 22.5
        # range(dadosrawvento[complete.cases(dadosrawvento)])
        # hist <- hist(dadosrawvento,breaks = 45*0:8 - 22.5, plot = F)
        
        mode <- hist$mids[which.max(hist$counts)]
        dados_vento_moda <- cbind(datahora,as.data.frame(mode),idinfo=c("mode"));
        names(dados_vento_moda)[2] <- names(dados)[sensores_wd]
        dados<-rbind.fill(dados,dados_vento_moda)
      }
    }
  }
  
  
  #  FREIXO ------------------------------------------------------------------------
  if (structure=="freixo") {
    #*** Acceleration ------------------------------------------------------------
    sensores_a<-which(substr(names(dados),0,1)=="A")
    if (!!length(sensores_a)) {
      #****** Center and square -------------------------------------------------------------------
      dados[,sensores_a]<-scale(dados[,sensores_a],center=apply(dados[,sensores_a],2,median,na.rm=T),scale=F)
    }
    
    #*** Vertical displacement ------------------------------------------------------------
    sensores_dv<-which(substr(names(dados),0,2)=="NL")
    if(all(c("NL.1M","NL.2M")  %in% names(dados))){dados[,"NL.1M"] <- dados[,"NL.2M"] - dados[,"NL.1M"]}
    if(all(c("NL.1J","NL.2J")  %in% names(dados))){dados[,"NL.1J"] <- dados[,"NL.2J"] - dados[,"NL.1J"]}
    
    #*** Temperature ------------------------------------------------------------
    sensores_TP<-which(substr(names(dados),0,1)=="T")
    if (!!length(sensores_TP)) {dados[,sensores_TP]  <-  dados[,sensores_TP]*2.4552-223.65}
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {}
    
  }
  
  
  #  RIBEIRA D'AGUA ------------------------------------------------------------------------
  if (structure=="ribeira_agua") {
    
    
    #*** Efetive strains ----------------------------------------------------------------
    numeracao <- c("1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8",
                   "2.1","2.2","2.3","2.4","2.5","2.6","2.7","2.8")
    ext <- paste0("C.",numeracao)
    tmp <- paste0("TR.",numeracao)
    
    if(any(is.element(ext,colnames(dados)))){
      
      data_0 <- "2014-08-15 06:00:00"
      zeros_c <- c(2670,2665,2722,2606,2747,3116,2668,2609,
                   2803,2675,2692,2682,2781,2581,2436,36772) #Leitura original ()
      zeros_t <- c(34.88,45.57,45.92,44.47,34.39,53.64,61.3,54.58,
                   34.54,49.07,49.23,45.01,34.24,57.86,63.38,59.34) #Leitura original (TR.1.1,TR.1.2,TR.2.1,TR.2.2)
      
      for (ii in 1:length(ext)){
        if(all(c(ext[ii],tmp[ii]) %in% colnames(dados))){
          dados[,ext[ii]] <- zeros_c[ii] - dados[,ext[ii]] + (zeros_t[ii] - dados[,tmp[ii]])*(12.2-10)  
        }else{
          dados[,ext[ii]] <- NA
        }
      }
    };rm(numeracao,ext,tmp,zeros_c,zeros_t)
    
    
    #*** Shrinkage strains ----------------------------------------------------------------
    numeracao <- c("1.1","1.2","1.3")
    ext <- paste0("CR.",numeracao)
    tmp <- paste0("TRCR.",numeracao)
    
    if(any(is.element(ext,colnames(dados)))){
      
      data_0 <- "2014-08-15 11:55:00"
      zeros_c <- c(2828.24,2810.82,2774.86) #Leitura original ()
      zeros_t <- c(27.5,27.73,27.43) #Leitura original (TR.1.1,TR.1.2,TR.2.1,TR.2.2)
      
      for (ii in 1:length(ext)){
        if(all(c(ext[ii],tmp[ii]) %in% colnames(dados))){
          dados[,ext[ii]] <- zeros_c[ii] - dados[,ext[ii]] + (zeros_t[ii] - dados[,tmp[ii]])*(12.2-10)  
        }else{
          dados[,ext[ii]] <- NA
        }
      }
    };rm(numeracao,ext,tmp,zeros_c,zeros_t)
    
    
    #*** Creep strains ----------------------------------------------------------------
    numeracao <- c("1.1","1.2","1.3")
    ext <- paste0("CF.",numeracao)
    tmp <- paste0("TRCF.",numeracao)
    
    if(any(is.element(ext,colnames(dados)))){
      
      data_0 <- "2014-08-15 11:55:00"
      zeros_c <- c(2659.2,2671.03,2709.59) #Leitura original ()
      zeros_t <- c(27.31,27.19,27.31) #Leitura original (TR.1.1,TR.1.2,TR.2.1,TR.2.2)
      
      for (ii in 1:length(ext)){
        if(all(c(ext[ii],tmp[ii]) %in% colnames(dados))){
          dados[,ext[ii]] <- zeros_c[ii] - dados[,ext[ii]] + (zeros_t[ii] - dados[,tmp[ii]])*(12.2-10)  
        }else{
          dados[,ext[ii]] <- NA
        }
      }
    };rm(numeracao,ext,tmp,zeros_c,zeros_t)
    
    
    #*** Creep coefficients  ----------------------------------------------------------------
    numeracao <- c("1.1","1.2","1.3")
    ext_CR <- paste0("CR.",numeracao)        
    ext_CF <- paste0("CF.",numeracao)        
    cf_pressao <- paste0("PF.",numeracao)    
    cf_fluencia <- paste0("CFlu.",numeracao) 
    
    
    if(any(is.element(cf_pressao,colnames(dados)))){
      
      data_0 <- "2014-08-15 11:55:00"
      zeros_cf_pressao <- c(60,60,60) #Leitura original (PF.1.1, PF.1.2, PF.1.3)
      deformacao_elastica <- c(126,123,136) #CF.1.1, CF.1.2, CF.1.3
      Ecm <- 36.9 #MPa
      Ec0 <- 29.8 #MPa
      
      #Average of shrinkage strains
      vec_media_extensoes_retracao <- apply(dados[,ext_CR],1,mean,na.rm = T)
      
      dados_aux <- data.frame(matrix(NA,ncol = length(cf_fluencia),nrow = nrow(dados)))
      for (ii in 1:length(cf_fluencia)){
        if(all(c(ext_CR[ii],ext_CF[ii],cf_pressao[ii]) %in% colnames(dados))){
          
          #Extensões de fluência deduzida a retração e a variação elástica
          #CF,i-CR,med,i-(PF,i-PF,0)*(PF,0/eps,0)
          dados_aux[,ext_CF[ii]] <- 
            dados[,ext_CF[ii]]-vec_media_extensoes_retracao - (dados[,cf_pressao[ii]] - zeros_cf_pressao[ii]) * (deformacao_elastica[ii] / zeros_cf_pressao[ii])
          
          #calcula o COEFICIENTE DE FLUENCIA Ec0
          dados[,cf_fluencia[ii]] <- dados_aux[,ext_CF[ii]]/deformacao_elastica[ii]
          
          #calcula o COEFICIENTE DE FLUENCIA Ecm
          dados[,cf_fluencia[ii]] <- dados[,cf_fluencia[ii]]*Ec0/Ecm
        }else{
          dados[,cf_fluencia[ii]] <- NA
        }
      }
    };rm(numeracao,ext_CR,ext_CF,cf_pressao,cf_fluencia,zeros_cf_pressao,deformacao_elastica,Ecm,Ec0)
    
    
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {}
    
  }
  
  #  SABOR ------------------------------------------------------------------------
  if (structure=="sabor") {
    
    #*** Change Old Variables Names ------------------------------------------------------------
    dados_nomes_errados <- c("C.1.1","C.1.2","C.2.1","C.2.2",
                             "C1.1","C1.2","C2.1","C2.2",
                             "TR1.1","TR1.2","TR2.1","TR2.2"
    )
    dados_nomes_corretos <- c("DR.1.1","DR.1.2","DR.2.1","DR.2.2",
                              "DR.1.1","DR.1.2","DR.2.1","DR.2.2",
                              "TR.1.1","TR.1.2","TR.2.1","TR.2.2"
    )
    
    if(any(is.element(dados_nomes_errados,colnames(dados)))){
      i_cont_name <- 0
      for (ii in 1:length(dados_nomes_errados)) {
        if(is.element(dados_nomes_errados[ii],colnames(dados))){
          indice_coluna <- which(colnames(dados)==dados_nomes_errados[ii])
          colnames(dados)[indice_coluna] <-dados_nomes_corretos[ii] 
        }
      }
    }
    
    #*** Efetive strains ----------------------------------------------------------------
    
    ext <- c("DR.1.1","DR.1.2","DR.2.1","DR.2.2")
    tmp <- c("TR.1.1","TR.1.2","TR.2.1","TR.2.2")
    if(any(is.element(ext,colnames(dados)))){
      coef_ext <- c(0.0000116,0.00000916,0.00001107,0.00000911)
      data_0 <- "2014-06-08 10:00:00"
      zeros_dr <- c(2459.654,2451.218,2507.985,2457.334) #Leitura original (DR.1.1,DR.1.2,DR.2.1,DR.2.2)
      zeros_tr <- c(27.614,  27.538,  28.416,  28.294  ) #Leitura original (TR.1.1,TR.1.2,TR.2.1,TR.2.2)
      for (ii in 1:length(ext)){dados[,ext[ii]]<-((dados[,ext[ii]])^2-(zeros_dr[ii])^2)*coef_ext[ii]+(dados[,tmp[ii]]-zeros_tr[ii])*0.02}
    }
    
    #*** Resistivity corretion ----------------------------------------------------------
    sensores_r <- which(substr(names(dados),0,2)=="R.")
    indices_0 <- c()
    if (!!length(sensores_r)) {for (i in sensores_r){indices_0 <- which(dados[,i]<=0); dados[indices_0,i] <- 250000}}
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {}
    
  }
  
  
  #  CRIZ2 ------------------------------------------------------------------------
  if (structure=="criz2") {
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {}
    
  }
  
  
  #  SALGUEIRO MAIA ------------------------------------------------------------------------
  if (structure=="salgueiro maia") {
    
    #*** Vertical displacements ----------------------------------------------------------
    #calcula as series temporais zeradas
    zeros = data.frame(t(c(959.9, 626, 882.7, 824.3, 830, 719, 852.2, 775.5, 552.7, 1084)))  #Zeros dos NLs em 2017-07-08 00:00
    colnames(zeros) <- c("NL1","NL2","NL3J","NL3M","NL4J","NL4M","NL5J","NL5M","NL6","NL7")
    
    #calcula os valores reais dos NL's
    if(is.element("NL2",colnames(dados))){
      if(is.element("NL1",colnames(dados))){   dados[,"NL1"]  <- zeros[,"NL1"]-dados[,"NL1"]-(zeros[,"NL2"]-dados[,"NL2"])}
      if(is.element("NL3J",colnames(dados))){  dados[,"NL3J"] <- zeros[,"NL3J"]-dados[,"NL3J"]-(zeros[,"NL2"]-dados[,"NL2"])}
      if(is.element("NL3M",colnames(dados))){  dados[,"NL3M"] <- zeros[,"NL3M"]-dados[,"NL3M"]-(zeros[,"NL2"]-dados[,"NL2"])}
    }else{
      if(is.element("NL1",colnames(dados))){   dados[,"NL1"]  <- NA     }
      if(is.element("NL3J",colnames(dados))){  dados[,"NL3J"] <- NA     }
      if(is.element("NL3M",colnames(dados))){  dados[,"NL3M"] <- NA     }
    }
    
    if(is.element("NL6",colnames(dados))){
      if(is.element("NL4J",colnames(dados))){  dados[,"NL4J"] <- zeros[,"NL4J"]-dados[,"NL4J"]-(zeros[,"NL6"]-dados[,"NL6"])}
      if(is.element("NL4M",colnames(dados))){  dados[,"NL4M"] <- zeros[,"NL4M"]-dados[,"NL4M"]-(zeros[,"NL6"]-dados[,"NL6"])}
      if(is.element("NL5J",colnames(dados))){  dados[,"NL5J"] <- zeros[,"NL5J"]-dados[,"NL5J"]-(zeros[,"NL6"]-dados[,"NL6"])}
      if(is.element("NL5M",colnames(dados))){  dados[,"NL5M"] <- zeros[,"NL5M"]-dados[,"NL5M"]-(zeros[,"NL6"]-dados[,"NL6"])}
      if(is.element("NL7",colnames(dados))){   dados[,"NL7"]  <- zeros[,"NL7"]-dados[,"NL7"]-(zeros[,"NL6"]-dados[,"NL6"])}
    }else{
      if(is.element("NL4J",colnames(dados))){  dados[,"NL4J"] <- NA    }
      if(is.element("NL4M",colnames(dados))){  dados[,"NL4M"] <- NA    }
      if(is.element("NL5J",colnames(dados))){  dados[,"NL5J"] <- NA    }
      if(is.element("NL5M",colnames(dados))){  dados[,"NL5M"] <- NA    }
      if(is.element("NL7",colnames(dados))){   dados[,"NL7"]  <- NA    }
    }
    
    ##****** Vibrating wire based strain -------------------------------------
    sensores_extensao_C<-setdiff(names(dados)[which(substr(names(dados),0,1)=="C")],names(dados)[which(substr(names(dados),0,2)=="CL")])
    dados[,sensores_extensao_C] <- dados[,sensores_extensao_C]*0.7
    
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {}
    
  }
  
  #  SAO JOAO ------------------------------------------------------------------------
  if (structure=="sao joao") {
    
    #*** Strains ----------------------------------------------------------
    zeros_EXT <- data.frame(t(c(#Extensometros
      6824,6733,6787,6898,6930,6952,6963,6901,
      6887,6969,6926,6811,6843,6895,6828,6824,
      6919,6798,6819,6792,6828,6942,6926,6798,
      6910,6832,6879,6912,6853,6936,6843,6951,6988,6991,7038,6850,6934,6819,
      6914,6919,6894,6930,6944,6828,6964,6988,7034,6948,6884,6940,6852,6931,
      6863,6870,6845,6880,6835,6871,6861,6872,
      6949,6999,6861,6819,6961,6967,6871,6891,
      7039,6885,6938,6904,6953,6983,6945,7086,
      6969,6888,6963,6973,6895,6918,7079,6966,6888,7012,7023,6988,6946,7081,
      6914,6919,6894,6930,6944,6828,6964,6988,
      6851,6862,6892,6796,6891,6797,6914,6858,
      6855,6922,6854,6881,6845,6864,6806,6889,6825,6845,
      6842,6836,6900,6866,6963,6864,6948,6899
      #
    )))
    colnames(zeros_EXT) <-
      c('C.1.1','C.1.2','C.1.3','C.1.4','C.1.5','C.1.6','C.1.7','C.1.8',
        'C.2.1','C.2.2','C.2.3','C.2.4','C.2.5','C.2.6','C.2.7','C.2.8',
        'C.3.1','C.3.2','C.3.3','C.3.4','C.3.5','C.3.6','C.3.7','C.3.8',
        'C.4.1','C.4.2','C.4.3','C.4.4','C.4.5','C.4.6','C.4.7','C.4.8','C.4.9','C.4.10','C.4.11','C.4.12','C.4.13','C.4.14',
        'C.5.1','C.5.2','C.5.3','C.5.4','C.5.5','C.5.6','C.5.7','C.5.8','C.5.9','C.5.10','C.5.11','C.5.12','C.5.13','C.5.14',
        'C.6.1','C.6.2','C.6.3','C.6.4','C.6.5','C.6.6','C.6.7','C.6.8',
        'C.7.1','C.7.2','C.7.3','C.7.4','C.7.5','C.7.6','C.7.7','C.7.8',
        'C.8.1','C.8.2','C.8.3','C.8.4','C.8.5','C.8.6','C.8.7','C.8.8',
        'C.9.1','C.9.2','C.9.3','C.9.4','C.9.5','C.9.6','C.9.7','C.9.8','C.9.9','C.9.10','C.9.11','C.9.12','C.9.13','C.9.14',
        'C.10.1','C.10.2','C.10.3','C.10.4','C.10.5','C.10.6','C.10.7','C.10.8',
        'C.11.1','C.11.2','C.11.3','C.11.4','C.11.5','C.11.6','C.11.7','C.11.8',
        'C.12.1','C.12.2','C.12.3','C.12.4','C.12.5','C.12.6','C.12.7','C.12.8','C.12.9','C.12.10',
        'C.13.1','C.13.2','C.13.3','C.13.4','C.13.5','C.13.6','C.13.7','C.13.8')
    
    #zera as variaveis
    for(sensor_i in colnames(zeros_EXT)){if(is.element(sensor_i,colnames(dados))){dados[,sensor_i] <- zeros_EXT[,sensor_i] - dados[,sensor_i]}}
    
    #*** Vertical displacement ----------------------------------------------------------
    zeros_NL <- data.frame(t(c(
      #NL.1J, NL.2J,  NL.3J,  NL.4J,  NL.5J,  NL.6J,  NL.7J,  NL.8J,  NL.9J
      2507.3, 2727.8, 2507.9, 2703.3, 2389.1, 2619.1, 2651.4, 2547.8, 2559.7,
      #NL.1M, NL.2M,  NL.3M,  NL.4M,  NL.5M,  NL.6M,  NL.7M,  NL.8M,  NL.9M
      2932.4, 2990.8, 2897.1, 3207.4, 2729.1, 3172.4, 2697.4, 2964.8, 2742.6
    )))
    colnames(zeros_NL) <- c(
      "NL.1J","NL.2J","NL.3J","NL.4J","NL.5J","NL.6J","NL.7J","NL.8J","NL.9J",
      "NL.1M","NL.2M","NL.3M","NL.4M","NL.5M","NL.6M","NL.7M","NL.8M","NL.9M"
    )
    
    #zera as variaveis
    for(sensor_i in colnames(zeros_NL)){if(is.element(sensor_i,colnames(dados))){dados[,sensor_i] <- zeros_NL[,sensor_i] - dados[,sensor_i]}}
    
    #calcula os deslocametnos verticais efetivos
    sensores_NL_mont <- colnames(dados)[
      which(substr(colnames(dados),1,2)=="NL" &
              lapply(strsplit(colnames(dados),""),function(i) rev(i)[1])=="M")]
    #sensores NL do lado jusante
    sensores_NL_jus <- colnames(dados)[
      which(substr(colnames(dados),1,2)=="NL" &
              lapply(strsplit(colnames(dados),""),function(i) rev(i)[1])=="J")]
    #Calculo - correcao dos valores pela celula de referencia
    referencia <- "NL.3M"
    if(is.element(referencia,sensores_NL_mont)){
      vector_referencia <- dados[,referencia]
      for(NL_k in sensores_NL_mont[-which(sensores_NL_mont==referencia)]){
        dados[,NL_k] <- (dados[,NL_k] - vector_referencia) 
      }
    }else{ #se nao existir valor da celula de referencia os valores das restantes celulas sao considerados NA
      dados[,sensores_NL_mont] <- NA
    }
    
    referencia <- "NL.3J"
    if(is.element(referencia,sensores_NL_jus)){
      vector_referencia <- dados[,referencia]
      for(NL_k in sensores_NL_jus[-which(sensores_NL_jus==referencia)]){#tira a referencia a todos menos ao proprio
        dados[,NL_k] <- (dados[,NL_k] - vector_referencia) 
      }
    } else{ #se nao existir valor da celula de referencia os valores das restantes celulas sao considerados NA
      dados[,sensores_NL_jus] <- NA
    }
    
    
    #*** Rotation ------------------------------------------------------------
    zeros_CL <- data.frame(t(c(
      -1348, -286,
      1178 ,600,
      3    ,-225,
      -395 ,781
    )))
    
    colnames(zeros_CL) <- c(
      "CL.11.L","CL.11.T",
      "CL.12.L","CL.12.T",
      "CL.13.L","CL.13.T",
      "CL.14.L","CL.14.T"
    )
    
    #zera as variaveis
    for(sensor_i in colnames(zeros_CL)){if(is.element(sensor_i,colnames(dados))){dados[,sensor_i] <- zeros_CL[,sensor_i] - dados[,sensor_i]}}
    
    
    #*** Horizontal displacement ------------------------------------------------------------
    zeros_DH <- data.frame(t(c(
      207.2,203.8,215.8,221
    )))
    
    colnames(zeros_DH) <- c(
      "MJ.EDM","MJ.EDJ","MJ.EEJ","MJ.EEM"
    )
    
    #zera as variaveis
    for(sensor_i in colnames(zeros_DH)){if(is.element(sensor_i,colnames(dados))){dados[,sensor_i] <- zeros_DH[,sensor_i] - dados[,sensor_i]}}
    
    
    #*** Obtain statistical Quantities ---------------------------------------------------
    if (statistics_output==T) {}
  }   
  
  # RETURN --------------------------------------------------------------------------
  return(dados)
  
}