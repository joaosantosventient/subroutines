# diretoria_dados<-diretoria_dados
# decimfactor<-1
# nomeficheiromodal<-"modal "
# modos_a_escolher<-NULL
# modos_a_escolher<-c(1,3,6,7,8,10)


mergefilemodal<-function(diretoria_dados,decimfactor=1,nomeficheiromodal="modal ",modos_a_escolher=NULL,loaded_SSI=NULL) {
  
  library(plyr)
  
  if (is.null(modos_a_escolher)) {
    ls<-list.files(diretoria_dados,pattern=nomeficheiromodal)
  } else {
    if (!is.null(loaded_SSI)) { # recursive
      modos_loaded_SSI<-unique(loaded_SSI$modo)
      modos_a_retirar<-setdiff(modos_loaded_SSI,modos_a_escolher)
      modos_a_escolher<-setdiff(modos_a_escolher,modos_loaded_SSI)
    } # /recursive
    
    ls<-c()
    if (length(modos_a_escolher)>=1) { # recursive 
      for (i in 1:length(modos_a_escolher)) {
        if (nchar(modos_a_escolher[i])==1) {ls[i]<-paste0(nomeficheiromodal,"00",modos_a_escolher[i],".RData")}
        if (nchar(modos_a_escolher[i])==2) {ls[i]<-paste0(nomeficheiromodal,"0",modos_a_escolher[i],".RData")}
      }
    }
  }
  ls<-sort(ls)
  
  if (length(ls)>=1) { # recursive
    for (k in 1:length(ls)) {
      if (k==1) {
        load(file.path(diretoria_dados,ls[k]))
        if (exists("modal")) {ssi<-modal; rm(modal)}
        ssidf<-ssi[seq(1,nrow(ssi),by=decimfactor),]
      } else {
        load(file.path(diretoria_dados,ls[k]))
        if (exists("modal")) {ssi<-modal; rm(modal)}
        ssidf<-rbind.fill(ssidf,ssi[seq(1,nrow(ssi),by=decimfactor),])
      }
      print(ls[k])
    }
    ssidf<-ssidf[order(ssidf$datahora),]
    ssidf<-ssidf[,c("datahora","f","d",names(ssidf)[which(substr(names(ssidf),0,1)=="a")],names(ssidf)[which(substr(names(ssidf),0,1)!="a" & substr(names(ssidf),0,1)!="f" & substr(names(ssidf),0,1)!="d")])]
  }
  
  if (!is.null(loaded_SSI)) { # recursive
    if (length(modos_a_retirar)>=1) {
      for(j in modos_a_retirar){
        ssidf<-subset(loaded_SSI,modo!=modos_a_retirar)
      }
    } else {
      ssidf<-rbind.fill(ssidf,loaded_SSI)
    }
  } # /recursive
  
  
  return(ssidf)
}












































# 
# 
# mergefilemodal<-function(diretoria_dados,decimfactor=1,nomeficheiromodal="modal ") {
#   library(plyr)
#   ls<-list.files(diretoria_dados,pattern=nomeficheiromodal)
#   ls<-sort(ls)
#   for (i in 1:length(ls)) {
#     if (i==1) {
#       load(file.path(diretoria_dados,ls[i]))
#       if (exists("modal")) {ssi<-modal; rm(modal)}
#       ssidf<-ssi[seq(1,nrow(ssi),by=decimfactor),]
#     } else {
#       load(file.path(diretoria_dados,ls[i]))
#       if (exists("modal")) {ssi<-modal; rm(modal)}
#       ssidf<-rbind.fill(ssidf,ssi[seq(1,nrow(ssi),by=decimfactor),])
#     }
#     print(ls[i])
#   }
#   ssidf<-ssidf[order(ssidf$datahora),]
#   ssidf<-ssidf[,c("datahora","f","d",names(ssidf)[which(substr(names(ssidf),0,1)=="a")],names(ssidf)[which(substr(names(ssidf),0,1)!="a" & substr(names(ssidf),0,1)!="f" & substr(names(ssidf),0,1)!="d")])]
# return(ssidf)
# }
# 
#   
#   
#   
#   
#   
#   























# 
# if (structure=="25 abril" & correccoes==T) {
#   ssidf$f[which(ssidf$datahora>=as.POSIXct("2017-06-17 1:00:00") & ssidf$datahora<=as.POSIXct("2018-01-26 1:00:00"))]<-(1-(11/500))*ssidf$f[which(ssidf$datahora>=as.POSIXct("2017-06-17 1:00:00") & ssidf$datahora<=as.POSIXct("2018-01-26 1:00:00"))]
# }

# 
# 
# pdf(file.path(diretoria,"ssi ruido.pdf"),w=30,h=16)
#   plot(ssidf$datahora,ssidf$f,pch=".")
#   abline(v=as.POSIXct("2017-06-17 1:00:00"),col="red",lty=2,lwd=2)
#   abline(v=as.POSIXct("2018-01-26 1:00:00"),col="green",lty=2,lwd=2)
# dev.off()
# # 
# 
# windows(); plot(ssidf$datahora,ssidf$f,pch=".",xlim=c(as.POSIXct("2017060100",format="%Y%m%d%H"),as.POSIXct("2017080100",format="%Y%m%d%H")),ylim=c(3,6))
# 
#  
# 
# windows(); plot(as.POSIXct(as.character(substr(vec_ssi,4,14)),format="%Y%m%d%H"),t="l")
# windows(); plot(as.POSIXct(as.character(substr(vec_ficheiros,1,10)),format="%Y%m%d%H"),t="l")
