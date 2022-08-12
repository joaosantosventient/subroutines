
ssi_func<-function(dados,ordem_max,subspace_noise,rate,type) {
  dadosinput<-dados
  # ssi
  freqs<-damps<-modos<-list()
  for (ordem in 2:ordem_max) {
    req_order<-round(ordem/(1-subspace_noise),0)
    limsup<-floor(dim(dadosinput)[1]/(1+dim(dadosinput)[2]))  
    if (req_order<limsup & req_order>0) {p<-req_order} else {p<-limsup}
    if (type=="rd") {hankelmat<-hankelmatrd(dadosinput,p)}
    if (type=="cov") {hankelmat<-hankelmatcov(dadosinput,p)}
    s<-diag(sqrt(svd(hankelmat)$d)[1:(ordem)])
    u<-svd(hankelmat)$u[,1:(ordem)]
    obsermat<-u%*%s
    observmat_remain<-obsermat[(dim(dadosinput)[2]+1):dim(obsermat)[1],]    # geralmente indicado na biblio como O com seta para baixo
    observmat_subset<-obsermat[1:(dim(dadosinput)[2]*(p-1)),]               # geralmente indicado na biblio como O com seta para cima
#           inv_observmat<-ginv(observmat_subset)
#           A<-inv_observmat%*%observmat_remain
          qru<-qr(observmat_subset)
          S<-t(qr.Q(qru))%*%observmat_remain
          A<-solve(qr.R(qru))%*%S
    C<-obsermat[1:dim(dadosinput)[2],]
    evevA<-eigen(A) 
    evalA<-evevA$values
    evecA<-evevA$vectors
    modos[[ordem]]<-t(C%*%evecA)
    freqs[[ordem]]<-rate*Arg(evalA)/(2*pi)
    damps[[ordem]]<-abs(log(Mod(evalA)))
    print(ordem)
  }
  # Organize data
  names(freqs)<-1:length(freqs); 
  ordem<-c(); for (ioi in 1:length(freqs)) {ordem<-c(ordem,rep(names(freqs)[ioi],length(freqs[[ioi]])))}
  ssi<-data.frame(cbind(data.frame(ordem=ordem,f=unlist(freqs),d=unlist(damps),Reduce(rbind,modos))))
  ssi<-ssi[order(ssi$f),]
  ssi[,"ordem"]<-as.numeric(as.character(ssi[,"ordem"]))
  colnames(ssi)<-c("ordem","f","d",names(dadosinput))

  # unscale mode shapes
  for (ijk in 1:dim(ssi)[1]) {ssi[ijk,4:dim(ssi)[2]]<-as.complex(ssi[ijk,4:dim(ssi)[2]])/max(Mod(as.complex(ssi[ijk,4:dim(ssi)[2]])),na.rm=T)}
  
  # output
  if (type=="cov") {
    return(list(ssi=ssi))
  }
  if (type=="rd") {
    dadosrd<-dadosinput
    return(list(ssi=ssi,dadosrd=dadosrd))
  }
  
}