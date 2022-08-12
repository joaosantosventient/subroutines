# dados=dadosfilt
# ordem_max=ssiordmax
# subspace_noise=0.1
# rate=decim_rate
# type="cov"


fastssi_func<-function(dados,ordem_max,subspace_noise,rate,type) {
  dados<-dados[,colSums(is.na(dados))<nrow(dados)]
  dados<-dados[as.numeric(which(rowSums(dados,na.rm=T)!=0)),]
  dados<-dados[,which(apply(dados,2,sd)>1e-9)]
  ###
  ordem<-ordem_max
  req_order<-round(ordem/(1-subspace_noise),0)
  limsup<-floor(dim(dados)[1]/(1+dim(dados)[2]))  
  if (req_order<limsup & req_order>0) {p<-req_order} else {p<-limsup}
  if (type=="rd") {hankelmat<-hankelmatrd(dados,p)}
  if (type=="cov") {hankelmat<-hankelmatcov(dados,p)}
  ##
  s<-diag(sqrt(svd(hankelmat)$d)[1:(ordem)])
  u<-svd(hankelmat)$u[,1:(ordem)]
  obsermat<-u%*%s
  observmat_remain<-obsermat[(dim(dados)[2]+1):dim(obsermat)[1],]    # O seta baixo
  observmat_subset<-obsermat[1:(dim(dados)[2]*(p-1)),]               # O seta cima
  C<-obsermat[1:dim(dados)[2],]    
  qru<-qr(observmat_subset)
  S<-t(qr.Q(qru))%*%observmat_remain
  freqs<-damps<-modos<-list()
  for (ordem in 2:ordem_max) {
    R<-qr.R(qru)
    A<-solve(R[1:ordem,1:ordem])%*%S[1:ordem,1:ordem]
    Citer<-C[1:dim(dados)[2],1:ordem]
    evevA<-eigen(A)
    evalA<-evevA$values
    evecA<-evevA$vectors
    modos[[ordem]]<-t(Citer%*%evecA)
    freqs[[ordem]]<-rate*Arg(evalA)/(2*pi)
    damps[[ordem]]<-abs(log(Mod(evalA)))
    # print(ordem)
  }

  ###
  names(freqs)<-1:length(freqs); 
  ordem<-c(); for (ioi in 1:length(freqs)) {ordem<-c(ordem,rep(names(freqs)[ioi],length(freqs[[ioi]])))}
  ssi<-data.frame(cbind(data.frame(ordem=ordem,f=unlist(freqs),d=unlist(damps),Reduce(rbind,modos))))
  ssi<-ssi[order(ssi$f),]
  ssi[,"ordem"]<-as.numeric(as.character(ssi[,"ordem"]))
  colnames(ssi)<-c("ordem","f","d",names(dados))

  ###
  for (ijk in 1:dim(ssi)[1]) {ssi[ijk,4:dim(ssi)[2]]<-as.complex(ssi[ijk,4:dim(ssi)[2]])/max(Mod(as.complex(ssi[ijk,4:dim(ssi)[2]])),na.rm=T)}
  
  ###
  if (type=="cov") {
    return(list(ssi=ssi))
  }
  if (type=="rd") {
    dadosrd<-dados
    return(list(ssi=ssi,dadosrd=dadosrd))
  }
  
}