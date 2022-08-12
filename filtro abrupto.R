


# versao sem parte no espelho do espetro
filtroabrupto<-function(dados,cuttofffreqinf,cuttofffreqsup,rate) {
  library(signal)
  if (is.matrix(dados) | is.data.frame(dados)) {
    dados[is.na(dados)]<-0
    dim1<-2^(1:50)[min(which(2^(1:50)>=dim(dados)[1]))]-dim(dados)[1]
    dim2<-dim(dados)[2]
    dfzeros<-data.frame(matrix(0,ncol=dim2,nrow=dim1)); names(dfzeros)<-names(dados)
    dadosfilt<-rbind(dados[,1:dim(dados)[2]],dfzeros[,1:dim(dados)[2]])
    for (mmm in 1:dim(dadosfilt)[2]) {
      fftemp<-fft(dadosfilt[,mmm])
      liminf_cutsup<-ceiling(dim(dadosfilt)[1]*cuttofffreqsup/rate)
      liminf_cutinf<-ceiling(dim(dadosfilt)[1]*cuttofffreqinf/rate)
      fftemp[c(1:liminf_cutinf,liminf_cutsup:length(fftemp))]<-0
      dados[,mmm]<-Re(ifft(fftemp))[1:dim(dados)[1]]
    }
  }
  if (is.vector(dados)) {
    dados[is.na(dados)]<-0
    dim1<-2^(1:50)[min(which(2^(1:50)>=length(dados)))]-length(dados)
    dfzeros<-rep(0,dim1)
    dadosfilt<-c(dados,dfzeros)
    fftemp<-fft(dadosfilt)
    liminf_cutsup<-ceiling(length(dadosfilt)*cuttofffreqsup/rate)
    liminf_cutinf<-ceiling(length(dadosfilt)*cuttofffreqinf/rate)
    fftemp[c(1:liminf_cutinf,liminf_cutsup:length(fftemp))]<-0
    dados<-Re(ifft(fftemp))[1:length(dados)]
  }
  return(dados)
}



# dados<-dadosfilt
# cuttofffreqinf<-freq_windows[[j]][1]
# cuttofffreqsup<-freq_windows[[j]][2]
# rate<-samplerate


# versao com parte no espelho do espetro
# filtroabrupto<-function(dados,cuttofffreqinf,cuttofffreqsup,rate) {
#   library(signal)
#     if (is.matrix(dados) | is.data.frame(dados)) {
#     dados[is.na(dados)]<-0
#     dim1<-2^(1:50)[min(which(2^(1:50)>=dim(dados)[1]))]-dim(dados)[1]
#     dim2<-dim(dados)[2]
#     dfzeros<-data.frame(matrix(0,ncol=dim2,nrow=dim1)); names(dfzeros)<-names(dados)
#     dadosfilt<-rbind(dados[,1:dim(dados)[2]],dfzeros[,1:dim(dados)[2]])
#     for (mmm in 1:dim(dadosfilt)[2]) {
#       fftemp<-stats::fft(dadosfilt[,mmm])
#       liminf_cutsup<-ceiling(dim(dadosfilt)[1]*cuttofffreqsup/rate)
#       limsup_cutsup<-floor(dim(dadosfilt)[1]*(1-cuttofffreqsup/rate))
#       fftemp[liminf_cutsup:limsup_cutsup]<-0
#       liminf_cutinf<-ceiling(dim(dadosfilt)[1]*cuttofffreqinf/rate)
#       limsup_cutinf<-floor(dim(dadosfilt)[1]*(1-cuttofffreqinf/rate))
#       fftemp[c(1:liminf_cutinf,limsup_cutinf:length(fftemp))]<-0
#       dados[,mmm]<-Re(signal::ifft(fftemp))[1:dim(dados)[1]]
#     }
#   }
#   if (is.vector(dados)) {
#     dados[is.na(dados)]<-0
#     dim1<-2^(1:50)[min(which(2^(1:50)>=length(dados)))]-length(dados)
#     dfzeros<-rep(0,dim1)
#     dadosfilt<-c(dados,dfzeros)
#     fftemp<-stats::fft(dadosfilt)
#     liminf_cutsup<-ceiling(length(dadosfilt)*cuttofffreqsup/rate)
#     limsup_cutsup<-floor(length(dadosfilt)*(1-cuttofffreqsup/rate))
#     fftemp[liminf_cutsup:limsup_cutsup]<-0
#     liminf_cutinf<-ceiling(length(dadosfilt)*cuttofffreqinf/rate)
#     limsup_cutinf<-floor(length(dadosfilt)*(1-cuttofffreqinf/rate))
#     fftemp[c(1:liminf_cutinf,limsup_cutinf:length(fftemp))]<-0
#     dados<-Re(signal::ifft(fftemp))[1:length(dados)]
#   }
#   return(dados)
# }
