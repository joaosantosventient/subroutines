fdd<-function(seriestemp,samplerate,spans=10) {  
  
  library(pracma)
  library(astsa)
  
  # esta funcao serve para fazer o fdd e tem como entradas as series temporais
  # e a sample rate e como saidas as mesmas series assim como a matrix de espetros,
  # os espetros de valores proprios e os modos de vibracao (vewtores proprios)
  
  # Centrar os dados recebidos
  seriestemp<-scale(seriestemp,scale=F,center=T)
  
  # Eliminar tendências
  seriestemp<-detrend(seriestemp,tt='linear',bp=nrow(seriestemp))   #tt-trend type, bp-break point do ficheiro seriestemp
  
  # Transformar a nossa série numa série temporal
  seriesvec<-seriestemp
  seriestemp<-ts(seriestemp)
  
  # Criar o espectro
  espectro<-mvspec(seriestemp,plot=F,spans=c(spans,spans))     #spans-é para especificar a suavidade/polidez do espectro os valores 10 são os que são comum usar
  
  # Criar a matriz espectral
  matriz_spec<-espectro$fxx     #fxx-é a estimativa da matriz espectral
  
  # Fazer a matriz dos valores singulares do espectro
  svspec<-array(dim=c(dim(matriz_spec)[1],dim(matriz_spec)[2],dim(matriz_spec)[3]))
  for (i in 1:dim(matriz_spec)[3]) {
    svspec[,,i]<-diag(eigen(matriz_spec[,,i])$values)
  }
  
  # Fazer a matriz dos modos
  modos<-matrix(NA,nrow=dim(matriz_spec)[3],ncol=dim(matriz_spec)[2])
  for (i in 1:dim(matriz_spec)[3]) {
    modos[i,]<-eigen(matriz_spec[,,i])$vectors%*%(eigen(matriz_spec[,,i])$values)
  }
  
  # Output da funcao
    fdd_list<-list(freq=espectro$freq*samplerate,seriesvec=seriesvec,matriz_spec=matriz_spec,svspec=svspec,modos=modos)
    return(fdd_list)
  
}


# Encontrar frequencias de vibracão
# if(obter_modos==T) {
#   frequencias_modais<-c()
#   for (i in estimativas_frequencias) {
#     freq_banda<-i*freq_lims
#     spec_banda<-svspec[1,1,which(espectro$freq*samplerate>freq_banda[1] & espectro$freq*samplerate<freq_banda[2])]
#     frequencias_modais<-c(frequencias_modais,freq_banda[1]+espectro$freq[which.max(spec_banda)])
#   }
# }

# Fazer a matriz dos modos
# if(obter_modos==T) {
# modos<-matrix(NA,nrow=dim(matriz_spec)[3],ncol=dim(matriz_spec)[2])
# for (i in 1:dim(matriz_spec)[3]) {
#   modos[i,]<-eigen(matriz_spec[,,i])$vectors%*%(eigen(matriz_spec[,,i])$values)
# }
# for (i in 1:dim(modos)[1]) {
#   a<-(Arg(modos[i,])<=pi/2 & Arg(modos[i,])>=-pi/2)
#   a[which(a==T)]<-1
#   a[which(a==F)]<--1
#   modos[i,]<-Mod(modos[i,])*a
#   modos[i,]<-modos[i,]/modos[i,which.max(abs(modos[i,]))]
# }
# modos<-Re(modos)


# Obter configuracoes modais
# if(obter_modos==T) {
#   posicoes<-c()
#   for (j in 1:length(frequencias_modais)) {posicoes[j]<-min(which(espectro$freq*samplerate>=frequencias_modais[j]))}
#   configuracoes_modais<-modos[posicoes,]
# }

