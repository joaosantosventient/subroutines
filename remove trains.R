# 
# diffpace<-10
# resol_freq<-5e-4
# detectvar<-c("dm.P1","dm.P7")

remove_trains<-function(dadosmodal,detectvar,samplerate,diffpace,resol_freq) {
  for (ii in 1:length(detectvar)) {if (ii==1) {identvar<-abs(diff(dados[,detectvar[ii]],diffpace))} else {identvar<-identvar+abs(diff(dados[,detectvar[ii]],diffpace))}}
  identvar<-ma(identvar,samplerate*60)
  valim<-sort(identvar)[1/resol_freq*samplerate]
  dadosmodal<-dadosmodal[-which(identvar>valim),]
  return(dadosmodal)
}
# dadosmodal<-remove_trains(dadosmodal,detectvar,samplerate,diffpace,resol_freq) 