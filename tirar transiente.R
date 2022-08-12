tirar_transiente<-function(filtro,sinal_filtrado,fator_len_filtro=2) {
  library(signal)
  # sinal_filtrado<-sinal_filtrado-mean(sinal_filtrado,na.rm=T)
  sinal_filtrado_sem_transiente<-c(
    rep(NA,fator_len_filtro*length(filtro[[1]])),
    sinal_filtrado[(length(filtro[[1]])*fator_len_filtro+1):(length(sinal_filtrado)-length(filtro[[1]])*fator_len_filtro)],
    rep(NA,fator_len_filtro*length(filtro[[1]]))
  )
  sinal_filtrado_sem_transiente<-sinal_filtrado_sem_transiente#-mean(sinal_filtrado_sem_transiente,na.rm=T)
  return(sinal_filtrado_sem_transiente)
}


# sinal_filtrado_sem_transiente<-c(
#   mean(sinal_filtrado,na.rm=T)+hanning(length(filtro[[1]])*fator_len_filtro*2)[1:(length(filtro[[1]])*fator_len_filtro)]*sign(sinal_filtrado[1:(length(filtro[[1]])*fator_len_filtro)]-mean(sinal_filtrado,na.rm=T))*(-1),
#   sinal_filtrado[(length(filtro[[1]])*fator_len_filtro+1):(length(sinal_filtrado)-length(filtro[[1]])*fator_len_filtro)],
#   mean(sinal_filtrado,na.rm=T)+hanning(length(filtro[[1]])*fator_len_filtro*2)[(length(filtro[[1]])*fator_len_filtro+1):(length(filtro[[1]])*fator_len_filtro*2)]*sign(sinal_filtrado[(length(sinal_filtrado)-length(filtro[[1]])*fator_len_filtro+1):(length(sinal_filtrado))]-mean(sinal_filtrado,na.rm=T)*(-1))
# )