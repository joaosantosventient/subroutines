
load_data_with_trigger<-function(ficheiro,directoria_dados_COMP=directoria_dados_COMP,directoria_dados_TRIG=directoria_dados_TRIG) {
  
  load(file.path(directoria_dados_COMP,ficheiro))
  load(file.path(directoria_dados_TRIG,ficheiro))
  if (length(lista_trigger)>0) {
    trigger_vec<-unlist(lista_trigger)
    dados_com_trigger<-list()
    for (i in 1:length(lista_trigger)) {
      dados_com_trigger[[i]]<-dados[lista_trigger[[i]],]
    }
    names(dados_com_trigger)<-names(lista_trigger)
    dados_sem_trigger<-dados[-trigger_vec,]
  } else {
    dados_sem_trigger<-dados
    dados_com_trigger<-NULL
  }

  return(list(lista_trigger=lista_trigger,dados=dados,dados_com_trigger=dados_com_trigger,dados_sem_trigger=dados_sem_trigger))
}