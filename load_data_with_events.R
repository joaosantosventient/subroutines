
load("//de-noe-comp1/shm/shm_data/25 abril/COMP/201912170900.RData")


load_data_with_events<-function(ficheiro,directoria_dados_COMP=directoria_dados_COMP,) {
  
  #  25 ABRIL------------------------------------------------------------------------
  if (structure=="25 abril") {
    
    if("e1.0" %in% names(dados)) {
      
    }
    
  }
  
  # load(file.path(directoria_dados_COMP,ficheiro))
  # load(file.path(directoria_dados_TRIG,ficheiro))
  # if (length(lista_trigger)>0) {
  #   trigger_vec<-unlist(lista_trigger)
  #   dados_com_trigger<-list()
  #   for (i in 1:length(lista_trigger)) {
  #     dados_com_trigger[[i]]<-dados[lista_trigger[[i]],]
  #   }
  #   names(dados_com_trigger)<-names(lista_trigger)
  #   dados_sem_trigger<-dados[-trigger_vec,]
  # } else {
  #   dados_sem_trigger<-dados
  #   dados_com_trigger<-NULL
  # }

  return(list(dados=dados,dados_com_events=dados_com_events,dados_sem_events=dados_sem_events,dados_sem_events_1=dados_sem_events_1))
}