sensors_and_quantities<-function (dados,structure,diretoria) {
  
  
  
  # SENSORS AND QUANTITIES 25 ABRIL ----------------------------------------------------------------
  if (structure=="25 abril") {
    
    #*** Load sensors and quantities list of dataframes ---------------------------------------------
    load(file=file.path(diretoria,"sensors_and_quantities.RData"))
    nomes_grandezas<-sensors_and_quantities[[structure]]$nomes_grandezas
    nomes_seccoes<-sensors_and_quantities[[structure]]$nomes_seccoes
    
    #*** Lista grandezas -----------------------------------------------------
    lista_grandezas<-list()
    nomes<-setdiff(names(dados),c("datahora","idinfo"))
    for (i in 1:nrow(nomes_grandezas)) {
      lista_grandezas[[as.character(nomes_grandezas[i,"Quantity"])]]<-nomes[which(substr(nomes,0,1)==as.character(nomes_grandezas[i,"Nomenclature"]))]
    }
    names(lista_grandezas)<-as.character(nomes_grandezas[,"Quantity"])
    
    #*** Lista seccoes -----------------------------------------------------
    lista_seccoes<-list()
    nomes<-setdiff(names(dados),c("datahora","idinfo"))
    nomessplit<-strsplit(nomes,".",fixed=T)
    nomes_lista_seccoes<-c()
    for (i in 1:nrow(nomes_seccoes)) {
      vectemp<-c()
      for (j in 1:length(nomessplit)) {
        # if (length(nomessplit[[j]]==1)) { #acho que o parentises do if estava no sitio errado. tcoelho 20201002
        if (length(nomessplit[[j]])==1) {
          vectemp<-c(vectemp)
        } else {
          if (nomessplit[[j]][2]==as.character(nomes_seccoes[i,"Nomenclature"])) {vectemp<-c(vectemp,nomes[j])}
        }
      }
      if (!is.null(vectemp)) {nomes_lista_seccoes<-c(nomes_lista_seccoes,nomes_seccoes[i,"Names"])}
      lista_seccoes[[as.character(nomes_seccoes[i,"Name"])]]<-vectemp
    }
    
  } 
  # (structure=="25 abril")
  
  # SENSORS AND QUANTITIES PONTE EDGAR CARDOSO ----------------------------------------------------------------
  if (structure=="figueira") {
    
    #*** Load sensors and quantities list of dataframes ---------------------------------------------
    load(file=file.path(diretoria,"sensors_and_quantities.RData"))
    nomes_grandezas<-sensors_and_quantities[[structure]]$nomes_grandezas
    nomes_seccoes<-sensors_and_quantities[[structure]]$nomes_seccoes
    
    #*** Lista grandezas -----------------------------------------------------
    lista_grandezas<-list()
    nomes<-setdiff(names(dados),c("datahora","idinfo"))
    for (i in 1:nrow(nomes_grandezas)) {
      lista_grandezas[[as.character(nomes_grandezas[i,"Quantity"])]]<-nomes[which(substr(nomes,0,1)==as.character(nomes_grandezas[i,"Nomenclature"]))]
    }
    names(lista_grandezas)<-as.character(nomes_grandezas[,"Quantity"])
    
    #*** Lista seccoes -----------------------------------------------------
    lista_seccoes<-list()
    nomes<-setdiff(names(dados),c("datahora","idinfo"))
    nomessplit<-strsplit(nomes,".",fixed=T)
    nomes_lista_seccoes<-c()
    for (i in 1:nrow(nomes_seccoes)) {
      vectemp<-c()
      for (j in 1:length(nomessplit)) {
        if (length(nomessplit[[j]])==1) {
          vectemp<-c(vectemp)
        } else {
          if (nomessplit[[j]][2]==as.character(nomes_seccoes[i,"Nomenclature"])) {vectemp<-c(vectemp,nomes[j])}
        }
      }
      if (!is.null(vectemp)) {nomes_lista_seccoes<-c(nomes_lista_seccoes,nomes_seccoes[i,"Names"])}
      lista_seccoes[[as.character(nomes_seccoes[i,"Name"])]]<-vectemp
    }
    
  } 
  
  
  # RETURN ------------------------------------------------------------------
  return(list(nomes_grandezas=nomes_grandezas,nomes_seccoes=nomes_seccoes,lista_grandezas=lista_grandezas,lista_seccoes=lista_seccoes))
  
  
}