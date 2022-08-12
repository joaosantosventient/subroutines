
# dados<-dados
 # anal_modal<-c("a","p")

subset_modal<-function(dados,anal_modal,structure,sub_structure="") {
  
  # CONDICAO SOBRE EXISTENCIA DE CRITERIO PARA FAZER ANALISE MODAL-----------------------------------------
  if (anal_modal==F) {
    dadosmodal<-dados
  } else {
    dadosmodal<-dados[,which(substr(names(dados),0,1) %in% anal_modal)]
  }
  # OBTENCAO DE DIM2 e INICIALIZAO DE NUMTRUES ----------------------------------------
  dim2<-dim(dadosmodal)[2]
  num_TRUES<-0
  
  # CONDICAO SOBRE A EXISTENCIA DE SENSORES PARA ANALISE MODAL -----------------------------------------------
  if (dim2>0) {
    #*** SUBSET MODAL 25 ABRIL ----------------------------------------------------------------
    if (structure=="25 abril") {
      #****** Deck rotation dls--------------------------------------------
      if (!is.null(dadosmodal$a2v.66S) & !is.null(dadosmodal$a3v.66S)) {dadosmodal$ar.66S<-(dadosmodal$a2v.66S-dadosmodal$a3v.66S)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.22S) & !is.null(dadosmodal$a3v.22S)) {dadosmodal$ar.22S<-(dadosmodal$a2v.22S-dadosmodal$a3v.22S)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.0) & !is.null(dadosmodal$a3v.0)) {dadosmodal$ar.0<-(dadosmodal$a2v.0-dadosmodal$a3v.0)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.22N) & !is.null(dadosmodal$a3v.22N)) {dadosmodal$ar.22N<-(dadosmodal$a2v.22N-dadosmodal$a3v.22N)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a1v.66N) & !is.null(dadosmodal$a3v.66N)) {dadosmodal$ar.66N<-(dadosmodal$a1v.66N-dadosmodal$a3v.66N)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.66N) & !is.null(dadosmodal$a3v.66N)) {dadosmodal$ar.66N<-(dadosmodal$a2v.66N-dadosmodal$a3v.66N)/2; num_TRUES<-num_TRUES+1}
      #****** Deck vertical dls--------------------------------------------
      if (!is.null(dadosmodal$a2v.66S) & !is.null(dadosmodal$a3v.66S)) {dadosmodal$av.66S<-(dadosmodal$a2v.66S+dadosmodal$a3v.66S)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.22S) & !is.null(dadosmodal$a3v.22S)) {dadosmodal$av.22S<-(dadosmodal$a2v.22S+dadosmodal$a3v.22S)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.0) & !is.null(dadosmodal$a3v.0)) {dadosmodal$av.0<-(dadosmodal$a2v.0+dadosmodal$a3v.0)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.22N) & !is.null(dadosmodal$a3v.22N)) {dadosmodal$av.22N<-(dadosmodal$a2v.22N+dadosmodal$a3v.22N)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a1v.66N) & !is.null(dadosmodal$a3v.66N)) {dadosmodal$av.66N<-(dadosmodal$a1v.66N+dadosmodal$a3v.66N)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2v.66N) & !is.null(dadosmodal$a3v.66N)) {dadosmodal$av.66N<-(dadosmodal$a2v.66N+dadosmodal$a3v.66N)/2; num_TRUES<-num_TRUES+1}
      #****** Deck transverse dls--------------------------------------------
      if (!is.null(dadosmodal$a1t.66S)) {dadosmodal$ah.66S<-dadosmodal$a1t.66S; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a1t.22S)) {dadosmodal$ah.22S<-dadosmodal$a1t.22S; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a1t.0)) {dadosmodal$ah.0<-dadosmodal$a1t.0; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a1t.22N)) {dadosmodal$ah.22N<-dadosmodal$a1t.22N; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a1t.66N)) {dadosmodal$ah.66N<-dadosmodal$a1t.66N; num_TRUES<-num_TRUES+1}
      #****** Deck ground dls--------------------------------------------
      if (!is.null(dadosmodal$atx.P1)) {dadosmodal$al.P1<-dadosmodal$atx.P1; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$aty.P1)) {dadosmodal$ah.P1<-dadosmodal$aty.P1; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$atz.P1)) {dadosmodal$av.P1<-dadosmodal$atz.P1; num_TRUES<-num_TRUES+1}
      #****** P3 rotations dls--------------------------------------------
      if (!is.null(dadosmodal$a2l.P3mC) & !is.null(dadosmodal$a3l.P3jC)) {dadosmodal$ar.P3C<-(dadosmodal$a2l.P3mC-dadosmodal$a3l.P3jC)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2l.P3mD) & !is.null(dadosmodal$a3l.P3jD)) {dadosmodal$ar.P3D<-(dadosmodal$a2l.P3mD-dadosmodal$a3l.P3jD)/2; num_TRUES<-num_TRUES+1}
      #****** P3 longitudinal dls--------------------------------------------
      if (!is.null(dadosmodal$a2l.P3mC) & !is.null(dadosmodal$a3l.P3jC)) {dadosmodal$al.P3C<-(dadosmodal$a2l.P3mC+dadosmodal$a3l.P3jC)/2; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2l.P3mD) & !is.null(dadosmodal$a3l.P3jD)) {dadosmodal$al.P3D<-(dadosmodal$a2l.P3mD+dadosmodal$a3l.P3jD)/2; num_TRUES<-num_TRUES+1}
      #****** P3 horizontal dls--------------------------------------------
      if (!is.null(dadosmodal$a1t.P3mC)) {dadosmodal$ah.P3C<-dadosmodal$a1t.P3mC; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a1t.P3mD)) {dadosmodal$ah.P3D<-dadosmodal$a1t.P3mD}
      #****** P3 Groud dls --------------------------------------------
      if (!is.null(dadosmodal$a1t.P3mA)) {dadosmodal$ah.P3A<-dadosmodal$a1t.P3mA; num_TRUES<-num_TRUES+1}
      if (!is.null(dadosmodal$a2l.P3mA)) {dadosmodal$al.P3A<-dadosmodal$a2l.P3mA; num_TRUES<-num_TRUES+1}
      #*** Remove initial ones ---------------------------------------------
      if (anal_modal==F) {} else {
        dimmax<-dim2+1+num_TRUES
        if (dimmax>ncol(dadosmodal)) {dimmax<-ncol(dadosmodal)}
        dadosmodal<-dadosmodal[,(dim2+1):(dimmax)]
      }
      #*** Remove null variables ---------------------------------------------
      if (anal_modal==F) {} else {
        aremover<-c()
        for (k in 1:ncol(dadosmodal)) {aremover[k]<-length(which(abs(dadosmodal[,k])<1e-20))/dim(dadosmodal)[1]*100>25}
        dadosmodal<-dadosmodal[,!aremover]
      }
      #*** New dados ---------------------------------------------
      dados<-dados[,-which(substr(names(dados),0,1) %in% anal_modal)]
      dados<-as.data.frame(cbind(dados,dadosmodal))
      #*** Output ---------------------------------------------
      return(list(dadosmodal=dadosmodal,dados=dados))
    }
    
    
    #*** SUBSET MODAL SADO ----------------------------------------------------------------
    if (structure=="sado") {
      #*** New dados ---------------------------------------------
      dadosmodal<-dadosmodal[,setdiff(names(dadosmodal),c("al.p1","al.p2","al.p3","al.p4"))]
      dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      #*** Output ---------------------------------------------
      return(list(dadosmodal=dadosmodal,dados=dados))
    }
    
    
    #*** SUBSET MODAL CARTAXO ----------------------------------------------------------------
    if (structure=="cartaxo") {
      if (sub_structure=="modulo 2") {
        sensors <- c("AC.01","AC.02","AC.03")
        indices <- which((names(dados) %in% sensors)==T)
        dadosmodal<-data.frame(dados[,indices])
        if(ncol(dadosmodal)<2){names(dadosmodal) <- names(dados)[indices]}
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="modulo 3") {
        sensors <- c("AC.04","AC.05","AC.06")
        indices <- which((names(dados) %in% sensors)==T)
        dadosmodal<-data.frame(dados[,indices])
        if(ncol(dadosmodal)<2){names(dadosmodal) <- names(dados)[indices]}
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="modulo 4") {
        sensors <- c("AC.07","AC.08","AC.09")
        indices <- which((names(dados) %in% sensors)==T)
        dadosmodal<-data.frame(dados[,indices])
        if(ncol(dadosmodal)<2){names(dadosmodal) <- names(dados)[indices]}
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="modulo 5") {
        sensors <- c("AC.10","AC.11","AC.12")
        indices <- which((names(dados) %in% sensors)==T)
        dadosmodal<-data.frame(dados[,indices])
        if(ncol(dadosmodal)<2){names(dadosmodal) <- names(dados)[indices]}
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="modulo 6") {
        sensors <- c("AC.13","AC.14","AC.15")
        indices <- which((names(dados) %in% sensors)==T)
        dadosmodal<-data.frame(dados[,indices])
        if(ncol(dadosmodal)<2){names(dadosmodal) <- names(dados)[indices]}
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="modulo 7") {
        sensors <- c("AC.16","AC.17","AC.18")
        indices <- which((names(dados) %in% sensors)==T)
        dadosmodal<-data.frame(dados[,indices])
        if(ncol(dadosmodal)<2){names(dadosmodal) <- names(dados)[indices]}
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="modulo 8") {
        sensors <- c("AC.19","AC.20","AC.21")
        indices <- which((names(dados) %in% sensors)==T)
        dadosmodal<-data.frame(dados[,indices])
        if(ncol(dadosmodal)<2){names(dadosmodal) <- names(dados)[indices]}
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      #Clear empty rows
      colnames_dadosmodal <- names(dadosmodal)
      dadosmodal <- as.data.frame(dadosmodal[apply(dadosmodal, 1,function(i) !all(is.na(i))),])
      names(dadosmodal) <- colnames_dadosmodal
      
      
      #*** Output ---------------------------------------------
      return(list(dadosmodal=dadosmodal,dados=dados))
    }
    
    #*** SUBSET MODAL PIRAMIDES ----------------------------------------------------------------
    if (structure=="piramides") {

      if (sub_structure=="tabuleiro") {
        sensors <- c("av.1m","av.2j","av.3m")#av.2m avariado
        dadosmodal<-dados[,which((names(dados) %in% sensors)==T)] #av.2m avariado
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="TR-116M") {
        sensors <- c("a.1m")
        sensors_index <- which((names(dados) %in% sensors)==T)
        dadosmodal<-dados[,c(sensors_index)] #condicao especifica para tirantes - manha para o modal tracking aceitar
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="TS-116I") {
        sensors <- c("a.2m")
        sensors_index <- which((names(dados) %in% sensors)==T)
        dadosmodal<-dados[,c(sensors_index)] #condicao especifica para tirantes - manha para o modal tracking aceitar
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="TS-116S") {
        sensors <- c("a.3m")
        sensors_index <- which((names(dados) %in% sensors)==T)
        dadosmodal<-dados[,c(sensors_index)] #condicao especifica para tirantes - manha para o modal tracking aceitar
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="TS-117S") {
        sensors <- c("a.4m")
        sensors_index <- which((names(dados) %in% sensors)==T)
        dadosmodal<-dados[,c(sensors_index)] #condicao especifica para tirantes - manha para o modal tracking aceitar
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      #Clear empty rows
      if(is.data.frame(dadosmodal)){dadosmodal <- dadosmodal[apply(dadosmodal, 1,function(i) !all(is.na(i))),]}
      if(is.vector(dadosmodal)){dadosmodal <- dadosmodal[complete.cases(dadosmodal)]}
      
      #*** Output ---------------------------------------------
      return(list(dadosmodal=dadosmodal,dados=dados))
    }
    
    #*** SUBSET MODAL FREIXO ----------------------------------------------------------------
    if (structure=="freixo") {
      if (sub_structure=="tabuleiro montante") {
        sensors <- c("AV.1M","AT.2M","AV.3M","AV.4M","AT.5M","AV.6M","AV.7M","AT.8M")
        dadosmodal<-dados[,which((names(dados) %in% sensors)==T)] 
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      if (sub_structure=="tabuleiro jusante") {
        sensors <- c("AV.1J","AT.2J","AV.3J","AV.4J","AT.5J","AV.6J","AV.7J","AT.8J")
        dadosmodal<-dados[,which((names(dados) %in% sensors)==T)]
        dados<-dados[,which(!names(dados) %in% names(dadosmodal))]
      }
      
      dadosmodal <- dadosmodal[apply(dadosmodal, 1,function(i) !all(is.na(i))),]
      
      #*** Output ---------------------------------------------
      return(list(dadosmodal=dadosmodal,dados=dados))
    }
    
    
    
    
    

  } else {
    #*** output ---------------------------------------------
    return(list(dadosmodal=NULL,dados=dados))
  }
  
  
}