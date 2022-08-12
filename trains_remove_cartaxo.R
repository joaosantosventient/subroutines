library(signal);library(caTools);library(pracma)

trains_remove_cartaxo <- function(ac_data,threshold,sample_rate,dim_janela_movel,detrend=F,center=F) {
  #Funcao para remover os comboios do sinal e ficar apenas com vibração em regime livre
  #Esta rotina, comeca pelo fim do sinal e identifica o fim do comboio, e apaga desse ponto ate ao inicio do sinal
  #inputs:
  #-threshold <- valor de triger do comboio (em mg)
  #-sample_rate <- frequencia de aquisição
  #-dim_janela_movel <- tamanho da janela movel para calcular os maximos que permitem comparar com o threshold, para identificar o comboio
  
  
  
  #Guarda o nome das colunas
  colnames_ac_data <- colnames(ac_data)
  
  #Converter as colunas em numeric
  not_muneric_cols <- which(!apply(ac_data, 2, is.numeric)==T)# colunas nao numeric
  if(length(not_muneric_cols)!=0){
    ac_data[,not_muneric_cols] <- apply(ac_data[,not_muneric_cols],2,as.numeric)
  }
  
  # Detrend and Center ------------------------------------------------------
  if(detrend==T){ ac_data <- apply(ac_data, 2, detrend) }
  if(center==T){  ac_data <- apply(ac_data, 2, scale, scale=F)}
  
  #Inicia a varivel com e sem comboio
  ac_data_original <- ac_data
  notrain_ac_data <- ac_data
  
  
  # Identifica e remove o comboio ------------------------------------------------------
  tamanho_sinal <- 1#segundos, variavel auxiliar do ciclo while
  i_while <-0
  while(!(tamanho_sinal>=minutosaceitacaoficheiros*60 | tamanho_sinal==0)){ # o while para se a duracao sinal for superior a duracao de aceitacao ou se for igual a 0 (sinal invalido para analise)
    
    
    
    
    # ** Compara o sinal com o treshold -----------------------------------------------
    vec_maximos <- c()
    # windows()
    # par(mfrow=c(3,2))
    for(sensor_ii in colnames(notrain_ac_data)){
      sinal <- notrain_ac_data[,sensor_ii]
      maximos <- runmax(sinal,k = as.integer(dim_janela_movel*sample_rate))#vetor com os valores máximos numa janela de comprimento "dim_janela_movel"
      maximo <- tail(which(maximos>threshold),1)
      vec_maximos <- c(vec_maximos,maximo)
      
      
      # plot(sinal)
      # plot(maximos,col="red",t="p",ylim = c(0,100))
      # abline(h=threshold)
      # abline(v=maximo)
    }
    
    
    # Se nao forem identificados máximos acima do valor de acel_max, pára o ciclo while 
    if(length(vec_maximos)==0){
      # Duracao do sinal (segundos)
      tamanho_sinal <- nrow(notrain_ac_data)/samplerate
      if(tamanho_sinal<=minutosaceitacaoficheiros*60){ # se a duracao do sinal for inferior ao minimo (minutosaceitacaoficheiros) nao considera os dados
        notrain_ac_data <- data.frame()
      }
      break
    }
    
    # ** Remove o sinal do comboio -----------------------------------------------
    maximo_global <- max(vec_maximos) #fica apenas com o maior valor dos maximos identificados em cada sensor, 
    notrain_ac_data_temp <- notrain_ac_data
    notrain_ac_data_temp[1:maximo_global,] <- NA
    
    
    
    
    # windows()
    # par(mfrow=c(2,1))
    # #Sinal completo
    # plot(notrain_ac_data[,1],ylim=c(-50,50),t="l")
    # lines(notrain_ac_data[,2],col="red")
    # lines(notrain_ac_data[,3],col="green")
    # #Sinal sem comboio
    # plot(notrain_ac_data_temp[,1],ylim=c(-50,50),t="l")
    # lines(notrain_ac_data_temp[,2],col="red")
    # lines(notrain_ac_data_temp[,3],col="green")
    
    
    # Duracao do sinal sem comboio (segundos) para verificar na condicao WHILE
    # tamanho_sinal <- nrow(notrain_ac_data_temp)/samplerate
    tamanho_sinal <- length(maximo_global:nrow(notrain_ac_data_temp))/samplerate
    # Verificar tamanho do sinal editado. Se for menor que 2 minutos provavelmente apanhou um segundo comboio no fim do registo, 
    # então apaga 10 segundos ao fim do sinal para apagar o comboio do fim
    if(tamanho_sinal<minutosaceitacaoficheiros*60){
      #Verificar se o maximo estah na primeira ou segunda metade do tamanho do sinal
      if(maximo_global<nrow(ac_data)/2){
        #Maximo estah na 1a metade, nao deve haver comboio no fim do sinal
        
        tamanho_sinal <- 0
        notrain_ac_data <- data.frame()
        
      }else{
        #Maximo esta na 2a metade, eh provavel que esteja um comboio no fim do sinal. Vai-se tentar apaga-lo
        #Retira-se 5 segundos a cada iteracao
        colnames_notrain_ac_data <- names(notrain_ac_data)
        notrain_ac_data <- as.data.frame(notrain_ac_data[1:(nrow(notrain_ac_data)-samplerate*5),])
        if(ncol(notrain_ac_data)<2){names(notrain_ac_data) <- colnames_notrain_ac_data}
        
      }
      
    }else{
      notrain_ac_data <- notrain_ac_data_temp
    }
    i_while <- i_while+1
    # print(i_while)
    
    # plot(notrain_ac_data[,1])
  }
  
  if(tamanho_sinal>0){
    #Apaga linhas com NA's
    notrain_ac_data <- notrain_ac_data[apply(notrain_ac_data, 1,function(i) all(complete.cases(i))),]
    notrain_ac_data <- as.data.frame(notrain_ac_data)
    colnames(notrain_ac_data) <- colnames_ac_data
  }
  
  
  return(list(ac_data_original = ac_data_original,notrain_ac_data = notrain_ac_data))
}


