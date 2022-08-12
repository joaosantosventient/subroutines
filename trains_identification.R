


trains_identification<- function(ac_data,lag_window,threshold,train_time,sample_rate) {
  #Funcao para identificar a passagem dos comboios
  #inputs:
  #-ac_data <- dataframe ou vector de acelerações (em mg)
  #-lag_window <- janela (em segundos) que precorre o sinal à procura de maximos
  #-threshold <- valor de triger do comboio (em mg)
  #-train_time <- tempo medio (em segundos) da passagem do comboio
  #-sample_rate <- frequencia de aquisição
  
  #variaveis iniciais
  trains_index_all_ac <- c()
  
  #condições para avaliar se é um vector ou um data.frame 
  if(!is.data.frame(ac_data)&!is.vector(ac_data)){print("erro ac_data não é um dataframe ou um vector");return(NULL)}
  if(is.data.frame(ac_data)){ncol_ac_data <- ncol(ac_data);if(!nrow(ac_data)>1){print("erro nrow(ac_data)<=1");return(NULL)}} 
  if(is.vector(ac_data)){ncol_ac_data <- 1;if(!length(ac_data)>1){print("erro length(ac_data)<=1");return(NULL)}}
  
  #ciclo for para correr todos os acelerometros do data.frame, no caso de ser um vetor nclo_ac_data <- 1
  for (acel_i in 1:ncol_ac_data){
    if(is.data.frame(ac_data)){ac_data_i <- abs(ac_data[,acel_i])}else{ac_data_i <- abs(ac_data)} #transforma o sinal do acelerometro em absoluto
    #determina o numero de janelas a efetuar, caso o numero de janelas seja inferior a 4 não corre
    lag_number <- 1:(length(ac_data_i)/(lag_window*sample_rate)) 
    if(length(lag_number)<4){print("lag_window muito grande, apenas permite identificar 3 comboios por hora");return(NULL)}
    #Inicia variaveis para os ciclos for seguintes
    y_max <- NULL
    peak_index <- c()
    trains_index <- c()
    #Ciclo para correr todas as janelas e identifica se os picos maximos de cada janela ultrapassam o threshold
    for (i in lag_number){
      if (i==1){y_max <- max(ac_data_i[1:lag_window*sample_rate]);if(y_max>=threshold){peak_index <- c(peak_index,which(ac_data_i==y_max))}}
      if(i==tail(lag_number,1)){y_max <- max(ac_data_i[(lag_window*sample_rate*(i)):(length(ac_data_i))]);if(y_max>=threshold){peak_index <- c(peak_index,which(ac_data_i==y_max))}}
      if(i!=1&i!=tail(lag_number,1)){y_max <- max(ac_data_i[(lag_window*sample_rate*(i-1)):(lag_window*sample_rate*i)]);if(y_max>=threshold){peak_index <- c(peak_index,which(ac_data_i==y_max))}}
    }
    #ciclo para percorrer todos os picos identificados e retirar antes e depois o train_time/2 
    if(!is.null(peak_index)){
      for(i in 1:length(peak_index)){
        train_index_i <- (peak_index[i]-(train_time/2*sample_rate)):(peak_index[i]+(train_time/2*sample_rate))
        trains_index <-c(trains_index,train_index_i)
      }
      # Se for um data.frame faz join de todos os indices de comboios a remover do sinal
      if(is.data.frame(ac_data)){if(length(trains_index_all_ac)>1) {trains_index_all_ac<- c(trains_index_all_ac, trains_index)}else{trains_index_all_ac <- trains_index}}
      
    }
  }
  trains_index_all_ac <- unique(trains_index_all_ac)
  return(trains_index_all_ac)
}


# Teste para a funcao

# ac_data<-dados[,which(substr(names(dados),0,1)=="a")]
# lag_window <- 60 #seconds
# threshold <- 1 #mg
# train_time <- 200 #seconds
# sample_rate <- 50 #Hz
# 
# result2 <- trains_identification(ac_data,lag_window,threshold,train_time,sample_rate)
# # 
# plot(ac_data$avm.t2m,pch=".",col="white")
# for(i in 1:ncol(ac_data)){points(ac_data[,i],col=i,pch=".")}
# abline(v=c(head(result2),tail(result2)))
