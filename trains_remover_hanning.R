library(signal)

trains_remover_hanning<- function(ac_data,lag_window,threshold,train_time,sample_rate,hanning_dim) {
  #Funcao para remover a passagem dos comboios e para aplicar a janela de hanning às extremidades dos dados sem comboios
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
    if(is.data.frame(ac_data)){ac_data_i <- abs(scale(ac_data[,acel_i],center = T,scale = F))}else{ac_data_i <- abs(scale(ac_data,center = T,scale = F))} #transforma o sinal do acelerometro em absoluto
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
        if(any(trains_index<1)){trains_index <- trains_index[which(trains_index>0)]} #condiição para remover indices negativos
      }
      # Se for um data.frame faz join de todos os indices de comboios a remover do sinal
      if(is.data.frame(ac_data)){if(length(trains_index_all_ac)>1) {trains_index_all_ac<- c(trains_index_all_ac, trains_index)}else{trains_index_all_ac <- trains_index}}
      if(is.vector(ac_data)){trains_index_all_ac <- trains_index}
    }
  }
  
  #retira os comboio e aplica as janelas de hanning nas extremidades
  if(length(trains_index_all_ac)>1){
    #ordena e remove os indices duplicados
    trains_index_all_ac <- sort(unique(trains_index_all_ac))
    ac_data_no_trains <- ac_data
    
    #se ac_data for um data.frame
    if(is.data.frame(ac_data)){
      #determina os indices dos pontos de extremidade para apolicar as janelas de hanning ao ac_data
      train_extremos<- c(1,
                         head(trains_index_all_ac,1),
                         trains_index_all_ac[which(diff(trains_index_all_ac)>1)],
                         trains_index_all_ac[(which(diff(trains_index_all_ac)>1)+1)],
                         tail(trains_index_all_ac,1),
                         nrow(ac_data))
      
      #ciclo para aplicar as janelas de hanning nos pontos de extremidade dos dados sem comboios
      index_to_remove <- c()
      for (i in seq(1,length(train_extremos),by=2)){
        vector_hanning <- hanning(hanning_dim*sample_rate)
        index_1a_metade_janela_hanning <- train_extremos[i]:(train_extremos[i]+((hanning_dim*sample_rate)/2)-1)
        #condicao que avalia se há espaço para aplicar a hanning windows
        if((train_extremos[i+1]-train_extremos[i])<length(vector_hanning)){
          index_to_remove <- c(index_to_remove,train_extremos[i]:train_extremos[i+1])
        }else{
          index_2a_metade_janela_hanning <- ((train_extremos[i+1])-((hanning_dim*sample_rate)/2)+1):train_extremos[i+1]
          ac_data_no_trains[index_1a_metade_janela_hanning,] <- ac_data[index_1a_metade_janela_hanning,]*vector_hanning[1:(length(vector_hanning)/2)]
          ac_data_no_trains[index_2a_metade_janela_hanning,] <- ac_data[index_2a_metade_janela_hanning,]*vector_hanning[(length(vector_hanning)/2-1):length(vector_hanning)]
        }
      }
      #remove ao ac_data os comboios e das janelas de dados sem comboios que nao tem comprimento suficiente para aplicar a hanning window
      if(!is.null(index_to_remove)){ac_data_no_trains2 <- ac_data_no_trains[-c(trains_index_all_ac,index_to_remove),]}else{ac_data_no_trains2 <- ac_data_no_trains[-trains_index_all_ac,]}
      
      #se ac_data for um vetor
    }else{
      #determina os indices dos pontos de extremidade para aplicar as janelas de hanning ao ac_data
      train_extremos<- c(1,
                         head(trains_index_all_ac,1),
                         trains_index_all_ac[which(diff(trains_index_all_ac)>1)],
                         trains_index_all_ac[(which(diff(trains_index_all_ac)>1)+1)],
                         tail(trains_index_all_ac,1),
                         length(ac_data))
      #ciclo para aplicar as janelas de hanning nos pontos de extremidade dos dados sem comboios
      index_to_remove <- c()
      for (i in seq(1,length(train_extremos),by=2)){
        vector_hanning <- hanning(hanning_dim*sample_rate)
        index_1a_metade_janela_hanning <- train_extremos[i]:(train_extremos[i]+((hanning_dim*sample_rate)/2)-1)
        #condicao que avalia se há espaço para aplicar a hanning windows
        if((train_extremos[i+1]-train_extremos[i])<length(vector_hanning)){
          index_to_remove <- c(index_to_remove,train_extremos[i]:train_extremos[i+1])
        }else{
          ac_data_no_trains[index_1a_metade_janela_hanning] <- ac_data[index_1a_metade_janela_hanning]*vector_hanning[1:(length(vector_hanning)/2)]
          ac_data_no_trains[index_2a_metade_janela_hanning] <- ac_data[index_2a_metade_janela_hanning]*vector_hanning[(length(vector_hanning)/2+1):length(vector_hanning)]
        }
      }
      #remove ao ac_data os comboios
      #remove ao ac_data os comboios e das janelas de dados sem comboios que nao tem comprimento suficiente para aplicar a hanning window
      if(!is.null(index_to_remove)){ac_data_no_trains2 <- ac_data_no_trains[-c(trains_index_all_ac,index_to_remove)]}else{ac_data_no_trains2 <- ac_data_no_trains[-trains_index_all_ac]}
    }
    
  }else{ac_data_no_trains2 <- ac_data}
  return(ac_data_no_trains2)
}

# Teste para a funcao


# ac_data<-dados[,which(substr(names(dados),0,1)=="a")]
# plot(ac_data,pch=".",col="green4",ylim = c(-0.05,0.05))
# 
# plot(ac_data$avm.t2m,pch=".",col="white")
# for(i in 1:ncol(ac_data)){points(ac_data[,i],col=i,pch=".")}
# 
# 
# lag_window <- 30 #seconds
# threshold <- 1 #mg
# train_time <- 200 #seconds
# sample_rate <- 50 #Hz
# hanning_dim <- 60#seconds
# 
# result3 <- trains_remover_hanning(ac_data,lag_window,threshold,train_time,sample_rate,hanning_dim)
# # # #
# plot(result3$avm.t2m*5,pch=".",col="white")
# for(i in 1:ncol(result3)){points(result3[,i],col=i,pch=".")}
# # abline(v=c(head(result2),tail(result2)))
# 
# # plot(result3,pch=".",col="green4")

# # ac_data <- dadosmodal
# # lag_window <- 60
# # threshold <- 40
# train_time <-  250
# # sample_rate <-  50
# # hanning_dim <-  240
# # 
# # for (acel_i in 1:ncol_ac_data){
# #   ac_data_i <- abs(scale(ac_data[,acel_i],center = T,scale = F))
# #   windows()
# #   plot(ac_data_i,pch=".")
# # }

