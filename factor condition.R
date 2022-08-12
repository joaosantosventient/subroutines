
#Verificar se existem fatores de escala nas ultimas leituras e aplicar esse valor aos novos dados
sensores_factor <- colnames(estatico)[which(substr(colnames(estatico),1,6)=="factor")]
if(length(sensores_factor>0)){
  #Adiciona o valor do fator de escala de cada sensor na respetiva coluna "factor_..."
  for(sensor_i in sensores_factor){
    factor <- tail(na.omit(estatico[,sensor_i]),1)
    for (iii in 1:length(dados$datahora)) {
      index_datahora_dados_no_estatico <- which(estatico$datahora==dados$datahora[iii])
      estatico[index_datahora_dados_no_estatico,sensor_i] <- factor
    }
  }
}