#Apply sensores limits from limites_funcionamento to estatico.Rdata as outliers

apply_sensors_limits <- function(nome_obra,estatico){
  
  get_sensors <- function(data){colnames(data)[-c(which(colnames(data)=="datahora"),
                                                  which(colnames(data)=="idinfo"),
                                                  which(substr(colnames(data),1,3)=="out"),
                                                  which(substr(colnames(data),1,4)=="orig"),
                                                  which(substr(colnames(data),1,6)=="offset"),
                                                  which(substr(colnames(data),1,6)=="factor"))]}
  
  load("//de-noe-comp1/shm/shm_data_auxi/matriz_obras.RData")#carrega a matriz das obras
  load("//de-noe-comp1/shm/shm_data_auxi/matriz_sensores.RData")#carrega a matriz dos sensores
  matriz_sensores <- as.data.frame(matriz_sensores)
  nome_obra <- matriz_obras[which(matriz_obras$Path==nome_estrutura),"Nome"]
  load("//de-noe-comp1/shm/shm_data_auxi/limites_funcionamento.RData")#carrega a lista dos limites dos sensores
  limites_sensores_DF <- as.data.frame(limites_funcionamento[[nome_obra]])
  limites_sensores_DF[,c("Limite inferior","Limite superior")] <- apply(limites_sensores_DF[,c("Limite inferior","Limite superior")], 2, as.numeric)
  
  
  linhas_a_manter <- apply(limites_sensores_DF[,c("Limite inferior","Limite superior")], 1, function(i) !all(is.na(i)))
  limites_sensores_DF <- limites_sensores_DF[linhas_a_manter,]
  
  
  if(!is.null(limites_sensores_DF)){
    if(nrow(limites_sensores_DF)>0){
      
      for(i in 1:nrow(limites_sensores_DF)){
        for(limite in c("Limite inferior","Limite superior")){
          if(is.numeric(limites_sensores_DF[i,limite])){
            valor_limite <- limites_sensores_DF[i,limite]
            grupo_sensores_ID <- as.character(limites_sensores_DF$ID[i])
            sensores <- get_sensors(estatico)
            
            #Determinar o prefixos dos sensores (tipos de sensor)
            sensores_substr <- gsub("[^A-z]","", #Retira apenas as letras para corresponder ao tipo de sensor
                                    sapply(strsplit(sensores, "[.]"), `[[`, 1)) #Faz substr ate ao ponto "."
            
            #quais os sensores do estatico que pertecem ao grupo_sensores_ID
            matriz_compt_i <- subset(matriz_sensores, ID == grupo_sensores_ID,)[4:ncol(matriz_sensores)]
            matriz_compt_i <- as.data.frame(apply(matriz_compt_i, 1, as.character))
            sensores_grupo <- sensores[which(tolower(sensores_substr) %in% as.character(matriz_compt_i[,1]))]
            
            #Aplicar outliers aos valores fora dos limites
            for(sensor_j in sensores_grupo){
              
              coluna_outlier <- paste0("out_",sensor_j)
              
              #Veriricar se existem valores fora dos limites
              if(limite=="Limite inferior"){
                if(is.element(coluna_outlier, colnames(estatico))){
                  indices_outliers <- which(estatico[[sensor_j]] < valor_limite & is.na(estatico[[coluna_outlier]]))
                }else{
                  indices_outliers <- which(estatico[[sensor_j]] < valor_limite)    
                }
              }else if (limite=="Limite superior"){
                if(is.element(coluna_outlier, colnames(estatico))){
                  indices_outliers <- which(estatico[[sensor_j]] > valor_limite & is.na(estatico[[coluna_outlier]]))  
                }else{
                  indices_outliers <- which(estatico[[sensor_j]] > valor_limite)    
                }
              }
              
              if(length(indices_outliers)!=0){
                1
                1
                #Verificar se a coluna de outlier existe, senao cria-a
                if(!is.element(coluna_outlier, colnames(estatico))){
                  outliers_DF <- data.frame(matrix(NA,nrow = dim(estatico)[1],ncol = 1))
                  colnames(outliers_DF) <- coluna_outlier
                  estatico <- cbind(estatico,outliers_DF)
                }
                #Atribuir o valor de 1 ah coluna outlier
                estatico[indices_outliers,coluna_outlier] <- 1
              }
              rm(indices_outliers,coluna_outlier,outliers_DF)
            }
          }
        }
      }
    }
  }
  
  return(estatico)
}
