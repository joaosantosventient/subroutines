#Define sensores avariados ou desligados como outliers
# esta subrotina eh chamada nas rotinas de preprocessamento.R

load("//de-noe-comp1/shm/shm_data_auxi/matriz_obras.RData")#carrega a lista dos sensores com avaria
nome_obra <- matriz_obras[which(matriz_obras$Path==nome_estrutura),"Nome"]
load("//de-noe-comp1/shm/shm_data_auxi/lista_sensores_avariados.RData")#carrega a lista dos sensores com avaria
sensores_avariados_DF <- lista_sensores_avariados[[nome_obra]]


if(!is.null(sensores_avariados_DF)){
  if(dim(sensores_avariados_DF)[1]>0){
    indices <- data.frame()
    #Quais dos sensores avariados estao presentes no estatico
    sensores_2_edit <-sensores_avariados_DF[,"Sensor"][sensores_avariados_DF[,"Sensor"] %in% names(estatico)]
    if(length(sensores_2_edit)!=0){
      for (row_index in 1:length(sensores_2_edit)) {
        indices<- which(estatico$datahora>as.POSIXct(sensores_avariados_DF[row_index,"Data"]))
        if(length(indices)!=0){
          if(is.element(paste0("out_",sensores_avariados_DF[row_index,"Sensor"]),colnames(estatico))){
            estatico[indices,paste0("out_",sensores_avariados_DF[row_index,"Sensor"])] <- 1
          } else {
            outliers <- matrix(ncol = 1,nrow = nrow(estatico))
            colnames(outliers) <- paste0("out_",sensores_avariados_DF[row_index,"Sensor"])
            outliers[indices,paste0("out_",sensores_avariados_DF[row_index,"Sensor"])] <- 1
            outliers2 <- data.frame(outliers)
            estatico <- cbind(estatico,outliers2)
            rm(outliers,outliers2)
          }
        }
      }
    }
  }
}