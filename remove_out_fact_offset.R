#Subrotina chamada nas rotinas de processamento


#*** Remove outliers identified in shiny and add offsets --------------------------------
outlier_columns<-names(estatico)[which(substr(names(estatico),0,3)=="out")]
offset_columns<-names(estatico)[which(substr(names(estatico),0,6)=="offset")]
factor_columns<-names(estatico)[which(substr(names(estatico),0,6)=="factor")]
data_columns<-setdiff(names(estatico),c(outlier_columns,offset_columns,factor_columns))

#****** Offsets -----------------------------------------------------------------
if(length(offset_columns)!=0){    
  data_com_offsets<-data_columns[data_columns %in% substr(offset_columns,8,100)]
  data_com_offsets<-sort(data_com_offsets)
  offset_columns<-sort(offset_columns)
  
  #Corre os varios tipos de idinfo
  estatico_clean <- data.frame()
  for(i_idinfo in levels(factor(estatico$idinfo))){
    estatico_subset <- subset(estatico,idinfo==i_idinfo)#filtra os estatico de cada idinfo
    offset_columns<-names(estatico_subset)[which(substr(names(estatico_subset),0,6)=="offset")]
    offset_columns<-sort(offset_columns)
    for (i in 1:length(data_com_offsets)) {
      estatico_subset[is.na(estatico_subset[,offset_columns[i]]),offset_columns[i]]<-0
      vec_corrigir<-cumsum(estatico_subset[,offset_columns[i]])
      step<-1; vec_corrigir<-c(rep(vec_corrigir[1],step),vec_corrigir[1:(length(vec_corrigir)-step)])
      estatico_subset[,data_com_offsets[i]]<-estatico_subset[,data_com_offsets[i]]-vec_corrigir
    }
    estatico_clean <- rbind(estatico_subset,estatico_clean)
  }
  estatico <- estatico_clean[order(estatico_clean$datahora),] #sort by date
}

#****** Factors -------------------------------------------------------------
if(length(factor_columns)!=0){
  data_com_factors<-data_columns[data_columns %in% substr(factor_columns,8,100)]
  data_com_factors<-sort(data_com_factors)
  for (i in 1:length(data_com_factors)) {
    if(is.element(paste0("factor_",data_com_factors[i]),colnames(estatico))){
      coluna_factor <- paste0("factor_",data_com_factors[i])
      x <- data.frame(cbind(estatico[,data_com_factors[i]],as.numeric(estatico[,coluna_factor])))
      estatico[,data_com_factors[i]] <- apply(x, 1, function(i){
        if(all(is.na(i)) | is.na(i[1])){
          NA 
        }else{
          prod(as.numeric(i), na.rm=T)
        }
      })
    }
  }
}

#****** Outliers ----------------------------------------------------------------
if(length(outlier_columns)!=0){
  data_com_outliers<-data_columns[data_columns %in% substr(outlier_columns,5,100)]
  for (i in 1:length(data_com_outliers)) {estatico[which(!is.na(estatico[,paste0("out_",data_com_outliers)[i]])),data_com_outliers[i]]<-NA}
}

#*** Remove outliers identified in shiny and add offsets --------------------------------
if(length(outlier_columns)!=0){estatico<-estatico[,which(substr(names(estatico),0,3)!="out")]}
if(length(offset_columns)!=0){estatico<-estatico[,which(substr(names(estatico),0,6)!="offset")]}
if(length(factor_columns)!=0){estatico<-estatico[,which(substr(names(estatico),0,6)!="factor")]}

#*** Apaga colunas de sensores que estejam avariados em todo o periodo dos dados, ou seja, NA em todas as linhas das colunas "out_sensor"-----------------------------------------------------------------
col_NA <- c()
for(i in 1:ncol(estatico)){if(all(is.na(estatico[,i]))){col_NA <- c(col_NA,i)}}
if(!is.null(col_NA)){estatico <- estatico[,-col_NA]}
