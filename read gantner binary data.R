

readbin_gantner_data<-function(directoria,file,header_master,header_slave,decim_factor,indice_inicio,indice_fim,id) {
  
  library(R.utils)
  inteiros<-c("signedint8","unsignedint8","signedint16","unsignedint16","signedint32","unsignedint32","signedint64","unsignedint64")
  reais<-c("float","double")
  data<-file(paste0(directoria,file),"rb")
  data_names<-header_slave$varname[which(substr(header_slave$varname,0,1) %in% id)]
  if (length(data_names)!=0) {
    for (ijij in 1:length(data_names)) {
      # print(data_names[ijij])
      col_index<-min(which(header_slave$varname==data_names[ijij]))
      if (any(header_slave$datatype[col_index]==inteiros)) {tipo<-"integer"}
      if (any(header_slave$datatype[col_index]==reais)) {tipo<-"double"}
      fragments_temp<-readBinFragments(data,tipo,idxs=seq(1+col_index+(header_slave$channelcount+1)*(indice_inicio-1),1+col_index+(header_slave$channelcount+1)*(indice_fim-1),by=(header_slave$channelcount+1)*decim_factor),
                                               size=header_slave$databytelen[col_index],endian=header_slave$endianness,origin="start")        
      if (ijij==1) {dados_gant<-fragments_temp} else {dados_gant<-as.data.frame(cbind(dados_gant,fragments_temp))}
    }
    names(dados_gant)<-data_names
  } else {
    dados_gant<-NA
  }
  close(data)
  return(dados_gant)
}