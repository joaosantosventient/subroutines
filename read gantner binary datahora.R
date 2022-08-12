


readbin_gantner_datahora<-function(directoria,file,header_master,header_slave,indice_inicio,decim_factor,indice_fim) {

  library(R.utils)
  inteiros<-c("signedint8","unsignedint8","signedint16","unsignedint16","signedint32","unsignedint32","signedint64","unsignedint64")
  reais<-c("float","double")
  data<-file(paste0(directoria,file),"rb")
  if (any(header_slave$dacttimedatatype==inteiros)) {tipo<-"integer"}
  if (any(header_slave$dacttimedatatype==reais)) {tipo<-"double"}
  timestamp<-readBinFragments(data,tipo,idxs=seq(1+(header_slave$channelcount+1)*(indice_inicio-1),1+(header_slave$channelcount+1)*(indice_fim-1),by=(header_slave$channelcount+1)*decim_factor),
                              size=header_slave$dacttimebytelen,endian=header_slave$endianness,origin="start")
  close(data)
  options(digits=12); options(digits.secs=12)
  timestamp<-timestamp*header_slave$dacttimetosecondfactor
  starttime<-header_master$starttime*header_slave$starttimetodayfactor*86400
  datahora<-as.POSIXlt(starttime+timestamp,origin="1899-12-30 00:00:00.000000",tz="GMT")
  return(datahora)

}


