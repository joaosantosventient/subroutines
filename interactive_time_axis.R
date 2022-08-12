
interactive_time_axis<-function(datahora,rangex=NULL,axisposition=1) {

  if (is.null(rangex[1]) | is.null(rangex[2])) {
    rangedatetime<-difftime(range(datahora,na.rm=T)[2],range(datahora,na.rm=T)[1],units="secs",tz="GMT")
    atdatahora<-seq(range(datahora,na.rm=T)[1],range(datahora,na.rm=T)[2],by=difftime(range(datahora,na.rm=T)[2],range(datahora,na.rm=T)[1],units="secs",tz="GMT")/20)
  } else {
    atdatahora<-seq(rangex[1],rangex[2],by=(rangex[2]-rangex[1])/20)
    atdatahora<-as.POSIXct(atdatahora,format="%Y-%m-%d %H:%M:%OS",origin="1970-01-01 00:00:00",tz="GMT")
  }

  labelsdatahora<-substr(as.character(atdatahora),1,16)

  eixo<-axis(axisposition,at=atdatahora,las=2,labels=labelsdatahora)
  return(eixo)
}






