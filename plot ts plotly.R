plot_ts_plotly<-function(input_variables,datetimetoplot,dadostoplot,nomes_grandezas=NA) {
  
  options(warn=-1)
  
  if (is.na(nomes_grandezas)) {
    units<-rep("",length(input_variables))
  } else {
    units<-c()
    for (i in 1:length(input_variables)) {units[i]<-paste0("(",nomes_grandezas$Unit[nomes_grandezas$Nomenclature %in% substr(input_variables[i],1,1)],")")}
  }
  
  
  if (length(input_variables)>1) {
    p<-plot_ly(x=datetimetoplot,y=dadostoplot[,1],type="scattergl",mode="lines",name=paste(input_variables[1],units[1]))
    p<-layout(p,xaxis=list(title="Date/Time"),yaxis=list(title=""))
    for (i in 2:length(input_variables)) {
      p<-add_trace(p,x=datetimetoplot,y=dadostoplot[,i],name=paste(input_variables[i],units[i]))
    }
  } else {
    p<-plot_ly(x=datetimetoplot,y=dadostoplot,type="scattergl",mode="lines",name=paste(input_variables,units))
    p<-layout(p,xaxis=list(title="Date/Time"),yaxis=list(title=paste(input_variables,units)))
  }
  return(p)
  
  options(warn=0)
  
}