plot_scatter_plotly<-function(input_variables,dadostoplot,nomes_grandezas=NA) {

  options(warn=-1)
    
  if (is.na(nomes_grandezas)) {
    units<-rep("",length(input_variables))
  } else {
    units<-c()
    for (i in 1:length(input_variables)) {units[i]<-paste0("(",nomes_grandezas$Unit[nomes_grandezas$Nomenclature %in% substr(input_variables[i],1,1)],")")}
  }
  
  if (length(input_variables)<2) {
    p<-plot_ly(x=dadostoplot,y=dadostoplot,type="scattergl",mode="markers",name=paste(input_variables,units),marker=list(size=3))
    p<-layout(p,xaxis=list(title=paste(input_variables,units)),yaxis=list(title=paste(input_variables,units)))
  }
  if (length(input_variables)>=2) {
    p<-plot_ly(x=dadostoplot[,1],y=dadostoplot[,2],type="scattergl",mode="markers",name=paste(input_variables[2],units[2]),marker=list(size=3))
    p<-layout(p,xaxis=list(title=paste(input_variables[1],units[1])),yaxis=list(title=""))
    if (length(input_variables)>2) {
      for (i in 3:length(input_variables)) {
        p<-add_trace(p,x=dadostoplot[,1],y=dadostoplot[,i],name=paste(input_variables[i],units[i]))
      }
    }
  }
  return(p)
    
  options(warn=0)
  
}