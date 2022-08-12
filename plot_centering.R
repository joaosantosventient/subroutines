plot_centering<-function(remove_offset,input_variables,dadostoplot) {
  
  if (remove_offset=="disabled") {
    dadostoplot<-dadostoplot
  }
  if (remove_offset=="scaled") {
    if (length(input_variables)>1) {
      for (i in 1:length(input_variables)) {
        complete<-dadostoplot[complete.cases(dadostoplot[,i]),i]
        dadostoplot[,i]<-(dadostoplot[,i]-complete[1])/sd(complete)
      }
      dadostoplot<-as.data.frame(dadostoplot)
      names(dadostoplot)<-input_variables
    } else {
      dadostoplot<-as.numeric(dadostoplot)
      complete<-dadostoplot[complete.cases(dadostoplot)]
      dadostoplot<-(dadostoplot-complete[1])/sd(complete)
    }
  }
  if (remove_offset=="non_scaled") {
    if (length(input_variables)>1) {
      for (i in 1:length(input_variables)) {
        complete<-dadostoplot[complete.cases(dadostoplot[,i]),i]
        dadostoplot[,i]<-(dadostoplot[,i]-complete[1])
      }
      dadostoplot<-as.data.frame(dadostoplot)
      names(dadostoplot)<-input_variables
    } else {
      dadostoplot<-as.numeric(dadostoplot)
      complete<-dadostoplot[complete.cases(dadostoplot)]
      dadostoplot<-dadostoplot-complete[1]
    }
  }
  
  
  return(dadostoplot)
}



