bilinreg<-function(df,start=2,step=1,stop=nrow(df),col_input=1,col_output=2) {
  
  pred_list<-list(); pred_error<-x_intersect<-c()
  for (k in start:(stop-1)) {
    
    input_lm_left<-df[start:(k),col_input]
    output_lm_left<-df[start:(k),col_output]
    (lm_left<-lm(formula=as.formula("output_lm_left~input_lm_left+0")))
    
    input_lm_right<-df[(k+1):stop,col_input]
    output_lm_right<-df[(k+1):stop,col_output]
    (lm_right<-lm(formula=output_lm_right~input_lm_right))
    
    (x_intersect[k]<-(as.numeric(lm_right$coefficients[1])-0)/((as.numeric(lm_left$coefficients[1])-as.numeric(lm_right$coefficients[2]))))
    x_left<-c((start:stop)[which((start:stop)<x_intersect[k])],x_intersect[k])
    x_right<-c(x_intersect[k],(start:stop)[which((start:stop)>x_intersect[k])])
    
    (pred_left<-as.numeric(lm_left$coefficients[1])*x_left)
    pred_right<-as.numeric(lm_right$coefficients[1])+as.numeric(lm_right$coefficients[2])*x_right
    
    pred_list[[k]]<-as.data.frame(rbind(cbind(x_left,pred_left),cbind(x_right,pred_right)))
    
    pred_error[k]<-sum(sqrt((lm_left$residuals)^2))+sum(sqrt((lm_right$residuals)^2))
    
  }
  
  return(list(pred_list=pred_list,pred_error=pred_error,x_intersect=round(x_intersect)))
  
}