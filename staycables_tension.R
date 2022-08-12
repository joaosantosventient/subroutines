# # 
# mass <- 43.36#kg/m
# length <- 32.59 #m
# area <- 37*150*10^-6 #m2
# E <- 195*10^9 #Pa
# inertia <- 1790.734*10^-12
# dados <- modal_ii


force_cable_estimator<-function(dados,mass,length,area,E,inertia) {
  
  # Define os dataframes 
  f <- dados$f
  n <- dados$modo
  m <- rep(mass,nrow(modal_ii))
  L <- rep(length,nrow(modal_ii))
  A <- rep(area,nrow(modal_ii))
  I <- rep(inertia,nrow(modal_ii))
  E <- rep(E,nrow(modal_ii))
  g <- rep(9.81,nrow(modal_ii)) #m/s^2
  
  # Funçao para definir as variaveis que são para ajustar - link https://stats.stackexchange.com/questions/171154/fitting-known-equation-to-data
  newFunc <- function(n,L,tension,g,m,A,I,E){
    Le <- L*(1+(((m*g*L)/tension)^2)/8)
    lambda2 <- ((((m*g*L)/tension)^2)*L*E*A)/(tension*Le)
    miu <- rep(0,length(n))
    miu[which(n==1)] <-lambda2[1]
    zeta <- L*(tension/(E*I))^0.5
    alfa <- 1+0.039*miu
    betan <- 1+(2/zeta)+((4+(n^2*pi^2)/2))/(zeta^2)
    fcv <- (n/(2*L))*(tension/m)^0.5
    
    ((alfa*betan-0.24*(miu/zeta)))*fcv
  }
  
  #Ajustar T em função dos dados
  fit <- nls(f~newFunc(n=n,L=L,tension,g=g,m=m,A=A,I=I,E=E),start = list(tension=2000))
  
  #calculo da tensao
  tensao <- coef(fit)/1000
  
  #data.frame results
  results <- as.data.frame(cbind(datahora=as.character(unique(dados$datahora)),tension=as.character(tensao)))
  results$datahora <- as.POSIXct(results$datahora,tz="GMT")
  results$tension <- round(as.numeric(as.character(results$tension)),1)
  # results$R <- round(as.numeric(as.character(results$R)),2)
  
  
  # Fitted values
  f_fitted <- rep(NA,nrow(dados))
  t_adjus <- rep(tensao*1000,length(n))
  for(i in n){f_fitted[i] <- newFunc(n=n[i],L=L[i],t=t_adjus[i],g=g[i],m=m[i],A=A[i],I=I[i],E=E[i])}
  R2 <- (sum((f_fitted-mean(f,na.rm=T))^2))/((sum((f_fitted-mean(f,na.rm=T))^2))+sum((f-f_fitted)^2))

  results$R2 <- R2
  
  return(results)
}



# 
# 
# plot(n,f)
# f_fitted <- nrow(dados)
# t_adjus <- rep(tensao*1000,length(n))
# for(i in n){
#   f_fitted[i] <- newFunc(n=n[i],L=L[i],t=t_adjus[i],g=g[i],m=m[i],A=A[i],I=I[i],E=E[i])
# }
# points(n,f_fitted,col="blue",pch=19)



