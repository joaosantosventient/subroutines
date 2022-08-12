# 
# dados=ssiband
# colinit=min(acelpositions,na.rm=T)
# colend=max(acelpositions,na.rm=T)


macmat <- function(dados,colinit,colend=dim(dados)[2]){
  # modos
  modos<-t(as.matrix(dados[,colinit:colend]))
  # remover
  colaemover<-c()
  for (i in 1:nrow(modos)) {
    colaemover[i]<-all(is.na(modos[i,]))
  }
  modos<-modos[!colaemover,]
  # transpose modos
  transpmodos<-t(modos)
  isnatranspmodos<-is.na(transpmodos)
  transpmodos[isnatranspmodos]<-0
  # conjugate modos
  modosconj<-matrix(complex(real=Re(modos),imaginary=-Im(modos)),nrow=dim(modos)[1],ncol=dim(modos)[2])
  isnamodosconj<-is.na(modosconj)
  modosconj[isnamodosconj]<-0
  #mac
  numerador<-(transpmodos%*%modosconj)
  diagonal<-diag(numerador)
  denominador<-numerador
  for (i in 1:length(diagonal)) {denominador[,i]<-diagonal[i]*diagonal}
  mac<-(Mod(transpmodos%*%modosconj))^2/denominador
  mac<-Re(mac)
  
  return(mac)
  
}


# 
# macmat <- function(dados,colinit){
#   dados <- as.matrix(dados)
#   dados <- apply(dados,c(1,2),as.numeric)
#   dim1<-dim(dados)[1]
#   dim2<-dim(dados)[2]
#   MAC_matrix<-matrix(NA,nrow=dim1,ncol=dim1)
#   vec3 <- rowSums(dados[,colinit:dim2]*dados[,colinit:dim2])
#   for(i_mac in 1:dim1){          
#     vec1 <- rowSums(t(dados[i_mac,colinit:dim2]*t(dados[,colinit:dim2])))^2
#     vec2 <- rep.int(sum(dados[i_mac,colinit:dim2]*dados[i_mac,colinit:dim2]),dim1)
#     MAC_matrix[,i_mac] <- vec1/(vec2*vec3)
#     # if (i_mac%%100==0) {print(i_mac)}
#   };rm(i_mac,dados)
#   return(MAC_matrix)
# }


# macmat <- function(dados,colinit){
#   modos<-t(as.matrix(dados[,colinit:dim(dados)[2]]))
#   modosconj<-matrix(complex(real=Re(modos),imaginary=-Im(modos)),nrow=dim(modos)[1],ncol=dim(modos)[2])
#   numerador<-Mod((t(modosconj)%*%modos)^2)
#   diagonal<-sqrt(diag(numerador))
#   denominador<-numerador
#   for (i in 1:length(diagonal)) {denominador[,i]<-diagonal[i]*diagonal}
#   mac<-numerador/denominador
#   return(mac)
# }



# macmat <- function(dados,colinit,colend=dim(dados)[2]){
#   modos<-t(as.matrix(dados[,colinit:colend]))
#   modosconj<-matrix(complex(real=Re(modos),imaginary=-Im(modos)),nrow=dim(modos)[1],ncol=dim(modos)[2])
#   numerador<-Mod((t(modos)%*%modosconj)^2)
#   diagonal<-sqrt(diag(numerador))
#   denominador<-numerador
#   for (i in 1:length(diagonal)) {denominador[,i]<-diagonal[i]*diagonal}
#   mac<-numerador/denominador
#   return(mac)
# }


# macmat <- function(dados,colinit,colend=dim(dados)[2]){
#   modos<-t(as.matrix(dados[,colinit:colend]))
#   modosconj<-matrix(complex(real=Re(modos),imaginary=-Im(modos)),nrow=dim(modos)[1],ncol=dim(modos)[2])
#   numerador<-(Mod((t(modos))%*%modosconj))^2
#   denominador<-(t((t(modos))%*%modosconj))%*%((t(modos))%*%modosconj)
#   mac<-numerador/denominador
#   return(mac)
#   # return(list(mac=mac,numerador=numerador,denominador=denominador))
# }

# macmat <- function(dados,colinit,colend=dim(dados)[2]){
#   modos<-as.matrix(dados[,colinit:colend])
#   mac<-matrix(nrow=nrow(modos),ncol=nrow(modos))
#   denominador<-
#   for (nn in 1:nrow(modos)) {
#     for (kk in 1:nrow(modos)) {
#       mac[nn,kk]<-(Mod(sum(modos[nn,]*modos[kk,]))^2)/((sum(modos[nn,]*modos[nn,]))*(sum(modos[kk,]*modos[kk,])))
#     }
#     # print(nn)
#   }
#   return(mac)
# }