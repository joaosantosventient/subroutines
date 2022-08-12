moving_average<-function(x,n){
                cx<-c(cumsum(c(0,x)))
                cn<-c(0:length(cx))
                rx<-cx[(n+1):length(cx)]-cx[1:(length(cx)-n)]
                rn<-cn[(n+1):length(cx)]-cn[1:(length(cx)-n)]
                rsum<-rx/rn
                
                if ((length(x)-length(rsum)) %% 2 == 0) {
                    x<-c(rep(NA,(length(x)-length(rsum))/2),rsum,rep(NA,(length(x)-length(rsum))/2))
                } else {
                    x<-c(rep(NA,ceiling((length(x)-length(rsum))/2)),rsum,rep(NA,floor((length(x)-length(rsum))/2)))
                }
}



#moving_average<-function(x,n){filter(x,rep(1/n,n),sides=2)}