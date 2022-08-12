

hankelmatcov<-function(dadosmat,order) {
  
  dadosmat<-as.matrix(dadosmat)
  
	mat1list<-mat2list<-mat3list<-list()
	for (i in 1:(2*order-1)) {
		mat1list[[i]]<-dadosmat[i:(dim(dadosmat)[1]),]
		mat2list[[i]]<-dadosmat[1:(dim(dadosmat)[1]-i+1),]
		mat3list[[i]]<-cor(mat1list[[i]],mat2list[[i]])
	}
  ## vou aqui
	mat3listlist<-mat4list<-list()
	for (i in 1:order) {
		mat3listtemp<-list()
		for (j in 1:order) {
			mat3listtemp[[j]]<-mat3list[[i+j-1]]
		}
		mat3listlist[[i]]<-mat3listtemp
		mat4temp<-mat3listtemp[[1]]
		for (j in 2:order) {
			mat4temp<-rbind(mat4temp,mat3listtemp[[j]])
		}
		mat4list[[i]]<-t(mat4temp)
	}
	hankelmat<-mat4list[[1]]
	for (i in 2:order) {
		hankelmat<-rbind(hankelmat,mat4list[[i]])
	}
	hankelmat<-t(hankelmat)
	return(hankelmat)
}


