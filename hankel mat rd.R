

hankelmatrd<-function(dadosmat,order) {
  
	mat3list<-list()
	for (i in 1:(2*order-1)) {
		mat3list[[i]]<-dadosmat[i,,]
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
	# print(dim(hankelmat))
	return(hankelmat)
}


