
mergeprepmaver<-function(diretoria_dados) {
  library(plyr)
  ls<-list.files(diretoria_dados,pattern="prep maver ")
  ls<-sort(ls)
  for (i in 1:length(ls)) {
    if (i==1) {
      load(file.path(diretoria_dados,ls[i]))
      dados<-dados_maver
    } else {
      load(file.path(diretoria_dados,ls[i]))
      dados<-rbind.fill(dados,dados_maver)
    }
    print(ls[i])
  }
  dados<-dados[order(dados$datahora),]
  return(dados)
}
