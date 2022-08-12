# Descricao ---------------------------------------------------------------
#Função para filtrar apenas os senosres do estatico.

# função ------------------------------------------------------------------

sensores_fun <- function(data){colnames(data)[-c(which(colnames(data)=="datahora"),
                                                 which(colnames(data)=="idinfo"),
                                                 which(substr(colnames(data),1,3)=="out"),
                                                 which(substr(colnames(data),1,4)=="orig"),
                                                 which(substr(colnames(data),1,6)=="offset"),
                                                 which(substr(colnames(data),1,6)=="factor"))]}