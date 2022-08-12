# Description -------------------------------------------------------------
#Esta rotina vai buscar ah dataframe os nomes dos sensores

get_sensors <- function(data){colnames(data)[-c(which(colnames(data)=="datahora"),
                                 which(colnames(data)=="idinfo"),
                                 which(substr(colnames(data),1,3)=="out"),
                                 which(substr(colnames(data),1,4)=="orig"),
                                 which(substr(colnames(data),1,6)=="offset"),
                                 which(substr(colnames(data),1,6)=="factor"))]}