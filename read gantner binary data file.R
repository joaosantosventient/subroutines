
# Input start -------------------------------------------------------------

rm(list=ls())
setwd("//de-noe-comp1/shm/routines")
diretoria_subrotinas<-"//de-noe-comp1/shm/routines/subroutines"
directoria_dados<-"//de-noe-comp1/shm/shm_data/sado - fase 2/aaa/"
file<-"E02E05@19052918000000.dat"
source(file.path(diretoria_subrotinas,"read gantner binary header.R"))
source(file.path(diretoria_subrotinas,"read gantner binary datahora.R"))
source(file.path(diretoria_subrotinas,"read gantner binary data.R"))
interv_aquisicao<-3600                                # numero de segundos nos quais foram adquirido dados

### Read Header
file_head_master<-paste0(substr(file,0,3),"Hdr",substr(file,nchar(file[1])-9,nchar(file)-7),".dat")
file_head_slave<-paste0(substr(file,4,6),"Hdr",substr(file[1],nchar(file)-6,nchar(file)-4),".dat")
header_master<-gantner_bin_header(directoria_dados,file_head_master)
header_slave<-gantner_bin_header(directoria_dados,file_head_slave)

### Read Dados
options(digits=12); options(digits.secs=12)
datahora<-readbin_gantner_datahora(directoria_dados,file,header_master,header_slave,1,1,header_slave$samplerate*interv_aquisicao)
dados<-readbin_gantner_data(directoria_dados,file,header_master,header_slave,1,1,header_slave$samplerate*interv_aquisicao,id=c("a","d","c","e","T","p","o"))
dados<-cbind(datahora=datahora,dados)

# WRITE TO CSV
write.csv(dados,file="aceleration_E05_2019053018.csv")

