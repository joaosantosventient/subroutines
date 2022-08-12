read_gantner_bin_header_multiple<-function(directoria,file) {
  
  ### ATTENTION: THIS FUNCTION CALLS READ_GANTNER_BINARY_HEADER
  
   if (substr(file[[1]][1],1,1)=="E") {
    file_head_master<-paste0(substr(file[[1]][1],0,3),"Hdr",substr(file[[1]][1],nchar(file[[1]][1])-9,nchar(file[[1]][1])-7),".dat")
    file_head_slave<-paste0(substr(file[[1]][1],4,6),"Hdr",substr(file[[1]][1],nchar(file[[1]][1])-6,nchar(file[[1]][1])-4),".dat")
    header_master<-gantner_bin_header(directoria_dados,file_head_master); header_slave<-gantner_bin_header(directoria_dados,file_head_slave)
  } else {
    file_head_master<-file_head_slave<-paste0(substr(file[[1]][1],0,4),"Hdr",substr(file[[1]][1],nchar(file[[1]][1])-9,nchar(file[[1]][1])-4),".dat")
    header_master<-gantner_bin_header(directoria_dados,file_head_master); header_slave<-gantner_bin_header(directoria_dados,file_head_slave)
  }
  headers<-list(header_master=header_master,header_slave=header_slave)
  return(headers)
}