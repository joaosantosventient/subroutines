FAST_data_compression <- function(RUN=T,
                                  nome_estrutura,
                                  rate_compressed_COMP,
                                  low_pass_freq,
                                  rate_compressed_FAST,
                                  grandezas_analise_estatica,
                                  grandezas_analise_dinamica){
  
  if(RUN==T){
    # FOLDERS -----------------------------------------------------------------
    directoria_dados_COMP <- paste0("//de-noe-comp1/shm/shm_data/",nome_estrutura,"/COMP")
    directoria_dados_FAST <- paste0("//de-noe-comp1/shm/shm_data/",nome_estrutura,"/FAST")
    directoria_dados_AUXI <- paste0("//de-noe-comp1/shm/shm_data/",nome_estrutura,"/AUXI")
    print(paste("Dir. COMP ->",directoria_dados_COMP))
    print(paste("Dir. FAST ->",directoria_dados_FAST))
    
    
    # LIST FILES TO PROCESS ---------------------------------------------------
    #*** List done and to do ------------------------------------------------
    ls_comp<-list.files(directoria_dados_COMP,pattern="RData")
    ls_fast<-list.files(file.path(directoria_dados_FAST),pattern="RData")
    
    #*** Define files not compiled yet --------------------------------------------
    if (length(ls_fast)!=0) {
      ls_fast_minus1 <- head(ls_fast,length(ls_fast)-1)#list FAST files except the last 
      ls_to_compile<-ls_comp[!substr(ls_comp,1,8) %in% substr(ls_fast_minus1,1,8)]
    } else {
      ls_to_compile<-ls_comp
    }
    #*** Manage exceptions --------------------------------------------------------
    exceptions_file_name <- "COMP exceptions.txt"
    if(file.exists(file.path(directoria_dados_AUXI,exceptions_file_name))){
      excecoes <- read.table(file.path(directoria_dados_AUXI,exceptions_file_name),colClasses = "character", header = F,check.names = F,na.strings = F)
      ls_to_compile<-setdiff(ls_to_compile,excecoes[,1]) #excecoes
    }else{excecoes <- NULL}
    
    # cap_date_RDATA <- "202105310000" # data de teste
    # ls_to_compile <- ls_to_compile[which(ls_to_compile >= paste0(cap_date_RDATA,".RData"))]# para apagar quando acabar de escrever a rotina
    
    #******Check if latest FAST file is complete
    if(length(ls_fast)>0){
      latest_FAST_file <- tail(ls_fast,1)
      load(file.path(directoria_dados_FAST,latest_FAST_file))
      dados_FAST_loaded <- dados; rm(dados)
      datahora_FAST <- paste0(unique(strftime(dados_FAST_loaded$datahora,format = "%Y%m%d%H%00",tz="GMT")),".RData")
      ls_to_compile <- setdiff(ls_to_compile,datahora_FAST)
    }else{
      latest_FAST_file <- ""
    }
    #****** Define last file to save  ---------------------------
    file_to_save_last <- paste0(substr(tail(ls_to_compile,1),1,8),substr(tail(ls_to_compile,1),13,100))
    
    
    
    
    # LOOP OVER FILES ------------------------------------------------------
    if (length(ls_to_compile)!=0) {
      for (i in 1:length(ls_to_compile)) {
        #*** Load COMP --------------------------------------------------------
        result=tryCatch({load(file.path(directoria_dados_COMP,ls_to_compile[i]))}, error=function(e){})
        if(is.null(result)){
          excecoes <- rbind(excecoes,ls_to_compile[i])
          write.table(x = excecoes,col.names = F,
                      file = file.path(directoria_dados_AUXI,exceptions_file_name),
                      quote = F,append = F,row.names = F)
          next
        }
        #*** Define files to save and to save next ---------------------------
        file_to_save<-paste0(substr(ls_to_compile[i],1,8),substr(ls_to_compile[i],13,100))
        if (i<length(ls_to_compile)) {
          file_to_save_next<-paste0(substr(ls_to_compile[i+1],1,8),substr(ls_to_compile[i+1],13,100))
        }
        #*** Find Samplerate ---------------------------------------------------------
        if (nrow(dados)>1) {samplerate<-round_any(median(1/as.numeric(diff(dados$datahora,1)),na.rm=T),10)} else {samplerate<-1}
        
        #*** Subset FAST  ----------------------------------------------------------
        dados_FAST <- dados[,c("datahora",colnames(dados)[substr(colnames(dados),1,1) %in% grandezas_analise_estatica])]
        
        #****** Merge and decimate FAST----------------------------------------------------
        if(is.null(ncol(dados_FAST))){print(paste("Skipped, none static sensors",ls_to_compile[i]));next} 
        if(is.na(samplerate)){print(paste("Error sample rate -> NA,",ls_to_compile[i]));next}
        if (samplerate>0) {
          if (exists("dados_FAST_rbind")) {
            dados_FAST_rbind<-rbind.fill(dados_FAST_rbind,dados_FAST[seq(1,nrow(dados_FAST),by=samplerate/rate_compressed_FAST),])
          } else {
            dados_FAST_rbind<-dados_FAST[seq(1,nrow(dados_FAST),by=samplerate/rate_compressed_FAST),]
          }
          print(ls_to_compile[i])
        }
        
        #*** Save FAST ----------------------------------------------------------
        if (file_to_save!=file_to_save_next | ls_to_compile[i]==tail(ls_to_compile,1) & file_to_save!=latest_FAST_file) {
          #****** Save all files from one day ----------------------------------------------
          dados <- dados_FAST_rbind
          save(dados,file=file.path(directoria_dados_FAST,file_to_save))
          print(paste(nome_estrutura,"-",file_to_save,"FAST saved"))
          rm(dados_FAST_rbind,dados); gc()
          
        }else if(file_to_save==latest_FAST_file & ls_to_compile[i]==tail(ls_to_compile,1)){
          #****** Update lastest FAST file every hour from the latest COMP files ----------------------
          dados <- rbind.fill(dados_FAST_loaded,dados_FAST_rbind)
          save(dados,file=file.path(directoria_dados_FAST,file_to_save))
          print(paste(nome_estrutura,"-",file_to_save,"FAST updated"))
          rm(dados,dados_FAST_loaded,dados_FAST_rbind)
        }
      }
    }
    rm(ls_fast, ls_to_compile)
  }
}