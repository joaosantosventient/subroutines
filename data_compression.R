data_compression <- function(RUN=T,
                             nome_estrutura,
                             rate_compressed_COMP,
                             low_pass_freq,
                             rate_compressed_FAST,
                             grandezas_analise_estatica,
                             grandezas_analise_dinamica,
                             periodo_nao_comprimido,
                             mail_warning_recipients=c()){
  
  if(RUN==T){
    # FOLDERS -------------------------------------------------------------------
    directoria_subroutines <- "//de-noe-comp1/shm/routines/subroutines"
    directoria_dados_COMP <- paste0("//de-noe-comp1/shm/shm_data/",nome_estrutura,"/COMP")
    directoria_dados_COMP_save <- directoria_dados_COMP #para testes pode-se definir outro caminho para guardar os dados COMP para nao afetar os dados originais
    directoria_dados_FAST <- paste0("//de-noe-comp1/shm/shm_data/",nome_estrutura,"/FAST")
    directoria_dados_AUXI <- paste0("//de-noe-comp1/shm/shm_data/",nome_estrutura,"/AUXI")
    print(paste("Dir. COMP ->",directoria_dados_COMP))
    print(paste("Dir. FAST ->",directoria_dados_FAST))
    
    # SUBROUTINES -----------------------------------------------
    source(file.path(directoria_subroutines,"subset modal.R"))
    # LIBRARIES -----------------------------------------------
    library(dplyr); library(plyr); library(pracma); library(signal); library(cluster); library(fpc); library(e1071); library(lubridate)
    library(mailR)
    
    # FUNCTIONS -----------------------------------------------------------------
    mail_warning <- function(mail_message,recipients){
      if(length(mail_warning_recipients)>0){
        send.mail(from = "shm.noelnec@gmail.com",
                  to = recipients,
                  subject = paste("FAST compile"),
                  body = mail_message,
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "shm.noelnec", passwd = "0br4sn03", ssl = TRUE),
                  authenticate = TRUE,
                  html = TRUE,
                  send = TRUE,
                  debug = F)
      }
    }
    
    #*** LOG file --------------------------------------------------------
    log_file_name <- "FAST compile log.txt"
    if(file.exists(file.path(directoria_dados_AUXI,log_file_name))){
      FAST_log <- read.table(file.path(directoria_dados_AUXI,log_file_name),colClasses = "character", header = F,check.names = F,na.strings = F,sep = "\t")
    }else{FAST_log <- NULL}
    save_FAST_log <- function(FAST_log,msg,print=T){
      FAST_log <- rbind(msg,FAST_log)
      if(is.data.frame(FAST_log)){FAST_log <- distinct(FAST_log)}
      write.table(x = FAST_log,col.names = F,
                  file = file.path(directoria_dados_AUXI,log_file_name),
                  quote = F,append = F,row.names = F)
      if(print==T){print(msg)}
      msg <- ""
      assign("FAST_log",FAST_log, envir = .GlobalEnv)
    }
    
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
      excecoes <- read.table(file.path(directoria_dados_AUXI,exceptions_file_name),colClasses = "character", header = F,check.names = F,na.strings = F,sep = "\t")
      ls_to_compile<-setdiff(ls_to_compile,excecoes[,1]) #excecoes
    }else{excecoes <- NULL}
    
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
    #*** Subset files to compress from date range --------------------------------------------
    sys_date <- as.POSIXlt(Sys.Date(),tz="GMT"); sys_date$year <- sys_date$year - periodo_nao_comprimido
    cap_date_COMP_RDATA <- paste0(format(sys_date,"%Y%m%d%H%M"),".RData")
    dados_COMP_list <- list()
    
    
    
    # LOOP OVER FILES ------------------------------------------------------
    if (length(ls_to_compile)!=0) {
      
      skip_file <- F
      for (i in 1:length(ls_to_compile)) {
        
        #Mail progress tracking
        if(i==1){previous_hour <- hour(Sys.time())}
        if(hour(Sys.time()) > previous_hour){
          msg <- paste("loading",ls_to_compile[i])
          mail_warning(msg,recipients = mail_warning_recipients)
          previous_hour <- hour(Sys.time())
        }else if(hour(Sys.time())==0 & previous_hour==23){
          previous_hour <- -1
        }
        #*** Define files to save and to save next ---------------------------
        file_to_save<-paste0(substr(ls_to_compile[i],1,8),substr(ls_to_compile[i],13,100))
        if (i<length(ls_to_compile)) {
          file_to_save_next<-paste0(substr(ls_to_compile[i+1],1,8),substr(ls_to_compile[i+1],13,100))
        }
        #*** Load COMP --------------------------------------------------------
        result=tryCatch({
          load(file.path(directoria_dados_COMP,ls_to_compile[i]))
        }, error=function(e){})
        if(is.null(result)){
          msg <-paste(ls_to_compile[i],"-> load error")
          save_FAST_log(FAST_log,msg)
          excecoes <- rbind(ls_to_compile[i],excecoes)
          if(is.data.frame(excecoes)){excecoes <- distinct(excecoes)}
          write.table(x = excecoes,col.names = F,
                      file = file.path(directoria_dados_AUXI,exceptions_file_name),
                      quote = F,append = F,row.names = F)
          skip_file <- T
        }else print(paste(ls_to_compile[i],"loaded"))
        if(skip_file==F){
          #****** Check COMP variables names --------------------------------------------------------
          sensors_dados_COMP <- colnames(dados)[-which(colnames(dados)=="datahora")]
          if(any(!unique(substr(sensors_dados_COMP,1,1)) %in% c(grandezas_analise_estatica,grandezas_analise_dinamica))){
            msg <- paste(ls_to_compile[i],"- ficheiro COMP contem variaveis nao contempladas nas grandezas da rotina")
            save_FAST_log(FAST_log,msg)
          }
          #*** Find Samplerate ---------------------------------------------------------
          if(colnames(dados)[1]=="dados"){colnames(dados)[1] <- "datahora"}
          if (nrow(dados)>1) {samplerate<-round_any(median(1/as.numeric(diff(dados$datahora,1)),na.rm=T),10)} else {samplerate<-1}
          if(is.infinite(samplerate)){
            msg <- paste(ls_to_compile[i],"-> samplerate = Inf, file skiped")
            save_FAST_log(FAST_log,msg)
            skip_file <- T
          }
          if(samplerate==0){
            msg <- paste(ls_to_compile[i],"-> samplerate = 0, file skiped")
            save_FAST_log(FAST_log,msg)
            skip_file <- T
          }
          if(skip_file==F){
            #****** Manipulate date----------------------------------------------
            datahora <- gsub(".RData","",ls_to_compile[i])
            datahora <- as.POSIXct(datahora,format="%Y%m%d%H%M",tz = "GMT")
            datahora <- as.POSIXct(datahora, format = "%Y-%m-%d %H:%M:%OS")
            datahora_col_1_4 <- as.POSIXct(strftime(dados$datahora[round(nrow(dados)/4)], format = "%Y-%m-%d %H:%00",tz = "GMT"),tz = "GMT")
            datahora_col_1_2 <- as.POSIXct(strftime(dados$datahora[round(nrow(dados)/2)], format = "%Y-%m-%d %H:%00",tz = "GMT"),tz = "GMT")
            datahora_col_3_4 <- as.POSIXct(strftime(dados$datahora[round(nrow(dados)*3/4)], format = "%Y-%m-%d %H:%00",tz = "GMT"),tz = "GMT")
            cond1 <- datahora!=datahora_col_1_4
            cond2 <- datahora!=datahora_col_1_2
            cond3 <- datahora!=datahora_col_3_4
            if( (cond1 && cond2) || (cond1 && cond3) || (cond2 && cond3) ){ #if at least two of the hours differ from the filename, than datahora is rebuild
              strftime2 <- function(x, ...) {strftime(x + 1e-6,...)}#https://stackoverflow.com/questions/28303026/adding-milliseconds-to-time-stamp-in-r
              start <- datahora
              end <- start + nrow(dados)/samplerate
              datahora_vec <- strftime2(seq(start,end,units="seconds",by=1/samplerate),tz="GMT")
              if(length(datahora_vec)!=nrow(dados)){
                if(length(datahora_vec)>nrow(dados)){
                  datahora_vec <- datahora_vec[1:nrow(dados)]
                }else{
                  msg <- paste(ls_to_compile[i],"-> unable to create datahora, file skiped")
                  save_FAST_log(FAST_log,msg)
                  mail_warning(msg,recipients = mail_warning_recipients)
                  skip_file <- T
                }
              }
              dados$datahora <- datahora_vec
              msg <- paste(ls_to_compile[i],"-> coluna datahora corrigida pelo nome do ficheiro")
              save_FAST_log(FAST_log,msg = msg)
            }
            if(skip_file==F){
              #*** Subset  FAST  ----------------------------------------------------------
              grandezas_FAST <- colnames(dados)[! substr(colnames(dados),1,1) %in% grandezas_analise_dinamica]
              if(length(grandezas_FAST)==1){
                dados_FAST <- as.data.frame(dados[,grandezas_FAST])
                colnames(dados_FAST) <- grandezas_FAST
              }else dados_FAST <- dados[,grandezas_FAST]
              #****** Merge and decimate FAST----------------------------------------------------
              if(is.null(ncol(dados_FAST))){
                msg <- paste("Skipped, none static sensors",ls_to_compile[i]);
                save_FAST_log(FAST_log,msg = msg)
                mail_warning(msg,recipients = mail_warning_recipients)
                break
              } 
              if(is.na(samplerate)){msg <- paste("Error sample rate -> NA,",ls_to_compile[i]); save_FAST_log(FAST_log,msg = msg); skip_file <- T}
              if(skip_file==F){
                if (samplerate>0) {
                  if(samplerate>rate_compressed_FAST & samplerate%%rate_compressed_FAST==0){
                    dados_FAST_decim <- dados_FAST[seq(1,nrow(dados_FAST),by=samplerate/rate_compressed_FAST),]
                    print(paste0(ls_to_compile[i],": FAST file subseted and decimated"))
                  }else{
                    dados_FAST_decim <- dados_FAST
                    print(paste0(ls_to_compile[i],": FAST file subseted not decimated"))
                    break
                  }
                  if (exists("dados_FAST_rbind")) {
                    dados_FAST_rbind<-rbind.fill(dados_FAST_rbind,dados_FAST_decim)
                  } else {
                    dados_FAST_rbind<-dados_FAST_decim
                  }
                  rm(dados_FAST_decim)
                }
                
                #*** Manage COMP if older than 1 year ---------------------------
                if(ls_to_compile[i]<cap_date_COMP_RDATA){
                  #****** Subset COMP--------------------------------------------------
                  grandezas_COMP <- colnames(dados)[substr(colnames(dados),1,1) %in% grandezas_analise_dinamica]
                  if(length(grandezas_COMP)==1){
                    new_dados_COMP_modal <- as.data.frame(dados[,grandezas_COMP])
                    colnames(new_dados_COMP_modal) <- grandezas_COMP
                    msg <- paste(ls_to_compile[i],"-> COMP modal so com uma coluna")
                    mail_warning(msg,recipients = mail_warning_recipients) 
                  }else new_dados_COMP_modal <- dados[,grandezas_COMP]
                  
                  datahora_vec_COMP <- dados$datahora
                  subset_acel <- as.data.frame(new_dados_COMP_modal[,colnames(dados)[substr(colnames(dados),1,1) %in% c("a")]])
                  NA_rows <- which(apply(subset_acel, 1, function(i_row){any(is.na(i_row))})==T)
                  samplerate_updated <- F
                  if(length(NA_rows)>0){
                    new_dados_COMP_modal <- new_dados_COMP_modal[-NA_rows,]
                    datahora_vec_COMP <- datahora_vec_COMP[-NA_rows]
                    #Update samplerate
                    if (nrow(new_dados_COMP_modal)>1) {
                      new_samplerate<-round_any(median(1/as.numeric(diff(as.POSIXct(datahora_vec_COMP),1)),na.rm=T),10)
                      if(new_samplerate!=samplerate){
                        previous_samplerate <- samplerate
                        samplerate <- new_samplerate
                        samplerate_updated <- T
                      }
                    } else {samplerate<-1}
                  }
                  #****** Filter and decimate --------------------------------------------------
                  if(samplerate>rate_compressed_COMP & samplerate%%rate_compressed_COMP==0){
                    dados_filt <- apply(new_dados_COMP_modal, 2, filtfilt, filt = butter(n = 4,W = low_pass_freq/(samplerate/2),type = "low"))
                    new_dados_COMP<-cbind(datahora = datahora_vec_COMP,data.frame(dados_filt))
                    new_dados_COMP<-new_dados_COMP[seq(1,nrow(new_dados_COMP),by=samplerate/rate_compressed_COMP),]  
                    print(paste0(ls_to_compile[i],": COMP file subseted, filtered and decimated | samplerate ",samplerate,"Hz ->",rate_compressed_COMP,"Hz"))
                  }else {
                    new_dados_COMP <- cbind(datahora = datahora_vec_COMP,data.frame(new_dados_COMP_modal))
                    if(samplerate_updated==T & nrow(new_dados_COMP)<nrow(dados)){
                      print(paste0(ls_to_compile[i],": COMP file subseted and decimated | samplerate ",previous_samplerate,"Hz ->",new_samplerate,"Hz"))
                    }else{
                      print(paste0(ls_to_compile[i],": COMP file subseted, not decimated: ",samplerate,"Hz"))
                    }
                  }
                  #****** Merge compressed COMP files to list----------------------------------------------------
                  dados_COMP_list[[ls_to_compile[i]]] <- new_dados_COMP
                }
              }
            }
          }
        }
        
        #*** Save FAST and COMP----------------------------------------------------------
        if (file_to_save!=file_to_save_next | ls_to_compile[i]==tail(ls_to_compile,1) & file_to_save!=latest_FAST_file) {
          #********* Save FAST ----------------------------------------------
          if(exists("dados_FAST_rbind")){
            dados <- dados_FAST_rbind
            print("########## DONT STOP THIS SCRIPT ##########")
            print(paste(nome_estrutura,"-",file_to_save,"Saving FAST"))
            save(dados,file=file.path(directoria_dados_FAST,file_to_save))
            rm(dados_FAST_rbind,dados); gc()
            result=tryCatch({load(file.path(directoria_dados_FAST,file_to_save))}, error=function(e){})
            if(is.null(result)){
              msg <- paste(file_to_save,"FAST error saving")
              save_FAST_log(FAST_log,msg = msg)
              mail_warning(msg,recipients = mail_warning_recipients)
              break
            };rm(dados)
            print("FAST saved")
          }
          #********* Save COMP ----------------------------------------------
          if(length(dados_COMP_list)>0){
            for(ii in 1:length(dados_COMP_list)){
              dados <- dados_COMP_list[[ii]]
              file_name <- names(dados_COMP_list)[ii]
              save(dados,file = file.path(directoria_dados_COMP_save,file_name))
              print(paste(file_name,"- COMP saved - DONT STOP THIS SCRIPT"))
            };rm(ii)
            dados_COMP_list <- list()
            rm(new_dados_COMP,dados)
          }
        }else if(file_to_save==latest_FAST_file & ls_to_compile[i]==tail(ls_to_compile,1)){
          #****** Update lastest FAST file every hour from the latest COMP files ----------------------
          dados <- rbind.fill(dados_FAST_loaded,dados_FAST_rbind)
          save(dados,file=file.path(directoria_dados_FAST,file_to_save))
          print(paste(nome_estrutura,"-",file_to_save,"FAST updated"))
          rm(dados,dados_FAST_loaded,dados_FAST_rbind)
        }
        skip_file <- F
      }
    }
    rm(ls_fast, ls_to_compile)
  }
}