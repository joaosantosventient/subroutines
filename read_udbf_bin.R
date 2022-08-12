#' Read an udbf file
#'
#' \code{read_udbf_bin} returns an \code{data.frame} with the contents
#' of the udbf hourly data file recorded by LNEC e.Gate aquisition system.
#' The names of the \code{data.frame} columns will be the names given
#' inside the udbf Header file structure. 
#' The dActTime (ts) data field is converted to a TimeStamp Posix format.
#' Note: The related udbf header(s) files must be in the
#' same directory as the binary udbf data files.
#' @param udbf_bin_dir The directory location of the udbf file
#' @param udbf_bin_fn The file name of the binary udbf file
#' @return A \code{data.frame} with the contents fo the udbf file
#' @seealso \code{\link{read_udbf_header}} is used by this function to read
#' the header(s) needed to decode the udbf bin file.
#' \code{\link{read_labview_bin}} reads the recorded data files if it are in
#' Labview binary format (Jorge Rodrigues/NOE udbf convertion VI)
#' @examples
#' ## How to call the funtion
#' ## Note: The udbf header file(s) must be in the same directory
#' 
#' read_udbf_bin("F:\\dummy","LE01@@14021503191054.dat")
#' read_udbf_bin("F:\\dummy","E02E01@@15070103297027.dat")
#' 
#' ## Example How to read an udbf data file save it to disk
#' ## load it again and plot the contents of the data.frame
#' 
#' udbf.bin.dir <- "F:\\p25a\\analise_1_semana\\2014 Fevereiro"
#' udbf.bin.fn <- "LE01@@14021503191054.dat" 
#' 
#' r.bin.dir <- "F:\\p25a\\analise_1_semana\\2014 Fevereiro\\rda" #dir to save rda file. Must Exist
#' 
#' data <- read_udbf_bin(udbf.bin.dir,udbf.bin.fn)
#' r.bin.fn <-paste(substr(udbf.bin.fn,1,19),'.rda',sep='')
#' save(data,file = paste(r.bin.dir,r.bin.fn,sep='\\'),compress = "xz")
#' # plot(data$e1.66S)
#' 
#' load(paste(r.bin.dir,r.bin.fn,sep='\\'))
#' plot(data$ts,data$e1.66S) 
read_udbf_bin <- function(udbf_bin_dir,udbf_bin_fn) {

#source("read_udbf_header.R")

udbf_bin_abs_fn <- paste(udbf_bin_dir,udbf_bin_fn,sep='\\')

# Le ficheiro header ------------------------------------------------------


if (substr(udbf_bin_fn,1,1) == "L") { # Low data file name format
  udbf_hdr_fn <- paste ( substr ( udbf_bin_fn , 1 , 4 ) , # LE01
                      'Hdr',
                      substr(udbf_bin_fn,14,23),sep='') #001000.dat  
  hdr <- read_udbf_header(paste(udbf_bin_dir,udbf_hdr_fn,sep='\\')) #L? ficheiro header udbf
  
}else if((substr(udbf_bin_fn,1,1)=="E")){ #normal data file format
  
  udbf_master_hdr_fn <- paste(substr(udbf_bin_fn,1,3), #E01
                       'Hdr',
                       substr(udbf_bin_fn,16,18),".dat",sep='') #001.dat  
  hdr.master <- read_udbf_header(paste(udbf_bin_dir,
                                       udbf_master_hdr_fn,
                                       sep='\\')) #L? ficheiro header udbf
  
  udbf.slave.hdr.fn <- paste(substr(udbf_bin_fn,4,6), #E01
                              'Hdr',
                              substr(udbf_bin_fn,19,21),".dat",sep="") #001.dat
  hdr <- read_udbf_header(paste(udbf_bin_dir,udbf.slave.hdr.fn,sep='\\')) #L? ficheiro header udbf
  
  hdr$StartTime <- hdr.master$StartTime
  rm(hdr.master)
}

# Corrige dActTimeDataType
# if(hdr$dActTimeDataType>12) hdr$dActTimeDataType <- 12  #falta correcao qd null





# Cria variaveis necessarias a leitura do ficheiro binario -----------------


# Segundo UDBF File Format V1.07 : "http://www.ioselect.com/Downloads/UDBF_107.pdf" 
# mais info do código do Driver LabView
udbf_data_type <- c("logical","integer","integer","integer","integer",
                    "integer","integer","double","raw","raw",
                    "raw","double","integer","integer","raw")
udbf_data_sign <- c(FALSE,TRUE,FALSE,TRUE,FALSE,
                    TRUE,FALSE,TRUE,FALSE,FALSE,
                    FALSE,TRUE,TRUE,FALSE,FALSE)
udbf_data_size <- c(1,1,1,2,2,
                    4,4,4,1,2,
                    4,8,8,8,8)

#cria vector tempor?rio para guardar colunas de vari?veis
tmp_read <- vector('list',hdr$VariableCount+1) # linha de leitura a guardar ts+variaveis
names(tmp_read)[1]<-'ts' #dActTime
# tmp_read_data_size <- udbf_data_size[hdr$dActTimeDataType] #dActTime
for(i in 1:hdr$VariableCount){ 
  names(tmp_read)[i+1] <- hdr$VariableSettings$Name[i] # atribui nome de todas as vari?veis
}

# Ve Tamanhos de cada variavel e sign
tmp_read_var_size <- c(udbf_data_size[hdr$dActTimeDataType],
                       udbf_data_size[hdr$VariableSettings$DataType]) # dActTime + variable size in bytes 
tmp_read_data_size <- sum(tmp_read_var_size)                            # numero total de bytes
tmp_read_var_sign <- c(udbf_data_sign[hdr$dActTimeDataType],
                       udbf_data_sign[hdr$VariableSettings$DataType]) # signed

udbf_bin_file_size <- file.info(udbf_bin_abs_fn)$size # tamanho do ficheiro udbf


# Le Ficheiro binario para memoria ------------------------------------------------


# Le tudo de uma vez para memoria
lines <- round(udbf_bin_file_size / tmp_read_data_size) #TODO ver melhor como calcular numero de linhas

udbf.bin.file <- file(description = udbf_bin_abs_fn , open = "rb") #abre o ficheiro udbf
buffer <- matrix(readBin(udbf.bin.file,"raw",n=lines*tmp_read_data_size),
                 nrow=lines,byrow=TRUE)
close(udbf.bin.file) # fecha o ficheiro udbf

rm(udbf.bin.file,udbf_bin_file_size)


# Converte matris de bytes  lida em variaveis -----------------------------

# converte raw data em variaveis
tmp_read[[1]] <- readBin( c(t(buffer[,1:tmp_read_var_size[1]])),
                          udbf_data_type[hdr$dActTimeDataType],
                          size = tmp_read_var_size[1],
                          n=nrow(buffer),
                          signed=tmp_read_var_sign[1],
                          endian=hdr$IsBigEndian) #dActTime

for (i in 1:hdr$VariableCount) {  #le variaveis
  
  col.inicio <- sum(tmp_read_var_size[1:i]) + 1 # soma dos bytes das variaveis
                                                # anteriores + 1
  col.fim <- col.inicio + tmp_read_var_size[i + 1] - 1 # coluna de inicio +
                                                       # numero bytes variavel
  tmp_read[[i+1]] <- readBin(c(t(buffer[,col.inicio:col.fim])),
                             udbf_data_type[hdr$VariableSettings$DataType[i]],
                             n=nrow(buffer),
                             size = tmp_read_var_size[i + 1],
                             signed=tmp_read_var_sign[i + 1],
                             endian=hdr$IsBigEndian)
} #for

rm(buffer,i,col.inicio,col.fim,lines,
   tmp_read_var_size,tmp_read_var_sign,tmp_read_data_size,
   udbf_data_sign,udbf_data_size,udbf_data_type)


# Converte dActTime para TimeStamp Posix ----------------------------------


# Gatner time stamp origin = 1899-12-30 00:00:00.000
start.time <- as.POSIXct(hdr$StartTime * hdr$StartTimeToDayFactor * 86400,
                         origin="1899-12-30 00:00:00.000000",
                         tz="UTC")
tmp_read[[1]] <- start.time + tmp_read[[1]] * hdr$dActTimeToSecondFactor


# Converte vector em data.frame -------------------------------------------

bin.data <- data.frame(tmp_read)
rm(tmp_read)


# Devolve Valor -----------------------------------------------------------

return(bin.data)

} # end function
