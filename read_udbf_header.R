#' Read an udbf header file
#'
#' \code{read_udbf_header} returns an \code{vector} with the contents
#' of the udbf header file recorded by LNEC e.Gate aquisition system.
#' The returned \code{vector} will have the same fields has the
#' udbf Header file structure. 
#' @param udbf.hdr.fn The ablosute path to the header file to be read
#' @return A \code{vector} with the contents fo the udbf header file
#' @examples
#' read_udbf_header("F:\\dummy\\LE01Hdr191054.dat")
#' read_udbf_header("F:\\dummy\\E01Hdr027.dat")
#' @seealso \code{\link{read_udbf_bin}} uses this function to read the
#' header(s) needed to decode the udbf bin data file.
read_udbf_header<-function(udbf.hdr.fn) {

# rm(list=ls()) #limpa tudo
  
###
## Nota : Ao ler os caracteres a fun??o readBin le at? encontrar 00h independent.
##        do tamanho que se de, na defini??o da gantner todas as strings terminam com 00h
##        correctamente devia-se ler readChar(labview.bin.file, Bytelength, useBytes = TRUE)  
###

# create udbf hdr Structure -----------------------------------------------

hdr = vector('list',14) # numero de campos a guardar
names(hdr)[1]='IsBigEndian'
names(hdr)[2]='Version'
names(hdr)[3]='TypeVendorLen'
names(hdr)[4]='TypeVendor'
names(hdr)[5]='WithCheckSum'
names(hdr)[6]='ModuleAdditionalDataLen'
names(hdr)[7]='ModuleAdditionalData'
names(hdr)[8]='StartTimeToDayFactor'
names(hdr)[9]='dActTimeDataType'
names(hdr)[10]='dActTimeToSecondFactor'
names(hdr)[11]='StartTime'
names(hdr)[12]='SampleRate'
names(hdr)[13]='VariableCount'
names(hdr)[14]='VariableSettings'

hdr[[9]] = 7 #by default dActTimeDataType is Unsigned Int 32 (Long)

# readudbf Header ---------------------------------------------------------

# udbf.hdr.fn = "LE01Hdr001000.dat"
udbf.hdr.file = file(description = udbf.hdr.fn , open = "rb") #abre o ficheiro header

if (readBin(udbf.hdr.file,"int",size = 1,signed=FALSE) != 0) { #IsBigEndian
  hdr[[1]] = "big"
}  else {
  hdr[[1]] = "little"
}
hdr[[2]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #Version
hdr[[3]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #TypeVendorLen
hdr[[4]] = readBin(udbf.hdr.file,"character",size = hdr$TypeVendorLen,signed=FALSE,endian=hdr$IsBigEndian) #TypeVendor
hdr[[5]] = readBin(udbf.hdr.file,"int",size = 1,signed=FALSE) #WithCheckSum
hdr[[6]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #ModuleAdditionalDataLen
if (hdr$ModuleAdditionalDataLen > 0 ) { # TODO Read Additional Data to vector
  hdr[[7]] = readBin(udbf.hdr.file,"int",size = hdr$ModuleAdditionalDataLen,signed=FALSE,endian=hdr$IsBigEndian) #ModuleAdditionalData
}

hdr[[8]] = readBin(udbf.hdr.file,"double",size = 8,endian=hdr$IsBigEndian) #StartTimeToDayFactor
if (hdr$Version >= 107)
    hdr[[9]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #dActTimeDataType
hdr[[10]] = readBin(udbf.hdr.file,"double",size = 8,endian=hdr$IsBigEndian) #dActTimeToSecondFactor
hdr[[11]] = readBin(udbf.hdr.file,"double",size = 8,endian=hdr$IsBigEndian) #StartTime
hdr[[12]] = readBin(udbf.hdr.file,"double",size = 8,endian=hdr$IsBigEndian) #SampleRate
hdr[[13]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #VariableCount
if (hdr$VariableCount > 0) { #le variaveis
 
  for (i in 1:hdr$VariableCount) { #precorre vari?veis
    tmp.var = vector('list',10) # campos da vari?vel a guardar
    names(tmp.var)[1]='NameLen'
    names(tmp.var)[2]='Name'
    names(tmp.var)[3]='DataDirection'
    names(tmp.var)[4]='DataType'
    names(tmp.var)[5]='FieldLen'
    names(tmp.var)[6]='Precision'
    names(tmp.var)[7]='UnitLen'
    names(tmp.var)[8]='Unit'
    names(tmp.var)[9]='AdditionalDataLen'
    names(tmp.var)[10]='AdditionalData'
    
    tmp.var[[1]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #NameLen
    tmp.var[[2]] = readBin(udbf.hdr.file,"character",size = tmp.var$NameLen,signed=FALSE,endian=hdr$IsBigEndian) #Name
    tmp.var[[3]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #DataDirection
    tmp.var[[4]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #DataType
    tmp.var[[5]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #FieldLen
    tmp.var[[6]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #Precision
    tmp.var[[7]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #UnitLen
    tmp.var[[8]] = readBin(udbf.hdr.file,"character",size = tmp.var$UnitLen,signed=FALSE,endian=hdr$IsBigEndian) #Unit
    tmp.var[[9]] = readBin(udbf.hdr.file,"int",size = 2,signed=FALSE,endian=hdr$IsBigEndian) #AdditionalDataLen
    
    if (tmp.var$AdditionalDataLen > 0) { # TODO  ler a additional data da variavel para um vector
     tmp.var[[10]] = readBin(udbf.hdr.file,"int",size = tmp.var$AdditionalDataLen,signed=FALSE,endian=hdr$IsBigEndian) #AdditionalData
    } else {
      tmp.var[[10]] = 0
    }
    
    if (i>1){ # adiciona vector ? data.frame
      variable.settings = rbind(variable.settings,data.frame(tmp.var,stringsAsFactors = FALSE))
    }else{ # cria data.frame
      variable.settings = data.frame(tmp.var,stringsAsFactors = FALSE) #store tmp.var in data.frame
    }
    
    rm(tmp.var)
  } # for - precorre todas as vari?veis
  
  hdr[[14]] = variable.settings
  rm(variable.settings, i)
} #if le variaveis


close(udbf.hdr.file) # fecha o ficheiro header

return(hdr)

} # Function