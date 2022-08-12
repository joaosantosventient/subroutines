
gantner_bin_header<-function(directoria,file) {

  hdr<-file(paste0(directoria,file),"rb")
  header<-list()
  
  ### endianness and version
  header$endianness<-readBin(hdr,"integer",n=1,size=1,signed=FALSE,endian=TRUE)
  if (header$endianness==1) {header$endianness<-"big"} else {header$endianness<-"little"}
  header$version<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
  
  ### superfluous optional data
  if (header$version>=106) {
    header$numberbytes<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    for (i in 1:header$numberbytes) {readBin(hdr,"integer",n=1,size=1,signed=FALSE,endian=header$endianness)}
  }
  if (header$version>=101) {header$withchecksum<-readBin(hdr,"integer",n=1,size=1,signed=FALSE,endian=header$endianness)} else {header$withchecksum<-1}
  header$moduleadditionaldatalen<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
  if (header$moduleadditionaldatalen>=4) {
    readBin(hdr,integer(),n=1,size=2,signed=FALSE,endian=header$endianness)
    readBin(hdr,integer(),n=1,size=2,signed=FALSE,endian=header$endianness)
  }
  if (header$moduleadditionaldatalen>=5) {
    readBin(hdr,integer(),n=1,size=header$moduleadditionaldatalen-4,signed=FALSE,endian=header$endianness)
  }
  
  ### time related data
  header$starttimetodayfactor<-readBin(hdr,"double",n=1,size=8,endian=header$endianness)
  if (header$version>=107) {
    header$dacttimedatatype<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
  } else {
    header$dacttimedatatype<-7
  }
  if (header$dacttimedatatype==0) {header$dacttimedatatype<-"no"; header$dacttimebytelen<-0}
  if (header$dacttimedatatype==1) {header$dacttimedatatype<-"boolean"; header$dacttimebytelen<-1}
  if (header$dacttimedatatype==2) {header$dacttimedatatype<-"signedint8"; header$dacttimebytelen<-1}
  if (header$dacttimedatatype==3) {header$dacttimedatatype<-"unsignedint8"; header$dacttimebytelen<-1}
  if (header$dacttimedatatype==4) {header$dacttimedatatype<-"signedint16"; header$dacttimebytelen<-2}
  if (header$dacttimedatatype==5) {header$dacttimedatatype<-"unsignedint16"; header$dacttimebytelen<-2}
  if (header$dacttimedatatype==6) {header$dacttimedatatype<-"signedint32"; header$dacttimebytelen<-4}
  if (header$dacttimedatatype==7) {header$dacttimedatatype<-"unsignedint32"; header$dacttimebytelen<-4}
  if (header$dacttimedatatype==8) {header$dacttimedatatype<-"float"; header$dacttimebytelen<-4}
  if (header$dacttimedatatype==9) {header$dacttimedatatype<-"bibset8"; header$dacttimebytelen<-1}
  if (header$dacttimedatatype==10) {header$dacttimedatatype<-"bibset16"; header$dacttimebytelen<-2}
  if (header$dacttimedatatype==11) {header$dacttimedatatype<-"bibset32"; header$dacttimebytelen<-4}
  if (header$dacttimedatatype==12) {header$dacttimedatatype<-"double"; header$dacttimebytelen<-8}
  if (header$dacttimedatatype==13) {header$dacttimedatatype<-"signedint64"; header$dacttimebytelen<-8}
  if (header$dacttimedatatype==14) {header$dacttimedatatype<-"unsignedint64"; header$dacttimebytelen<-8}
  if (header$dacttimedatatype==15) {header$dacttimedatatype<-"bibset64"; header$dacttimebytelen<-8}
  header$dacttimetosecondfactor<-readBin(hdr,"double",n=1,size=8,endian=header$endianness)
  header$starttime<-readBin(hdr,"double",n=1,size=8,endian=header$endianness)
  header$samplerate<-readBin(hdr,"double",n=1,size=8,endian=header$endianness)
  header$channelcount<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
  
  ### Eliminate superfluous data
  header$withchecksum<-header$moduleadditionaldatalen<-header$numberbytes<-NULL
   
  ### Channel Data
  i<-1
  for (i in 1:header$channelcount) {
    ### channel meaningful data
    header[[paste0("channel",i)]]<-list()
    header[[paste0("channel",i)]]$namelen<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    header[[paste0("channel",i)]]$varname<-readBin(hdr,"character",n=1,size=header[[paste0("channel",i)]]$namelen,signed=FALSE,endian=header$endianness)
    header[[paste0("channel",i)]]$datadir<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
      if (header[[paste0("channel",i)]]$datadir==0) {header[[paste0("channel",i)]]$datadir<-"input"}
      if (header[[paste0("channel",i)]]$datadir==1) {header[[paste0("channel",i)]]$datadir<-"output"}
      if (header[[paste0("channel",i)]]$datadir==2) {header[[paste0("channel",i)]]$datadir<-"inputoutput"}
      if (header[[paste0("channel",i)]]$datadir==3) {header[[paste0("channel",i)]]$datadir<-"empty"}
    header[[paste0("channel",i)]]$datatype<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    if (header[[paste0("channel",i)]]$datatype==0) {header[[paste0("channel",i)]]$datatype<-"no"; header[[paste0("channel",i)]]$databytelen<-0}
    if (header[[paste0("channel",i)]]$datatype==1) {header[[paste0("channel",i)]]$datatype<-"boolean"; header[[paste0("channel",i)]]$databytelen<-1}
    if (header[[paste0("channel",i)]]$datatype==2) {header[[paste0("channel",i)]]$datatype<-"signedint8"; header[[paste0("channel",i)]]$databytelen<-1}
    if (header[[paste0("channel",i)]]$datatype==3) {header[[paste0("channel",i)]]$datatype<-"unsignedint8"; header[[paste0("channel",i)]]$databytelen<-1}
    if (header[[paste0("channel",i)]]$datatype==4) {header[[paste0("channel",i)]]$datatype<-"signedint16"; header[[paste0("channel",i)]]$databytelen<-2}
    if (header[[paste0("channel",i)]]$datatype==5) {header[[paste0("channel",i)]]$datatype<-"unsignedint16"; header[[paste0("channel",i)]]$databytelen<-2}
    if (header[[paste0("channel",i)]]$datatype==6) {header[[paste0("channel",i)]]$datatype<-"signedint32"; header[[paste0("channel",i)]]$databytelen<-4}
    if (header[[paste0("channel",i)]]$datatype==7) {header[[paste0("channel",i)]]$datatype<-"unsignedint32"; header[[paste0("channel",i)]]$databytelen<-4}
    if (header[[paste0("channel",i)]]$datatype==8) {header[[paste0("channel",i)]]$datatype<-"float"; header[[paste0("channel",i)]]$databytelen<-4}
    if (header[[paste0("channel",i)]]$datatype==9) {header[[paste0("channel",i)]]$datatype<-"bibset8"; header[[paste0("channel",i)]]$databytelen<-1}
    if (header[[paste0("channel",i)]]$datatype==10) {header[[paste0("channel",i)]]$datatype<-"bibset16"; header[[paste0("channel",i)]]$databytelen<-2}
    if (header[[paste0("channel",i)]]$datatype==11) {header[[paste0("channel",i)]]$datatype<-"bibset32"; header[[paste0("channel",i)]]$databytelen<-4}
    if (header[[paste0("channel",i)]]$datatype==12) {header[[paste0("channel",i)]]$datatype<-"double"; header[[paste0("channel",i)]]$databytelen<-8}
    if (header[[paste0("channel",i)]]$datatype==13) {header[[paste0("channel",i)]]$datatype<-"signedint64"; header[[paste0("channel",i)]]$databytelen<-8}
    if (header[[paste0("channel",i)]]$datatype==14) {header[[paste0("channel",i)]]$datatype<-"unsignedint64"; header[[paste0("channel",i)]]$databytelen<-8}
    if (header[[paste0("channel",i)]]$datatype==15) {header[[paste0("channel",i)]]$datatype<-"bibset64"; header[[paste0("channel",i)]]$databytelen<-8}
    header[[paste0("channel",i)]]$fieldlen<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    header[[paste0("channel",i)]]$precision<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    header[[paste0("channel",i)]]$unitlen<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    header[[paste0("channel",i)]]$unit<-readBin(hdr,"character",n=1,size=header[[paste0("channel",i)]]$unitlen,signed=FALSE,endian=header$endianness)
    
    ### channel superfluous data
    header[[paste0("channel",i)]]$additionaldatalen<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    if (header[[paste0("channel",i)]]$additionaldatalen>0) {
      header[[paste0("channel",i)]]$additionaldatastructid1<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
      header[[paste0("channel",i)]]$additionaldatastructid2<-readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
    } else {
      header[[paste0("channel",i)]]$additionaldatastructid1<-header[[paste0("channel",i)]]$additionaldatastructid2<-0
    }
    if (header[[paste0("channel",i)]]$additionaldatastructid1==0) {
      if (header[[paste0("channel",i)]]$additionaldatalen>=5) {readBin(hdr,"character",n=1,size=header[[paste0("channel",i)]]$additionaldatalen,signed=FALSE,endian=header$endianness)}
    }
    if (header[[paste0("channel",i)]]$additionaldatastructid1==11) {
      if (header[[paste0("channel",i)]]$additionaldatastructid2==1) {
        readBin(hdr,"integer",n=1,size=2,signed=FALSE,endian=header$endianness)
      }
    }
    header[[paste0("channel",i)]]$datadir<-header[[paste0("channel",i)]]$additionaldatalen<-header[[paste0("channel",i)]]$namelen<-header[[paste0("channel",i)]]$unitlen<-header[[paste0("channel",i)]]$additionaldatastructid1<-header[[paste0("channel",i)]]$additionaldatastructid2<-NULL
  }
  
  close(hdr); rm(hdr)
  
  ### Reorganize
  header$varname<-header$datatype<-header$fieldlen<-header$precision<-header$unit<-header$databytelen<-c()
  for (i in 1:header$channelcount) {
    header$varname[i]<-header[[paste0("channel",i)]]$varname
    header$datatype[i]<-header[[paste0("channel",i)]]$datatype
    header$fieldlen[i]<-header[[paste0("channel",i)]]$fieldlen
    header$precision[i]<-header[[paste0("channel",i)]]$precision
    header$unit[i]<-header[[paste0("channel",i)]]$unit
    header$databytelen[i]<-header[[paste0("channel",i)]]$databytelen
    header[[paste0("channel",i)]]<-NULL
  }
  rm(i)
  
  return(header)

}
