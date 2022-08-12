


folder_manager<-function(diretoria_dados,key_folders_without_subfolders=c("BRUT","COMP","AUXI","SSI","FAST","ANAL","TRIG"),key_folders_with_subfolders=c("PREP","PROC")) {
  
  
  key_folders<-c(key_folders_without_subfolders,key_folders_with_subfolders)
  folders<-folders_aux<-folders_refs<-list(); 
  
  #*** Loop -------------------------------------------------------------
  for (i in 1:20) {
    #****** List dirs and subdirs -------------------------------------------------------------
    if (i==1) {
      folders_aux[[i]]<-list.dirs(diretoria_dados,recursive=F)
    } else {
      tempvec1<-c()
      for (j in 1:length(folders_aux[[i-1]])) {tempvec1<-c(tempvec1,list.dirs(folders_aux[[i-1]][j],recursive=F))}
      folders_aux[[i]]<-tempvec1
    }
    #****** Place each dir into each key folder -------------------------------------------------------------
    which_array<-c()
    for (j in 1:length(key_folders)) {
      which_aux<-which(regexpr(pattern=key_folders[j],folders_aux[[i]])>=0)
      if (key_folders[j] %in% key_folders_without_subfolders) {which_array<-c(which_array,which_aux)}
      folders[[key_folders[j]]]<-c(folders[[key_folders[j]]],folders_aux[[i]][which_aux])
    }
    #****** Remove those without subfolder from auxiliary list (auxiliary step) -------------------------------------------------------------
    if (length(which_array)!=0) {folders_aux[[i]]<-folders_aux[[i]][-which_array]}
  }
  #*** Remove those which do not have subfolders from those which have subfolders -------------------------------------------------------------
  for (i in 1:length(key_folders_with_subfolders)) {
    matrixTF<-matrix(NA,ncol=length(key_folders_without_subfolders),nrow=length(folders[[key_folders_with_subfolders[i]]]))
    for (j in 1:length(key_folders_without_subfolders)) {
      matrixTF[,j]<-regexpr(pattern=key_folders_without_subfolders[j],folders[[key_folders_with_subfolders[i]]])
    }
    matrixTF<-matrixTF>0
    vecTFaux<-c(); for (i in 1:nrow(matrixTF)) {vecTFaux[i]<-any(matrixTF[i,])}
    folders[[key_folders_with_subfolders[i]]]<-folders[[key_folders_with_subfolders[i]]][!vecTFaux]
  }
  #*** Remove keyfolders names from refs -------------------------------------------------------------
  folders_refs<-folders
  for (i in 1:length(folders)) {
    for (j in 1:length(folders[[i]])) {
      folders_refs[[i]][j]<-folders[[i]][j]
      for (k in 1:length(key_folders)) {
        folders_refs[[i]][j]<-gsub(paste0("/",key_folders[k]),"",folders_refs[[i]][j])
      }
      folders_refs[[i]][j]<-gsub(diretoria_dados,"",folders_refs[[i]][j])
      folders_refs[[i]][j]<-substr(folders_refs[[i]][j],2,1000)
      folders_refs[[i]][j]<-gsub("/"," - ",folders_refs[[i]][j])
    }
  }
  #*** Remove duplicates with larger len from ref ---------------------------------------------------------------------------------------
  for (i in 1:length(folders)) {
    folders_temp<-c()
    vec_names<-names(table(folders_refs[[i]])[folders_refs[[i]]])
    for (j in 1:length(vec_names)) {
      folders_with_same_sufix<-folders[[i]][which(folders_refs[[i]]==vec_names[j])]
      folders_temp<-c(folders_temp,folders_with_same_sufix[which.min(nchar(folders_with_same_sufix))])
    }
    folders[[i]]<-folders_temp
  }
  #*** Remove duplicates from folders_refs ---------------------------------------------------------------------
  for (i in 1:length(folders)) {
    folders_refs[[i]]<-folders_refs[[i]][!duplicated(folders_refs[[i]])]
  }
  #*** Output -------------------------------------------------------------
  return(list(folders=folders,folders_refs=folders_refs))
}
