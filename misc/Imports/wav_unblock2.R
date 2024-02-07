Dir="E:/PVAL_Rouet2107"

AllFiles=list.files(Dir,full.names=T,recursive=T)

Blocs=subset(AllFiles,grepl("bloc",basename(dirname(AllFiles))))

UnBlock=paste0(dirname(dirname(Blocs)),"/",basename(Blocs))

file.rename(from=Blocs,to=UnBlock)

AllDirs=list.dirs(Dir)

BlocDir=subset(AllDirs,grepl("bloc",basename(AllDirs)))

PartBlocDir=unique(dirname(BlocDir))

for (i in 1:length(BlocDir)){
  bfi=list.files(BlocDir[i])
  if(length(bfi)==0){
    test=unlink(BlocDir[i],force=T,recursive=T)
    
  }else{
    stop("pb dossier non vide")
  }
  
  
}
