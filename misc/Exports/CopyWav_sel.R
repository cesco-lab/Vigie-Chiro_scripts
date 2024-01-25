library(data.table)
DataSel=fread("FilToD_221110.csv")
#DataSel="FilToD_221110"
#IdSel=fread("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/IdSel.csv")
WavDir="C:/Users/yvesb/Downloads/pourFormation2211"
Newdir=("C:/Users/yvesb/Downloads/pourFormation2211sel")
Move=F

WavFiles=list.files(WavDir,full.names=T,pattern=".wav$",recursive=T)

#IdSelnew=subset(IdSel,!IdSel$Group.1 %in%
#                  basename(WavFiles))
#fwrite(IdSelnew,"C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/IdSelNew.csv")


if(!is.data.table(DataSel)){
  Ldata=list.files(DataSel,full.names=T,pattern=".csv$")
  for (i in 1:length(Ldata)){
    
    TableSeli=fread(Ldata[i])
    TableSeliS=subset(TableSeli,TableSeli$sel=="x")
    testCopy=match(paste0(TableSeliS$'nom du fichier',".wav"),basename(WavFiles))
    
    summary(is.na(testCopy))
    MissingFiles=subset(TableSeliS$'nom du fichier',is.na(testCopy))
    head(MissingFiles)
    testC2=subset(testCopy,!is.na(testCopy))
    testCtot=c(testC2,testC2-1,testC2+1)
    testCtot=unique(testCtot)
    DataCopy=WavFiles[testCtot]
    
    DataCopy=DataCopy[order(DataCopy)]
    
    
    dir.create(Newdir)
    NewLoc=paste0(Newdir,"/",basename(DataCopy))
    #PrevLoc=paste0(WavDir,"/",basename(DataCopy))
    if(Move){ 
      file.rename(DataCopy,NewLoc)
    }else{
      file.copy(DataCopy,NewLoc)
      
    }
  }
  
  
}else{
  
  
  
  #testCopy=match(basename(WavFiles)
  #               ,paste0(DataSel$donnee,".wav"))
  testCopy=match(paste0(DataSel$donnee,".wav"),basename(WavFiles))
  
  summary(is.na(testCopy))
  MissingFiles=subset(DataSel$donnee,is.na(testCopy))
  head(MissingFiles)
  testC2=subset(testCopy,!is.na(testCopy))
  testCtot=c(testC2,testC2-1,testC2+1)
  testCtot=unique(testCtot)
  DataCopy=WavFiles[testCtot]
  
  DataCopy=DataCopy[order(DataCopy)]
  
  
  dir.create(Newdir)
  NewLoc=paste0(Newdir,"/",basename(DataCopy))
  #PrevLoc=paste0(WavDir,"/",basename(DataCopy))
  
  if(Move){ 
    file.rename(DataCopy,NewLoc)
  }else{
    file.copy(DataCopy,NewLoc)
    
  }
  
}
