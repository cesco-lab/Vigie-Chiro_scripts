library(data.table)
library(tools)
library(foreach)

ParDir="D:/PI_ressecs"
OutputDir="G:/upload_ressecs"
#irodsDir="C:\\wamp64\\www\\.icommands"
SiteLoc=fread("./www/sites_localites.txt")
irodsDest="/ccin2p3/home/ybas/transferts"
  
ListPar=dir(ParDir,full.names=T)
dir.create(OutputDir)
ListPar=subset(ListPar,!grepl("batch.txt",ListPar))

#check participations codes
llp=(nchar(basename(ListPar)))
if(mean(llp)!=24){
  stop("ERREUR : nom des sous-repertoires non conformes")
}

for (i in 1:length(ListPar))
{
  ListWav=list.files(ListPar[i],full.names=T,pattern=".wav$")
  ListWav2=list.files(ListPar[i],full.names=T,pattern=".WAV$")
  ListWtot=c(ListWav,ListWav2)
  ListWtot=ListWtot[order(basename(ListWtot))]
  ListBloc=dir(ListPar[i],full.names=T,pattern="bloc")
  
  if(length(ListWtot)>0)
    {
    SiteInfo=tstrsplit(basename(ListWtot),split="-")[[1]]
    if(length(unique(SiteInfo))>1)
    {
      stop(paste("ERREUR : plusieurs sites dans le repertoire"
                 ,basename(ListPar[i])))
    }
    Sitei=gsub("Car","Vigiechiro - Point Fixe-",SiteInfo[1])
    testSite=match(Sitei,SiteLoc$site)
    DirSite=SiteLoc$id_site[testSite]
    dir.create(paste0(OutputDir,"/",DirSite))
    
    
    
    dir.create(paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])))
    dir.create(paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                      ,"/wav"))
    
    TarGZtot=list.files(paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                               ,"/wav")
                        ,pattern=".tar.gz")
    #case 0 : dealing with failed last block
    if(length(ListBloc)>0)
      {
    Bnum=gsub("bloc","",basename(ListBloc))
    Bnum=as.numeric(Bnum)
    LastBloc=ListBloc[which.max(Bnum)]
    LlB=list.files(LastBloc)
    
    #case 1 : block uncomplete
    if(length(LlB)<100){
      NumGet=100-length(LlB)
    WaveGet=ListWtot[1:NumGet]
    WaveDest=paste0(LastBloc,"/",basename(WaveGet))
    test=file.rename(from=WaveGet,to=WaveDest) #0.6 sec
    if(mean(test)!=1){stop(paste("ERREUR : copie fichier wave non permise"))}
    LlB=list.files(LastBloc)
    TarName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                   ,"/wav/"
                   ,file_path_sans_ext(LlB[1])
                   ,".tar")
    if(file.exists(TarName)){stop("bug 67")}
    TarGZName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                     ,"/wav/"
                     ,file_path_sans_ext(LlB[1])
                     ,".tar.gz")
    TarGZtot=c(TarGZtot,basename(TarGZName))
    dir.create(dirname(TarName))
    #tar(TarName,files=Listj)
    #tar(TarGZName,files=Listj,compression="gzip")
    Sys.time()
    system(paste0("7z a -ttar ",TarName," ",LastBloc,"/*.*")) #6.5 secondes
    Sys.time()
    system(paste0("7z a -tgzip ",TarGZName," ",TarName)) #12.4 secondes
    Sys.time()
    
    
    
    }else{
#case 2 : block complete but not zipped
      TarName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                     ,"/wav/"
                     ,file_path_sans_ext(LlB[1])
                     ,".tar")
      if(!file.exists(TarName))
      {
        TarGZName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                         ,"/wav/"
                         ,file_path_sans_ext(basename(LlB[1]))
                         ,".tar.gz")
        TarGZtot=c(TarGZtot,basename(TarGZName))
        dir.create(dirname(TarName))
        #tar(TarName,files=Listj)
        #tar(TarGZName,files=Listj,compression="gzip")
        Sys.time()
        system(paste0("7z a -ttar ",TarName," ",LastBloc,"/*.*")) #6.5 secondes
        Sys.time()
        system(paste0("7z a -tgzip ",TarGZName," ",TarName)) #12.4 secondes
        Sys.time()
        
        
      }else{
        # case 3 : block complete and zipped
        stop("case 3")
        
      }
        }
    
    ListWav=list.files(ListPar[i],full.names=T,pattern=".wav$")
    ListWav2=list.files(ListPar[i],full.names=T,pattern=".WAV$")
    ListWtot=c(ListWav,ListWav2)
    ListWtot=ListWtot[order(basename(ListWtot))]
    }else{
      Bnum=0
    }
    
    testp=foreach (j = 1:ceiling(length(ListWtot)/100)) %do% {
      
      NumBloc=j+max(Bnum)
      Numj=c(((j-1)*100+1):min(j*100,length(ListWtot)))
      Listj=ListWtot[Numj]
      BlocDir=paste0(ListPar[i],"/bloc",NumBloc)
      if(dir.exists((BlocDir))){stop(paste("ERREUR : repertoire"
                                     ,ListPar[i],"deja traite"))}
      dir.create(BlocDir)
      Newj=paste0(BlocDir,"/",basename(Listj))
      Sys.time()
      test=file.rename(from=Listj,to=Newj) #0.6 sec
      if(mean(test)!=1){stop(paste("ERREUR : copie fichier wave non permise"))}
      Sys.time()
      #ecriture liste.txt
            TarName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                           ,"/wav/"
                     ,file_path_sans_ext(basename(Listj[1]))
                     ,".tar")
      TarGZName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                       ,"/wav/"
                       ,file_path_sans_ext(basename(Listj[1]))
                       ,".tar.gz")
      TarGZtot=c(TarGZtot,basename(TarGZName))
      dir.create(dirname(TarName))
      #tar(TarName,files=Listj)
      #tar(TarGZName,files=Listj,compression="gzip")
      Sys.time()
      system(paste0("7z a -ttar ",TarName," ",BlocDir,"/*.*")) #6.5 secondes
      Sys.time()
      system(paste0("7z a -tgzip ",TarGZName," ",TarName)) #12.4 secondes
      Sys.time()
      
          }
    Listei=data.frame(liste=TarGZtot)
    Listei=as.data.table(Listei)[order(Listei$liste),]
    fwrite(Listei,paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                         ,"/wav/liste.txt"),col.names=F)
    
  }#else{
    #stop(paste("ERREUR : repertoire",basename(ListPar[i]),"vide"))
    #}
  
}

#suppression des *.tar
ListTar=list.files(OutputDir,pattern=".tar$",recursive=T,full.names=T)
file.remove(ListTar)

#copie irods du contenu de OutputDir
ListFilesToCopy=list.files(OutputDir,recursive=T,full.names=T)
test1=grepl(".tar.gz",ListFilesToCopy)
test2=grepl("liste.txt",ListFilesToCopy)
if(mean(test1|test2)!=1){
  stop("ERREUR : fichiers indesirables dans OutputDir")
  }
ListWindows=gsub("/","\\\\",ListFilesToCopy)

TGZDest=gsub(OutputDir,irodsDest,ListFilesToCopy)


#iinit=paste0(irodsDir,"\\iinit")
#system(iinit)
Batch=vector()
DirToMake=unique(dirname(TGZDest))
DirToMake2=unique(dirname(DirToMake))
DirToMake3=unique(dirname(DirToMake2))
DirToMakeTot=c(DirToMake3,DirToMake2,DirToMake)
for (k in 1:length(DirToMakeTot))
{
  icom=paste0("imkdir ",DirToMakeTot[k])
  #icom=paste0("iput ",ListTGZ[k]," ",TGZDest[k])
  #system(paste0("icd ",irodsDir))
  #system(icom)
  Batch=c(Batch,icom)
  
}



for (k in 1:length(ListFilesToCopy))
{
icom=paste0("iput ",ListWindows[k]," ",TGZDest[k])
  #icom=paste0("iput ",ListTGZ[k]," ",TGZDest[k])
  #system(paste0("icd ",irodsDir))
#system(icom)
Batch=c(Batch,icom)

}

ListDir=unique(dirname(TGZDest))

for (l in 1:length(ListDir))
{
ichmod1=paste0("ichmod -r read mnhn ",ListDir[l])
ichmod2=paste0("ichmod -r write mnhn ",ListDir[l])
Batch=c(Batch,ichmod1)
Batch=c(Batch,ichmod2)
}
BatchDF=data.frame(instruc=Batch)
fwrite(BatchDF,paste0(OutputDir,"/batch.txt"),col.names=F)

