library(data.table)
library(tools)
library(foreach)

ParDir="E:/SONS/A_TRIER/MINES_MAZAUGUES/WAV/A_FORMATTER"
OutputDir="E:/SONS/A_TRIER/upload"
#irodsDir="C:\\wamp64\\www\\.icommands"
#SiteLoc=fread("C:/Users/Arthur/kDrive/Shared/BOITE A OUTIL/TADARIDA/sites_localites.txt")
#irodsDest="/ccin2p3/home/ybas/transferts"

#FACTORISATION A PREVOIR

ListSite=dir(ParDir,full.names=T)
dir.create(OutputDir,showWarnings = F)


for (h in 1:length(ListSite))
{
  ListPar=dir(ListSite[h],full.names=T)
  
  #check participations codes
  llp=(nchar(basename(ListPar)))
  if(mean(llp)!=24){
    stop("ERREUR : nom des sous-repertoires non conformes")
  }
  
  for (i in 1:length(ListPar))
  {
    ListWtot=list.files(ListPar[i],full.names=T,pattern=".wav$"
                        ,ignore.case = T)
    ListWtot=ListWtot[order(basename(ListWtot))]
    ListRec=list.files(ListPar[i],full.names=T,recursive=T,pattern=".wav$"
                       ,ignore.case = T)
    ListBloc=dir(ListPar[i],full.names=T,pattern="bloc")
    DirSite=basename(dirname(ListPar[i]))
    dir.create(paste0(OutputDir,"/",DirSite),showWarnings = F)
    dir.create(paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i]))
               ,showWarnings = F)
    dir.create(paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                      ,"/wav"),showWarnings = F)
    
    TarGZtot=list.files(paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                               ,"/wav")
                        ,pattern=".tar")
    
    if(length(ListRec)>0)
    {
      
      if(length(ListBloc)>0)
      {
        #case -1 : blocks created but not tars
        if(length(TarGZtot)==0)
        {
          #case -1A : blocks all created
          if(length(ListWtot)==0){
            for (z in 1:length(ListBloc))
            {
              Wavz=list.files(ListBloc[z],full.names=T)
              if(length(Wavz)>0){
                TarName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                               ,"/wav/"
                               ,file_path_sans_ext(basename(Wavz[1]))
                               ,".tar")
                
                TarGZtot=c(TarGZtot,basename(TarName))
                dir.create(dirname(TarName),showWarnings = F)
                #tar(TarName,files=Listj)
                #tar(TarGZName,files=Listj,compression="gzip")
                Sys.time()
                system(paste0("7z a -ttar ",TarName," ",ListBloc[z]
                              ,"/*.*")) #65 secondes
                Sys.time()
              }else{
                stop("rare case not coded yet: empty block")
              }
            }
            Listei=data.frame(liste=TarGZtot)
            Listei=as.data.table(Listei)[order(Listei$liste),]
            fwrite(Listei,paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                                 ,"/wav/liste2.txt"),col.names=F)
            
          }else{
            stop("rare case not coded yet: no tars and incomplete blocks")
          }
        }else{
          #case 0 : dealing with failed last block
          
          Bnum=gsub("bloc","",basename(ListBloc))
          Bnum=as.numeric(Bnum)
          LastBloc=ListBloc[which.max(Bnum)]
          LlB=list.files(LastBloc)
          
          #case 1 : block uncomplete
          if(length(LlB)<1000){
            NumGet=1000-length(LlB)
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
            #TarGZName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
            #                ,"/wav/"
            #               ,file_path_sans_ext(LlB[1])
            #              ,".tar.gz")
            TarGZtot=c(TarGZtot,basename(TarName))
            dir.create(dirname(TarName))
            #tar(TarName,files=Listj)
            #tar(TarGZName,files=Listj,compression="gzip")
            Sys.time()
            system(paste0("7z a -ttar ",TarName," ",LastBloc,"/*.*")) #6.5 secondes
            Sys.time()
            #  system(paste0("7z a -tgzip ",TarGZName," ",TarName)) #12.4 secondes
            # Sys.time()
            
            
            
          }else{
            #case 2 : block complete but not zipped
            #TarName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
            #              ,"/wav/"
            #             ,file_path_sans_ext(LlB[1])
            #            ,".tar")
            TarName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                           ,"/wav/"
                           ,file_path_sans_ext(basename(LlB[1]))
                           ,".tar")
            
            if(!file.exists(TarName))
            {
              TarGZtot=c(TarGZtot,basename(TarName))
              dir.create(dirname(TarName))
              #tar(TarName,files=Listj)
              #tar(TarGZName,files=Listj,compression="gzip")
              Sys.time()
              system(paste0("7z a -ttar ",TarName," ",LastBloc,"/*.*")) #6.5 secondes
              Sys.time()
              #system(paste0("7z a -tgzip ",TarGZName," ",TarName)) #12.4 secondes
              #Sys.time()
              
              
            }else{
              test=length(untar(TarName,list=T))
              if(test<1000){ #tar uncomplete
                Sys.time()
                system(paste0("7z a -ttar ",TarName," ",LastBloc,"/*.*")) #6.5 secondes
                Sys.time()
                
              }
              # case 3 : block complete and zipped
              #stop("case 3")
              
            }
          }
          
          ListWav=list.files(ListPar[i],full.names=T,pattern=".wav$")
          ListWav2=list.files(ListPar[i],full.names=T,pattern=".WAV$")
          ListWtot=c(ListWav,ListWav2)
          ListWtot=ListWtot[order(basename(ListWtot))]
        }
      }else{
        Bnum=0
      }
      
      if(length(ListWtot)>0){
        testp=foreach (j = 1:ceiling(length(ListWtot)/1000)) %do% {
          
          NumBloc=j+max(Bnum)
          Numj=c(((j-1)*1000+1):min(j*1000,length(ListWtot)))
          Listj=ListWtot[Numj]
          BlocDir=paste0(ListPar[i],"/bloc",NumBloc)
          if(dir.exists((BlocDir))){stop(paste("ERREUR : repertoire"
                                               ,ListPar[i],"deja traite"))}
          dir.create(BlocDir)
          Newj=paste0(BlocDir,"/",basename(Listj))
          Sys.time()
          test=file.rename(from=Listj,to=Newj) #6 sec
          if(mean(test)!=1){stop(paste("ERREUR : copie fichier wave non permise"))}
          Sys.time()
          #ecriture liste.txt
          TarName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                         ,"/wav/"
                         ,file_path_sans_ext(basename(Listj[1]))
                         ,".tar")
          #TarGZName=paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
          #                ,"/wav/"
          #               ,file_path_sans_ext(basename(Listj[1]))
          #              ,".tar.gz")
          TarGZtot=c(TarGZtot,basename(TarName))
          dir.create(dirname(TarName))
          #tar(TarName,files=Listj)
          #tar(TarGZName,files=Listj,compression="gzip")
          Sys.time()
          system(paste0("7z a -ttar ",TarName," ",BlocDir,"/*.*")) #65 secondes
          Sys.time()
          #system(paste0("7z a -tgzip ",TarGZName," ",TarName)) #12.4 secondes
          #Sys.time()
          
        }
        Listei=data.frame(liste=TarGZtot)
        Listei=as.data.table(Listei)[order(Listei$liste),]
        fwrite(Listei,paste0(OutputDir,"/",DirSite,"/",basename(ListPar[i])
                             ,"/wav/liste2.txt"),col.names=F)
      }
    }
  }#else{
  #stop(paste("ERREUR : repertoire",basename(ListPar[i]),"vide"))
  #}
  
}

#suppression des *.tar
#ListTar=list.files(OutputDir,pattern=".tar$",recursive=T,full.names=T)
#file.remove(ListTar)


