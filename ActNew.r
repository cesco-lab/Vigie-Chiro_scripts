library(data.table)
SED="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/SelExportDonnees.r"
EPD="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/extr_PF_DataLP.r"
EPA="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/extr_PF_ActNuit.r"

PP=c("55","56","57","58","59","5a","5b","5c","5d")

#PP=c("5b")


args="Valid"
Thresholds=c(90,50)
args[10]="G:/VigieChiro/Raw/New"
args[12]=""
args[13]=""
args[14]=""
FRaw="G:/VigieChiro/Raw"
FDT="C:/wamp64/www/export_190803_190907.txt"


args[3]=Thresholds[1]

source(SED) #to save validated data

for (i in 1:length(PP))
{
  print(paste(i,Sys.time()))
  args[1]="PrefPart"
  
  for (j in c(c(0:9),"a","b","c","d","e","f"))
  {
    args[2]=paste0(PP[i],j)
    print(args[2])
    Sys.time()
    source(SED) 
    Sys.time()
    args[11]=paste0(args[10],"/export_",args[2],".csv")
    testsize=file.size(args[11])
    if(testsize>200)
    {
      
      source(EPD) # 3e5 donnees/min
      Sys.time()
      args[4]=paste0(args[10],"/DataLP_PF_export_",args[2],".csv")
      if(file.exists(args[4]))
      {
        
        source(EPA) # 1e6 donnees/min
        Sys.time()
      }
    }
  }
}


#update complete files
Fnew1=list.files(args[10],pattern="export_",full.names=T)
Fnew1=subset(Fnew1,substr(basename(Fnew1),1,1)=="e")
for (i in 1:length(Fnew1))
{
  TabNew=fread(Fnew1[i])
  if(nrow(TabNew)>0)
  {
    PartNew=levels(as.factor(TabNew$participation))
    TabOld=fread(paste0(dirname(args[10]),"/",basename(Fnew1[i])))
    TabPurge=subset(TabOld,!(TabOld$participation %in% PartNew))
    TabUpdate=rbind(TabPurge,TabNew)
    fwrite(TabUpdate,paste0(dirname(args[10]),"/",basename(Fnew1[i])))
    print(basename(Fnew1[i]))
    print(nrow(TabNew))
    print(Sys.time())
    
  }
}

Fnew1=list.files(args[10],pattern="DataLP",full.names=T)
Fnew1=subset(Fnew1,substr(basename(Fnew1),1,1)=="D")
for (i in 108:length(Fnew1))
{
  TabNew=fread(Fnew1[i])
  if(nrow(TabNew)>0)
  {
    PartNew=levels(as.factor(TabNew$participation))
    OldFile=paste0(dirname(args[10]),"/",basename(Fnew1[i]))
    if(file.exists(OldFile))
    {
      TabOld=fread(OldFile)
      TabPurge=subset(TabOld,!(TabOld$participation %in% PartNew))
      TabUpdate=rbind(TabPurge,TabNew)
    }else{
      TabUpdate=TabNew
    }
    fwrite(TabUpdate,paste0(dirname(args[10]),"/",basename(Fnew1[i])))
    print(basename(Fnew1[i]))
    print(nrow(TabNew))
    print(Sys.time())
    
  }
}

Fnew1=list.files(args[10],pattern="SpNuit",full.names=T)
for (i in 1:length(Fnew1))
{
  TabNew=fread(Fnew1[i])
  if(nrow(TabNew)>0)
  {
    PartNew=levels(as.factor(TabNew$participation))
    OldFile=paste0(dirname(args[10]),"/",basename(Fnew1[i]))
    if(file.exists(OldFile))
    {
      TabOld=fread(OldFile)
      TabPurge=subset(TabOld,!(TabOld$participation %in% PartNew))
      TabUpdate=rbind(TabPurge,TabNew)
    }else{
      TabUpdate=TabNew
    }
    fwrite(TabUpdate,paste0(dirname(args[10]),"/",basename(Fnew1[i])))
    print(basename(Fnew1[i]))
    print(nrow(TabNew))
    print(Sys.time())
    
  }
}




Pattern=basename(paste0(args[10],"/SpNuit2___",args[3],"_DataLP_PF_export_"))

SpToAgg=list.files(args[10],pattern=Pattern,full.names=T)

my.data=list()
for (k in 1:length(SpToAgg))
{
  my.data[[k]]=fread(SpToAgg[k])
}
ActTot=rbindlist(my.data)
fwrite(ActTot,paste0(FRaw,"/SpNuit2_",args[3],"_DataLP_PF_exportTot.csv"))

if(length(Thresholds)>1)
{
  for (h in 2:length(Thresholds))
  {
    args[3]=Thresholds[h]
    for (i in 1:length(PP))
    {
      print(paste(i,Sys.time()))
      args[1]="PrefPart"
      
      for (j in c(c(0:9),"a","b","c","d","e","f"))
      {
        args[2]=paste0(PP[i],j)
        print(args[2])
        args[4]=paste0(args[10],"/DataLP_PF_export_",args[2],".csv")
        if(file.exists(args[4]))
        {
          source(EPA) # 1e6 donnees/min
          Sys.time()
        }
      }
    }
    
    Fnew1=list.files(args[10],pattern="SpNuit",full.names=T)
    for (i in 1:length(Fnew1))
    {
      TabNew=fread(Fnew1[i])
      if(nrow(TabNew)>0)
      {
        PartNew=levels(as.factor(TabNew$participation))
        OldFile=paste0(dirname(args[10]),"/",basename(Fnew1[i]))
        if(file.exists(OldFile))
        {
          TabOld=fread(OldFile)
          TabPurge=subset(TabOld,!(TabOld$participation %in% PartNew))
          TabUpdate=rbind(TabPurge,TabNew)
        }else{
          TabUpdate=TabNew
        }
        fwrite(TabUpdate,paste0(dirname(args[10]),"/",basename(Fnew1[i])))
        print(basename(Fnew1[i]))
        print(nrow(TabNew))
        print(Sys.time())
        
      }
    }
    
    
    Pattern=basename(paste0(args[10],"/SpNuit2___",args[3],"_DataLP_PF_export_"))
    
    SpToAgg=list.files(args[10],pattern=Pattern,full.names=T)
    
    my.data=list()
    for (k in 1:length(SpToAgg))
    {
      my.data[[k]]=fread(SpToAgg[k])
    }
    ActTot=rbindlist(my.data)
    fwrite(ActTot,paste0(FRaw,"/SpNuit2_",args[3],"_DataLP_PF_exportTot.csv"))
  }
}

