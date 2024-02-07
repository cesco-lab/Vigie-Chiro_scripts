library(data.table)

PT_pattern="45"
PTdir="C:/Users/ybas/Downloads/"
#DSD=fread("C:/Users/yvesb/Documents/VigieChiro/gbifData/DataSelDate/DataSelRare_Vocal_311_232.csv")
DSD=fread("DataDate.csv")
Pos=F
AddSpecies="Bubo scandiacus"
MaxPT=1
Thres=0

PTf=list.files(PTdir,pattern=paste0("PredTab_",PT_pattern),full.names=T,recursive=T)

PTz=subset(PTf,grepl(".zip",PTf))

PTf=subset(PTf,grepl(".csv",PTf))

if(length(PTf)<length(PTz)){
  for (r in 1:min(MaxPT,length(PTz))){
    unzip(PTz[r],exdir=PTdir)
  }
}

PTf=list.files(PTdir,pattern=paste0("PredTab_",PT_pattern),full.names=T,recursive=T)
PTf=subset(PTf,grepl(".csv",PTf))

print(PTf)

PTlist=list()
for (i in 1:min(MaxPT,length(PTf))){
  PTlist[[i]]=fread(PTf[i])
  
}
PT=rbindlist(PTlist,use.names=T)
#PT=fread("C:/Users/yvesb/Downloads/PredTab_85_Presence_07_GI_SysGrid_Radius_ 90000_50000.csv")
AddSpecies%in%names(PT)

summary(PT$`Lanius excubitor`)

if(Pos){
DSDpos=subset(DSD,DSD$nsp>0)
DSDpos=subset(DSDpos,DSDpos$sp %in% names(PT))
PTshort=subset(PT,select=c("Group.1","Group.2",DSDpos$sp,AddSpecies))
}else{
  DSDpos=DSD
  DSDpos=subset(DSDpos,DSDpos$species %in% names(PT))
  PTshort=subset(PT,select=c("Group.1","Group.2",DSDpos$species))
}
test=("Gavia immer" %in% DSDpos$species)

Speciesorder=names(PTshort)[order(names(PTshort))]

PTshort=subset(PTshort,select=c("Group.1","Group.2",Speciesorder))



PTsp=subset(PTshort,select=Speciesorder)

MaxScores=apply(PTsp,2,max)
SpThres=subset(Speciesorder,MaxScores>Thres)

PTshort=subset(PTshort,select=c("Group.1","Group.2",SpThres))

fwrite(PTshort,"PredTab_short.csv",sep=";")
AddSpecies%in%names(PTshort)

sample(names(PTshort))
