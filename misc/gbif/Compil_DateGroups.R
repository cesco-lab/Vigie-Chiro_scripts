library(data.table)
library(Hmisc)

PTdir="C:/Users/yvesb/Downloads/PredTab"
Pattern=c("_ 90000_50000","180000_150000_50000")
Coordinates=data.frame(latitude=c(44.17288059276691, 43.826362)
                       ,longitude=c(5.928680674168401, 3.769733))
Coordnames=c("Sisteron","NDDL")
LSdir="C:/Users/yvesb/Documents/VigieChiro/gbifData/ListSp"
GroupSel=c("Geometridae"
           ,"Sphingidae"
           ,"Saturniidae",
           "Bombycidae",
           "Lasiocampidae")

OutF="PTall_MacroHetero1.csv"


PTfiles=vector()
for (h in 1:length(Pattern))
{
  PTh=list.files(PTdir,pattern=Pattern[h],full.names=T)
  PTfiles=c(PTfiles,PTh)
}


LSall=vector()
for (i in 1:length(GroupSel))
{
  LSfilei=list.files(LSdir,pattern=GroupSel[i],full.names=T,ignore.case =T)
  LSdata=list()
  for(j in 1:length(LSfilei))
  {
    LSdata[[j]]=fread(LSfilei[j])
  }
  LSi=rbindlist(LSdata)
  LSall=c(LSall,LSi$species)  
}
LSall=unique(LSall)

ListDate=unique(tstrsplit(basename(PTfiles),split="_")[[2]])
DataDate=(tstrsplit(basename(PTfiles),split="_")[[2]])


PTall=data.frame()
for (k in 1:length(ListDate))
{
  
  PTk=subset(PTfiles,DataDate==ListDate[k])
  PTkdata=list()
  for (l in 1:length(PTk))
  {
    PTkdata[[l]]=fread(PTk[l])
  }
  PTkall=rbindlist(PTkdata,use.names=T,fill=T)
  CoordMatch=find.matches(Coordinates,cbind(PTkall$Group.2,PTkall$Group.1)
                          ,tol=c(0.02,0.02),maxmatch=1)
  PTksel=PTkall[CoordMatch$matches,]
  Namek=subset(LSall,LSall %in% names(PTksel))
  PTksel2=subset(PTksel,select=Namek)
  PTksel3=t(PTksel2)
  PTksel4=as.data.frame(PTksel3)
  names(PTksel4)=Coordnames
  PTksel4$Espece=row.names(PTksel4)  
  PTall=rbind(PTall,PTksel4)
  print(paste0(ListDate[k],nrow(PTksel4)))
}

fwrite(PTall,OutF,sep=";")

