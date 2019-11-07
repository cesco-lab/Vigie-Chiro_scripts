library(data.table)

IdPart=fread("C:/Users/Yves Bas/Downloads/participation-5d1b2440090941000e9789d0-observations.csv")
IdPart$Dur=IdPart$temps_fin-IdPart$temps_debut

AggMax=aggregate(IdPart$tadarida_probabilite
                 ,by=list(IdPart$tadarida_taxon),FUN=max)

FileSel=vector()
for (i in 1:nrow(AggMax))
{
  Subtemp=subset(IdPart,(IdPart$tadarida_taxon==AggMax$Group.1[i])&
                   (IdPart$tadarida_probabilite==AggMax$x[i]))
  Subtemp=subset(Subtemp,Subtemp$Dur==max(Subtemp$Dur))
  FileSel=c(FileSel,Subtemp$`nom du fichier`[1])
  
}
fwrite(data.frame(FileSel),"FileSel.csv")
FileList=unique(IdPart$`nom du fichier`)
test=match(FileSel,FileList)
SelElargie=c(test-1,test,test+1)
SelElargie=unique(SelElargie)

FileSelElargie=FileList[SelElargie]
fwrite(data.frame(FileSelElargie),"FileSelElargie.csv")

IdPart$Sel=(IdPart$`nom du fichier` %in% FileSelElargie)

fwrite(IdPart,"IdPart.csv")
