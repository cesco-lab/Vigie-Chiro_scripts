library(data.table)
PTCT=fread("TableTCtot_BonneVersion.csv")
SpInterest=c("Barbar","Confus","Cyrscu","Epheph","Eptser","Isopyr"
             ,"Leppun","Myodau","Myomys","Myonat","Nyclei","Nycnoc"
             ,"Phafal","Phanan","Phogri","Pipkuh","Pipnat","Pippip","Pippyg"
             ,"Plaalb","Pleaur","Pleaus","Rusnit","Testes","Tetvir","Urosp"
          )
nbParEsp=4
tabasetot=fread("RSDB_HF_tabase3HF_sansfiltre.csv")

Pref=substr(tabasetot$Filename,1,27)

PTCT$Annee=tstrsplit(PTCT$Group.1,"-")[[2]]
barplot(table(PTCT$Annee))

PTCTnew=subset(PTCT,as.numeric(PTCT$Annee)>2011)

PTCTnew$Pref=substr(PTCTnew$Group.1,1,27)
test=(PTCTnew$Pref %in% Pref)
PTCTnew=subset(PTCTnew,!test)

nlevels(as.factor(PTCTnew$participation))
PnE=subset(PTCTnew,PTCTnew$SR>400000)
PnD=subset(PTCTnew,PTCTnew$SR<400000)

test=subset(PnD,PnD$Group.1=="Cir12-2015-Pass2-Tron1-Chiro_0_00175_000")

PselE=PnE[0,]
for (i in 1:length(SpInterest))
{
  Scorei=subset(PnE,select=SpInterest[i])
  Refmax=max(Scorei)
  for (j in 1:nbParEsp)
  {
    Psub=subset(PnE,(as.data.frame(Scorei)[,1]>(j/(nbParEsp+1)*Refmax))
                     &(as.data.frame(Scorei)[,1]<=((j+1)/(nbParEsp+1)*Refmax)))
    if(nrow(Psub)>0)
    {
    Psel=Psub[sample.int(nrow(Psub),1),]
    PselE=rbind(PselE,Psel)
    }
    }
  print(i)
}
fwrite(PselE,"PselE.csv",sep=";")


PselE$Group.1=gsub(".wav","",PselE$Group.1)
PselE$F0=gsub("_1_","_0_",PselE$Group.1)
PselE$F1=gsub("_0_","_1_",PselE$Group.1)
PselE$FileD=ifelse(PselE$Group.1==PselE$F0,PselE$F1,PselE$F0)
FileE=paste0(PselE$participation,";",PselE$Group.1)
FileD=paste0(PselE$participation,";",PselE$FileD)
Files=c(FileE,FileD)
Files=Files[order(Files)]
fwrite(as.data.frame(Files),"FileSelENew.csv",sep=";")

PrefE=substr(PselE$Group.1,1,27)

test=(PnD$Pref %in% PrefE)
PnD=subset(PnD,!test)

PselD=PnD[0,]
for (i in 1:length(SpInterest))
{
  Scorei=subset(PnD,select=SpInterest[i])
  Refmax=max(Scorei)
  for (j in 1:nbParEsp)
  {
    Psub=subset(PnD,(as.data.frame(Scorei)[,1]>(j/(nbParEsp+1)*Refmax))
                &(as.data.frame(Scorei)[,1]<=((j+1)/(nbParEsp+1)*Refmax)))
    if(nrow(Psub)>0)
    {
      Psel=Psub[sample.int(nrow(Psub),1),]
      PselD=rbind(PselD,Psel)
    }
  }
  print(i)
}
fwrite(PselD,"PselD.csv",sep=";")


PselD$Group.1=gsub(".wav","",PselD$Group.1)
PselD$F0=gsub("_1_","_0_",PselD$Group.1)
PselD$F1=gsub("_0_","_1_",PselD$Group.1)
PselD$FileD=ifelse(PselD$Group.1==PselD$F0,PselD$F1,PselD$F0)
FileE=paste0(PselD$participation,";",PselD$Group.1)
FileD=paste0(PselD$participation,";",PselD$FileD)
Files=c(FileE,FileD)
Files=Files[order(Files)]
fwrite(as.data.frame(Files),"FileSelDNew.csv",sep=";")


