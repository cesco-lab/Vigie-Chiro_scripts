library(data.table)

Datedir="C:/Users/yvesb/Documents/VigieChiro/gbifData/DateSp"
#GroupSel=c("Geometridae"
 #          ,"Sphingidae"
  #         ,"Saturniidae",
   #        "Bombycidae",
    #       "Lasiocampidae"
     #      ,"Drepanidae"
      #     ,"Noctuidae","Erebidae"
       #    #,"Euteliidae"
        #   ,"Notodontidae")
GroupSel="Syrphidae"
Probas=fread("C:/Users/yvesb/Downloads/PTall_SyrphidAll.csv")
OutF="Dates_SyrphidAll.csv"


PTfiles=vector()
for (h in 1:length(GroupSel))
{
  PTh=list.files(Datedir,pattern=GroupSel[h],full.names=T,ignore.case=T)
  PTfiles=c(PTfiles,PTh)
}

DateData=list()
for (i in 1:length(PTfiles))
{
  DateData[[i]]=fread(PTfiles[i])
  
}
DateAll=rbindlist(DateData)
DateAll=unique(DateAll,by="ListSpValide")

Probas=unique(Probas,by="Espece")

DateProba=merge(DateAll,Probas,by.x="ListSpValide",by.y="Espece")

hist(DateProba$PicSp,breaks=70)
plot(DateProba$PicSp,DateProba$NDDL)
plot(DateProba$PicSp,DateProba$Sisteron)
plot(DateProba$NDDL,DateProba$Sisteron)

fwrite(DateProba,OutF,sep=";")

