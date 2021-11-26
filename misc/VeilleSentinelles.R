library(readxl)
library(data.table)
library(xlsx)
FNew="C:/Users/yvesb/Downloads/Signalements.xlsx"
FOld=c("./natura/veille/Veille2019-11-22.xlsx",
  "./natura/veille/Veille - 2019-07-22.xlsx"
  ,"C:/Users/yvesb/Downloads/signalements-2020-02-09.xlsx"
  ,"./natura/veille/Signalements.xlsx"
  ,"./natura/veille/Signalements (1).xlsx"
  ,"./natura/veille/Signalements (3).xlsx"
  ,"./natura/veille/Signalements (4).xlsx")
  
NameSelect=c("N° de dossier",
             "Latitude" ,                   "Longitude"                     
             , "Département"            ,     "Ville"                      
             ,"Action(s) portant atteinte" 
            ,                     "Milieu(x) concerné(s)"      
             ,"Bassin versant"              , "Observation sentinelle"
            ,"Longitude","Latitude")


New=read_xlsx(FNew,col_types="text")

NewY=subset(New,New$`Émise par`=="Yves Bas")

OldData=list()
for (i in 1:length(FOld))
{
OldData[[i]]=read_xlsx(FOld[i])
}
Old=rbindlist(OldData,use.names=T,fill=T)

for (i in 1:nrow(Old))
{
  Oldi=as.data.frame(Old)[i,]
  Newi=subset(New,New$`N° de dossier`==Oldi$`N° de dossier`)
  if(nrow(Newi)>0)
  {
    #Oldi$`Observation sentinelle`=gsub("\r","",Oldi$`Observation sentinelle`)
    if(is.na(Newi$`Observation sentinelle`))
    {
      NewY=subset(NewY,NewY$`N° de dossier`!=Newi$`N° de dossier`)
    }else{
    if(Oldi$`Observation sentinelle`==Newi$`Observation sentinelle`)
    {
      NewY=subset(NewY,NewY$`N° de dossier`!=Newi$`N° de dossier`)
    }
    }
  }
}

NewSynth=subset(NewY,select=NameSelect)

NewSynth=NewSynth[order(NewSynth$Département),]
NameSynth=paste0("Veille",substr(Sys.time(),1,10),".xlsx")

write.xlsx(NewSynth,NameSynth)
#NameSynthCSV=paste0("Veille",substr(Sys.time(),1,10),".csv")
#fwrite(NewSynth,NameSynthCSV)
