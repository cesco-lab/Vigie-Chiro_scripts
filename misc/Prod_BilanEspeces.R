library(data.table)
#A EDITER en fonction de la localisation des fichiers .tc
#TC_path="C:/Users/Yves Bas/Documents/txt"
#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
#uncomment the following line if you prefer to do not use R in command line
if(length(args)==0){
  args="C:/Users/yvesb/Documents/www/622dbfd75b54ea479697aaba"
  #q()
}
TC_path=args[1]
SpeciesList=fread("C:/Users/yvesb/Documents/Tadarida/Tadarida-C/tadaridaC_src/other_inputs/SpeciesList.csv")
#DataCalibr=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/DataCalibr.csv")
ListClasses=fread("ListesClasses.csv")

Ref_coef=subset(SpeciesList,SpeciesList$Esp %in% ListClasses$Esp)


alogit <- function(x) 
{
  exp(x)/(1+exp(x))
}

#get the .tc files list
idlist=list.files(TC_path,pattern=".tc$",full.names=T,recursive=T)
print(length(idlist))
my.data <- list()
for(f in 1:length(idlist)) {
  my.data[[f]] <- fread(idlist[[f]])
}
Idtot=as.data.frame(rbindlist(my.data))

SpMax<-max.col(Idtot[,40:(ncol(Idtot)-2)],ties.method = "first")
ScoreMax=apply(Idtot[,40:(ncol(Idtot)-2)],MARGIN=1,FUN=max)
SpMax2=as.data.frame(cbind(Filename=as.character(Idtot[,1]),Id=colnames(Idtot)[SpMax+39],numsp=SpMax,IndConf=ScoreMax))


SpMax3=SpMax2
SpMax3$IndConf=as.numeric(as.character(SpMax3$IndConf))

ListSp=vector()  
AbBrute=vector()
Score=vector()
for (i in 1:nlevels(as.factor(SpMax3$Id)))
{
  Datasub=subset(SpMax3,(SpMax3$Id==levels(as.factor(SpMax3$Id))[i])
                 &(as.numeric(as.character(SpMax3$IndConf))>0.5))
  ListSp=c(ListSp,levels(as.factor(SpMax3$Id))[i])
  AbBrute=c(AbBrute,nrow(Datasub))
  Score=c(Score,max(Datasub$IndConf))
}


  
BilanEspeces0=as.data.frame(cbind(ListSp,AbBrute,Score)) 
#BilanEspeces1=merge(BilanEspeces0,Ref_coef,by.x="ListSp",by.y="Esp",all.y=T)
BilanEspeces1=merge(BilanEspeces0,Ref_coef,by.x="ListSp",by.y="Esp",all.y=T)

#BilanEspeces1=BilanEspeces0
BilanEspeces1$AbBrute=as.character(BilanEspeces1$AbBrute)
BilanEspeces1$AbBrute[is.na(BilanEspeces1$AbBrute)]=0
BilanEspeces1$Score[is.na(BilanEspeces1$Score)]=0
BilanEspeces1$Score[BilanEspeces1$Score<0.5]=0
BilanEspeces=as.data.frame(cbind(Groupe=BilanEspeces1$GroupFR
                                 ,Espece=BilanEspeces1$NomFR
                                 ,Nb_Contacts=BilanEspeces1$AbBrute
                                 ,Score=BilanEspeces1$Score))
BilanEspeces=BilanEspeces[order(as.character(BilanEspeces$Groupe),BilanEspeces$Espece),]

write.csv(BilanEspeces,paste(TC_path, "/BilanEspeces.csv", "", sep=""),row.names=F)
}
