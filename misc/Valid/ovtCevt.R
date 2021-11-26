library(data.table)

#DirExport="./VigieChiro/Raw"
Ovt=fread("C:/wamp64/www/ovt_ValidPar2021-04-13.csv")
EVT=fread("C:/wamp64/www/export_validtot210408.txt")

Ove=subset(Ovt,Ovt$espece=="empty")
Ovt=subset(Ovt,Ovt$espece!="empty")



#FileE=list.files(DirExport,pattern="export_",full.names=T)
#FileE=subset(FileE,grepl(".csv",FileE))

#for (i in 1:length(FileE))
#{
#DataE=fread(FileE[i])
#Suffixi=gsub("export_","",basename(FileE[i]))
#Suffixi=gsub(".csv","",Suffixi)
#Ovti=subset(Ovt,substr(Ovt$participation,1,3)==Suffixi)

#testR=match(paste(Ovti$participation,Ovti$donnee,Ovti$espece)
#         ,paste(DataE$participation,DataE$donnee,DataE$espece)
#)
testV=match(paste(Ovt$participation,Ovt$donnee,Ovt$espece)
            ,paste(EVT$participation,EVT$donnee,EVT$espece)
)
summary(testV)

test0=match("Car010257-2020-Pass1-D1-PaRecPR663312_20200713_220559_000"
            ,Ovt$donnee)
test00=match("Car010257-2020-Pass1-D1-PaRecPR663312_20200713_220559_000"
            ,EVT$donnee)
EVT[test00,]
Ovt[test0,]

Ovt$obs.espece=EVT$obs.espece[testV]
Ovt[is.na(Ovt)]=""
Ovt$obs.espece.new=ifelse(Ovt$obs.espece==""
                          ,Ovt$obs.espece.new,Ovt$obs.espece)
Ovt$obs.proba=EVT$obs.proba[testV]
Ovt[is.na(Ovt)]=""
Ovt$obs.proba.new=ifelse(Ovt$obs.proba==""
                         ,Ovt$obs.proba.new,Ovt$obs.proba)

Ovt$valid.espece=EVT$valid.espece[testV]
Ovt[is.na(Ovt)]=""


Ovt$valid.espece.new=ifelse(Ovt$valid.espece.new==""
                            ,Ovt$valid.espece,Ovt$valid.espece.new)
Ovt$valid.proba=EVT$valid.proba[testV]
Ovt[is.na(Ovt)]=""
Ovt$valid.proba.new=ifelse(Ovt$valid.proba.new==""
                            ,Ovt$valid.proba,Ovt$valid.proba.new)


Ovt_toupdate=subset(Ovt,(Ovt$valid.espece.new!=Ovt$valid.espece)|
                      (Ovt$valid.proba.new!=Ovt$valid.proba)|
                      (Ovt$obs.espece.new!=Ovt$obs.espece)|
                      (Ovt$obs.proba.new!=Ovt$obs.proba))
Ovt_toupdate=subset(Ovt_toupdate,(Ovt_toupdate$valid.espece.new!="")|
                                  (Ovt_toupdate$obs.espece.new!="")
                      )



#ALERTES
A1=subset(Ovt,Ovt$obs.espece.new!=Ovt$obs.espece)
A1=subset(A1,A1$obs.espece!="")
print(head(A1))
A2=subset(Ovt,Ovt$valid.espece.new!=Ovt$valid.espece)
A2=subset(A2,A2$valid.espece!="")
table(A2$valid.espece,A2$valid.espece.new)
print(head(A2))

#empty espece
testV=match(paste(Ove$participation,Ove$donnee,Ove$valid.espece.new)
            ,paste(EVT$participation,EVT$donnee,EVT$valid.espece)
)
summary(testV)
Ove_toupdate=subset(Ove,is.na(testV))

Ovt_toupdate=rbind(Ovt_toupdate,Ove_toupdate)
Ovt_toupdate=Ovt_toupdate[order(Ovt_toupdate$donnee),]

table(Ovt_toupdate$obs.espece.new)
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="barbar"]="Barbar"
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="chirosp"]="Chirosp"
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="myosp"]="Myosp"
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="pipkuh"]="Pipkuh"
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="pippip"]="Pippip"
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="pippyg"]="Pippyg"
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="Pipppip"]="Pippip"
Ovt_toupdate$obs.espece.new[Ovt_toupdate$obs.espece.new=="Rhisp"]="Rhisp."

Ovt_toupdate=unique(Ovt_toupdate,by=c("participation","donnee"
                                      ,"espece","valid.espece.new"))


fwrite(Ovt_toupdate,paste0("C:/wamp64/www/ovt_",Sys.Date(),".csv"),sep=";")

