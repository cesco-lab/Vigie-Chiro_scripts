library(data.table)
library(StreamMetabolism)

SpNuit=fread("C:/wamp64/www/SpNuit2Valid_DI_0_DataLP_PF_exportTot.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.csv")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
OutF="C:/wamp64/www/DurEnr.csv"

testS=match(paste(Particip$site,Particip$point)
            ,paste(SiteLoc$site,SiteLoc$nom))
testS2=match(paste(Particip$site)
            ,paste(SiteLoc$site))


testS=ifelse(is.na(testS),testS2,testS)
Plost=subset(Particip,is.na(testS))
head(Plost$participation)
Particip=subset(Particip,!is.na(testS))
testS=match(paste(Particip$site,Particip$point)
            ,paste(SiteLoc$site,SiteLoc$nom))
testS2=match(paste(Particip$site)
             ,paste(SiteLoc$site))
testS=ifelse(is.na(testS),testS2,testS)

     
     
Debut=aggregate(SpNuit$decalage_debut_coucher,by=c(list(SpNuit$participation)
                                                 ,list(SpNuit$Nuit)),min)
Fin=aggregate(SpNuit$decalage_fin_lever,by=c(list(SpNuit$participation)
                                                 ,list(SpNuit$Nuit)),min)
FinC=aggregate(SpNuit$decalage_fin_coucher,by=c(list(SpNuit$participation)
                                             ,list(SpNuit$Nuit)),min)
#FinC est bugué, ne pas prendre en compte !!!

testP=match(Debut$Group.1,Particip$participation)
Dlost=subset(Debut,is.na(testP))
Debut=subset(Debut,!is.na(testP))
Fin=subset(Fin,!is.na(testP))
FinC=subset(FinC,!is.na(testP))

testP=match(Debut$Group.1,Particip$participation)
Debut$longitude=SiteLoc$longitude[testS[testP]]
Debut$latitude=SiteLoc$latitude[testS[testP]]
summary(Debut$longitude)

test=subset(Debut,is.na(Debut$latitude))

Sys.time()
LLJour=cbind(Debut$latitude,Debut$longitude,Debut$Group.2) # 5 min
Sys.time()
#DateSrst=format(as.Date(LLJour[,3],origin = "1970-01-01"),format="%Y/%m/%d")
DateLP=format(as.Date(LLJour[,3]),format="%Y-%m-%d")

test=subset(Debut,is.na(LLJour[,1]))

Sys.time()
Srst=mapply(sunrise.set,as.numeric(LLJour[,1]),as.numeric(LLJour[,2]),DateLP) #50 sec
Sys.time()
SrstD=as.data.frame(t(Srst))

Debut$NightDur=3600*24+(as.numeric(SrstD$sunrise)-
                    as.numeric(SrstD$sunset))



Debut$x=ifelse(Debut$x>(12*3600),Debut$x-12*3600,Debut$x)
Fin$x=ifelse(Fin$x>(12*3600),Fin$x-12*3600,Fin$x)
#FinC$x=ifelse(FinC$x<0,FinC$x+24*3600,FinC$x)
Debut$nuit_complete=((Debut$x+Fin$x)<7200)
summary(Debut$nuit_complete)

Debut$debut_enregistrement=Debut$x/3600
Debut$fin_enregistrement_lever=Fin$x/3600
Debut$fin_enregistrement=FinC$x/3600
Debut$duree_enregistrement=ifelse(Debut$nuit_complete,Debut$NightDur/3600
                                  ,Debut$NightDur/3600-
                                    ifelse(Debut$debut_enregistrement>0
                                           ,Debut$debut_enregistrement,0)-
                                    ifelse(Debut$fin_enregistrement_lever>0
                                           ,Debut$fin_enregistrement_lever,0))

hist(Debut$duree_enregistrement)
OutOfNight=subset(Debut,Debut$duree_enregistrement<=0)
Samp=sample(nrow(OutOfNight),1)
OutOfNight[Samp,]

fwrite(Debut,OutF,sep=";")


Incomplete=subset(Debut,!Debut$nuit_complete)
Samp=sample(nrow(Incomplete),1)
Incomplete[Samp,]

IncompleteLong=subset(Incomplete,Incomplete$duree_enregistrement>15)
Samp=sample(nrow(IncompleteLong),1)
IncompleteLong[Samp,]


test=subset(Debut,Debut$duree_enregistrement>25)
head(test)
boxplot(Debut$duree_enregistrement~Debut$nuit_complete)
test=subset(Debut,(Debut$duree_enregistrement<5)&(Debut$nuit_complete))
head(test)

Sptest=subset(SpNuit,(SpNuit$participation=="5f20110ce87fa40011780acf")&
                (SpNuit$Nuit=="2020-07-20"))
plot(Sptest$min_decalage_coucher,Sptest$min_decalage_lever)
plot(Sptest$min_decalage_coucher,Sptest$min_decalage_lever)
Sptest$min_decalage_coucher
Sptest$decalage_fin_coucher
Sptest$decalage_fin_coucher
etest=subset(export,export$participation=="5aa8c9324bdc33000dbeb52a")

