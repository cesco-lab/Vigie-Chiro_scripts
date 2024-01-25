library(data.table)

Particip=fread("p_export.csv")
MaxPointsPerSquare=6
CoefRepresentatif=10
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
tail(unique(SiteLoc$observateur),20)

YearAbandon=year(Sys.Date())-4
ParticipPF=subset(Particip,grepl("Fixe",Particip$site))

P_wdata=subset(ParticipPF,ParticipPF$nb_obs>0)

P_wdata$year=year(P_wdata$date_debut)
barplot(table(P_wdata$year))

LastYearData=aggregate(P_wdata$year,by=list(P_wdata$site),max)
barplot(table(LastYearData$x))

SiteAbandonnes=subset(LastYearData$Group.1,LastYearData$x<=YearAbandon)

P_abandon=subset(P_wdata,P_wdata$site %in% SiteAbandonnes)

P_abandon$month=month(P_abandon$date_debut)

P_abandonGP=subset(P_abandon,P_abandon$month %in% c(6:9))

P_abandonGP$period=(P_abandonGP$month<8)
table(P_abandonGP$period)

DataYearPeriod=aggregate(P_abandonGP$participation,by=c(list(P_abandonGP$site)
                                                        ,list(P_abandonGP$point)
                                                        ,list(P_abandonGP$year)
                                                        ,list(P_abandonGP$period))
                         ,length)
DataYearPeriod$PR=substr(DataYearPeriod$Group.2,1,1)!="Z"
table(DataYearPeriod$PR)


DataYearPeriod$score=ifelse(DataYearPeriod$PR
                            ,(year(Sys.Date())-as.numeric(DataYearPeriod$Group.3))*CoefRepresentatif
                            ,(year(Sys.Date())-as.numeric(DataYearPeriod$Group.3)))

hist(DataYearPeriod$score)

DataPoint=aggregate(DataYearPeriod$score,by=c(list(DataYearPeriod$Group.1)
                                              ,list(DataYearPeriod$Group.2))
                    ,sum)
DataPoint=subset(DataPoint,DataPoint$Group.2!="")
hist(DataPoint$x)

ScoreCarre=vector()
for (j in 1:length(unique(DataPoint$Group.1))){
  print(unique(DataPoint$Group.1)[j])
  Dataj=subset(DataPoint,DataPoint$Group.1==unique(DataPoint$Group.1)[j])
  Dataj=Dataj[order(Dataj$x,decreasing=T),]
  Scorej=sum(Dataj$x[1:min(nrow(Dataj),MaxPointsPerSquare)])
  ScoreCarre=c(ScoreCarre,Scorej)
}
hist(ScoreCarre,ylim=c(0,100),breaks=50)

CarresAbandonnes=data.frame(Carre=unique(DataPoint$Group.1),Interet=ScoreCarre)

# CarresAbandonnes$Carre=as.numeric(gsub("Vigiechiro - Point Fixe-",""
#                                        ,CarresAbandonnes$Carre))

SiteLocPF=subset(SiteLoc,SiteLoc$protocole=="POINT_FIXE")

SiteLat=aggregate(SiteLocPF$latitude,by=list(SiteLocPF$site),mean)
SiteLong=aggregate(SiteLocPF$longitude,by=list(SiteLocPF$site),mean)

testLL=match(CarresAbandonnes$Carre,SiteLat$Group.1)

CarresAbandonnes$Latitude_WGS84=SiteLat$x[testLL]
CarresAbandonnes$Longitude_WGS84=SiteLong$x[testLL]

testSL=match(CarresAbandonnes$Carre,SiteLoc$site)

CarresAbandonnes$Proprietaire=SiteLoc$observateur[testSL]

head(table(CarresAbandonnes$Proprietaire)[order(table(CarresAbandonnes$Proprietaire),decreasing=T)]
     ,10)

Summ=aggregate(CarresAbandonnes$Interet,by=list(CarresAbandonnes$Proprietaire),sum)

head(Summ[order(Summ$x,decreasing=T),])

CarresAbandonnes$URL=paste0("https://vigiechiro.herokuapp.com/#/sites/"
                            ,SiteLoc$id_site[testSL])

head(CarresAbandonnes)

fwrite(CarresAbandonnes,"CarresAbandonnes.csv",sep=";")
