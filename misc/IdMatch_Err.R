library(data.table)

IdMatch=fread("IdMatchD.csv")

table(IdMatch$validateur_probabilite)
IdSur=subset(IdMatch,IdMatch$validateur_probabilite==1)

IdSur$TadErr=(IdSur$SpMax2!=IdSur$validateur_taxon)
table(IdSur$validateur_taxon,IdSur$TadErr)


IdSur$year=tstrsplit(IdSur$Group.1,"-")[[2]]
table(IdSur$year)
m1=glm(IdSur$TadErr~as.numeric(IdSur$year),family="binomial")
plot(as.numeric(IdSur$year),predict(m1,type="response"))

ErrYear=aggregate(IdSur$TadErr,by=list(IdSur$year),mean)
plot(ErrYear$Group.1,ErrYear$x)

for (i in 1:nlevels(as.factor(IdSur$validateur_taxon)))
{
  IdSp=subset(IdSur
              ,IdSur$validateur_taxon==
                levels(as.factor(IdSur$validateur_taxon))[i])
  IdE=subset(IdSp,IdSp$TadErr)
  if(nrow(IdE)>0)
  {
  ColSp=match(levels(as.factor(IdSur$validateur_taxon))[i]
              ,colnames(IdE))
  ScoreSp=subset(IdE,select=ColSp)
  hist(as.data.frame(ScoreSp)[,1],breaks=50
             ,main=levels(as.factor(IdSur$validateur_taxon))[i])
  print(levels(as.factor(IdSur$validateur_taxon))[i])
  print(table(IdE$SpMaxF2))
  print(table(IdE$SpMax2))
  }
  msp=glm(IdSp$TadErr~as.numeric(IdSp$year),family="binomial")
  print(plot(as.numeric(IdSp$year),predict(msp,type="response")
       ,main=levels(as.factor(IdSur$validateur_taxon))[i]))
  ErrYear=aggregate(IdSp$TadErr,by=list(IdSp$year),mean)
  print(plot(ErrYear$Group.1,ErrYear$x
       ,main=levels(as.factor(IdSur$validateur_taxon))[i])
  )
  
  
  
}

#plot evolution of amplitude
for (i in 1:nlevels(as.factor(IdSur$validateur_taxon)))
{
  IdSp=subset(IdSur
              ,IdSur$validateur_taxon==
                levels(as.factor(IdSur$validateur_taxon))[i])
  msp=glm(log(IdSp$Ampm90)~as.numeric(IdSp$year))
  print(plot(as.numeric(IdSp$year),predict(msp,type="response")
             ,main=levels(as.factor(IdSur$validateur_taxon))[i]))
  ErrYear=aggregate(log(IdSp$Ampm90),by=list(IdSp$year),mean)
  print(plot(ErrYear$Group.1,ErrYear$x
             ,main=levels(as.factor(IdSur$validateur_taxon))[i])
  )
  
  
  
}

#plot evolution of duration
for (i in 1:nlevels(as.factor(IdSur$validateur_taxon)))
{
  IdSp=subset(IdSur
              ,IdSur$validateur_taxon==
                levels(as.factor(IdSur$validateur_taxon))[i])
  #hist(IdSp$Duree)
  IdSp$pedestre=((substr(IdSp$Group.1,5,5)!="-")&(substr(IdSp$Group.1,10,10)=="-"))
  summary(IdSp$pedestre)
  msp=glm(IdSp$Duree~as.numeric(IdSp$year))
  print(plot(as.numeric(IdSp$year),predict(msp,type="response")
             ,main=levels(as.factor(IdSur$validateur_taxon))[i]))
  ErrYear=aggregate(IdSp$Duree,by=list(IdSp$year),mean)
  print(plot(ErrYear$Group.1,ErrYear$x
             ,main=levels(as.factor(IdSur$validateur_taxon))[i])
  )
  
  
  
}

