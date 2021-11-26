library(data.table)

Obstypes=fread("Urandom2103.csv")

#over 1000 nights subset
Obs1000=subset(Obstypes,Obstypes$x.x>1000)
Agg1=aggregate(Obs1000$x.x,by=list(Obs1000$type),sum)
Agg2=aggregate(Obs1000$x.x,by=list(Obs1000$type),length)

#over 1000 nights subset
Obs1=subset(Obstypes,Obstypes$x.x<=1000)
TotalNights1=sum(Obs1$x.x)
TotalObs1=nrow(Obs1)
Obs1rec=subset(Obs1,Obs1$type!="")
Agg3=aggregate(Obs1rec$x.x,by=list(Obs1rec$type),sum)
Agg4=aggregate(Obs1rec$x.x,by=list(Obs1rec$type),length)
Agg3$x=Agg3$x*TotalNights1/sum(Agg3$x)
Agg4$x=Agg4$x*TotalObs1/sum(Agg4$x)

Agg13=rbind(Agg1,Agg3)
Agg13a=aggregate(Agg13$x,by=list(Agg13$Group.1),sum)
Agg24=rbind(Agg2,Agg4)
Agg24a=aggregate(Agg24$x,by=list(Agg24$Group.1),sum)

Agg13a$propdata=Agg13a$x/sum(Agg13a$x)*100
AggSupp=Agg13a
Agg13a$type="proportion of data"
AggSupp$type="proportion of participants"
AggSupp$propdata=Agg24a$x/sum(Agg24a$x)*100
AggToPlot=rbind(Agg13a,AggSupp)

p5<-ggplot(data=AggToPlot, aes(x=Group.1, y=propdata,fill=type)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("participant types")+
  ylab("%age")+
  theme(axis.text.x = element_text(  size=9, angle=90
                                   ,h=1))+
  coord_flip()
p5

#participation per involvement
Obstypes$involvement=ifelse(Obstypes$x.x>1000,"high"
                            ,ifelse(Obstypes$x.x<100,"low","moderate"))
Aggi=aggregate(Obstypes$x.x,by=list(Obstypes$involvement),sum)
Aggil=aggregate(Obstypes$x.x,by=list(Obstypes$involvement),length)
Aggi$prop=Aggi$x/sum(Aggi$x)
Aggil$prop=Aggil$x/sum(Aggil$x)

