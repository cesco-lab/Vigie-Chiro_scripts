library(data.table)

#DataT=fread("C:/Users/yvesb/Downloads/DataTrends.csv")
DataT=fread("DataTrends.csv")

summary(DataT$julian)


names(DataT)
#boxplot(DataT$temps_enr~DataT$protocole)

DataPF=subset(DataT,DataT$protocole=="POINT_FIXE")

#DataPF$Hour=hour(DataPF$date_debut)
DataPF$Hour=substr(DataPF$date_debut,12,13)
table(DataPF$Hour)
barplot(table(DataPF$Hour))

test23=subset(DataPF,DataPF$Hour=="23")
head(test23$participation)

hist(DataPF$julian,breaks=360)
summary(DataPF$julian)

#janvier-février
mean(DataPF$julian<60) #2.7%
#janvier-mars
mean(DataPF$julian<91) #2.6%
#avril
mean((DataPF$julian<121)&(DataPF$julian>90)) #5.9%
#mai
mean((DataPF$julian<152)&(DataPF$julian>120)) #7.3%
#juin-septembre
mean((DataPF$julian<274)&(DataPF$julian>151)) #69.8%
#octobre
mean((DataPF$julian<305)&(DataPF$julian>273)) #7.3%
#novembre
mean((DataPF$julian<335)&(DataPF$julian>303)) #2.8%
#décembre
mean((DataPF$julian>304)) #1.6%




test=subset(DataPF,DataPF$temps_enr==min(DataPF$temps_enr))

test$participation
test$date_debut
names(test)

test=subset(DataPF,DataPF$temps_enr<20000)
LparPb=unique(test$participation)

DataPb=subset(DataPF,DataPF$participation %in% LparPb)

DataPb_lj=aggregate(DataPb$julian,by=c(list(DataPb$participation),list(DataPb$julian)),length)
DataPb_nbj=aggregate(DataPb_lj$x,by=list(DataPb_lj$Group.1),length)
summary(DataPb_nbj)

DataPF_lj=aggregate(DataPF$julian,by=c(list(DataPF$participation),list(DataPF$julian)),length)
DataPF_nbj=aggregate(DataPF_lj$x,by=list(DataPF_lj$Group.1),length)
summary(DataPF_nbj)


test=subset(DataPF_nbj,DataPF_nbj$x==1)
