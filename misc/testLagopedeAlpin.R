library(data.table)

FileList=list.files("F:/Lago2-pourscan/txt",full.names=T,pattern=".ta$")
FreqMPrange=c(1,3)
Durrange=c(3,10)
PrevMP2range=c(15,35)
Fminrange=c(0.7,2)
BWrange=c(1.1,4.5)
Fmaxrange=c(1.7,6.5)


FileSample=FileList[sample.int(length(FileList),10)]


DataLago=list()
for (i in 1:length(FileList))
{
  DataLago[[i]]=fread(FileList[i])
}
DataLagoTot=rbindlist(DataLago)

hist(DataLagoTot$FreqMP,breaks=300,xlim=c(0,6))
FMtest=(DataLagoTot$FreqMP>=FreqMPrange[1] & DataLagoTot$FreqMP<=FreqMPrange[2])
summary(FMtest)
boxplot(DataLagoTot$BW~FMtest)
boxplot(DataLagoTot$PrevMP2~FMtest,ylim=c(0,1000))

Durtest=(DataLagoTot$Dur>=Durrange[1] & DataLagoTot$Dur<=Durrange[2])
summary(Durtest)
boxplot(DataLagoTot$BW~Durtest)
table(Durtest,FMtest)

hist(subset(DataLagoTot$PrevMP2,FMtest&Durtest),breaks=3000,xlim=c(0,200))

PrevMP2test=(DataLagoTot$PrevMP2>=PrevMP2range[1] 
             & DataLagoTot$PrevMP2<=PrevMP2range[2])
summary(PrevMP2test)
boxplot(DataLagoTot$Fmin~PrevMP2test)
table(Durtest,FMtest,PrevMP2test)

Fmintest=(DataLagoTot$Fmin>=Fminrange[1] 
                     & DataLagoTot$Fmin<=Fminrange[2])
summary(Fmintest)
boxplot(DataLagoTot$BW~Fmintest)
table(Durtest,FMtest,PrevMP2test,Fmintest)

Fmaxtest=(DataLagoTot$Fmax>=Fmaxrange[1] 
          & DataLagoTot$Fmax<=Fmaxrange[2])
summary(Fmaxtest)
boxplot(DataLagoTot$BW~Fmaxtest)
table(Durtest,FMtest,PrevMP2test,Fmintest,Fmaxtest)

BWtest=(DataLagoTot$BW>=BWrange[1] 
          & DataLagoTot$BW<=BWrange[2])
summary(BWtest)
boxplot(DataLagoTot$Dur~BWtest)
table(Durtest,FMtest,PrevMP2test,Fmintest,Fmaxtest,BWtest)


AggEvents=aggregate(FMtest&Durtest&PrevMP2test&Fmintest&Fmaxtest&BWtest
                    ,by=list(DataLagoTot$Filename)
                    ,FUN=sum)
hist(AggEvents$x,xlim=c(0,12),breaks=100,ylim=c(0,1000))

fwrite(AggEvents,"AggEvents.csv",sep=";")

plot(AggEvents$x)
