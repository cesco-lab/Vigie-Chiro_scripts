library(data.table)
library(warbleR)

Perf=fread("C:/Users/Yves Bas/Downloads/PerfSp_2020-08-28_1913.csv")

Perf$Recording_ID=Perf$Group.1
Perf$genus=tstrsplit(Perf$Species,split=" ")[[1]]
Perf$specific_epithet=tstrsplit(Perf$Species,split=" ")[[2]]

#DataXC=querxc(X=Perf,download=F)
MDXC=data.frame()
for (i in 1:nrow(Perf))
{
test=querxc(paste0("nr:",Perf$Recording_ID[i]),download=F)
MDXC=rbindlist(list(MDXC,test),use.names=T,fill=T)
print(i)
}

Perf=cbind(Perf,MDXC)
Perf$song=grepl("song",Perf$Vocalization_type)|grepl("drum",Perf$Vocalization_type)


Perfsum=aggregate(Perf$x,by=list(Perf$Species),mean)
Perfsum2=aggregate(Perf$Rate,by=list(Perf$Species),mean)
Perfsum$x2=Perfsum2$x

fwrite(Perfsum,paste0("Perfsum_",Sys.Date(),".csv"),sep=";")

PerfsumCS=aggregate(Perf$x,by=c(list(Perf$Species),list(Perf$song)),mean)
PerfsumCS2=aggregate(Perf$Rate,by=c(list(Perf$Species),list(Perf$song)),mean)
PerfsumCS$x2=PerfsumCS2$x
fwrite(PerfsumCS,paste0("PerfsumCS_",Sys.Date(),".csv"),sep=";")
boxplot(PerfsumCS$x~PerfsumCS$Group.2)
boxplot(PerfsumCS$x2~PerfsumCS$Group.2)
