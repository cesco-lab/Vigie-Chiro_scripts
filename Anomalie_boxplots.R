library(data.table)
FAT="AnomalieTemp.csv"
AnomalieTemp=fread(FAT)

AnomalieTemp$Month=substr(AnomalieTemp$V3,6,7)

AnomalieTemp$Year=substr(AnomalieTemp$V3,1,4)


for (i in 1:nlevels(as.factor(AnomalieTemp$Month)))
{
  ATTemp=subset(AnomalieTemp
                ,AnomalieTemp$Month==levels(as.factor(AnomalieTemp$Month))[i])
  boxplot(ATTemp$AT1~ATTemp$Year,main=ATTemp$Month[1])
}
