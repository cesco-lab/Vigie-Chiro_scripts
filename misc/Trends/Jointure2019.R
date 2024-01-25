library(data.table)


I1=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/IndicatorForONB_2020-06-22.csv")
I2=fread("IndicatorForONB_2022-11-03.csv")
YearChange=2019


IAll=subset(I1,I1$year<=YearChange)
I2YC=subset(I2,I2$year==YearChange)
Ratio=IAll$WeightedIndicator[nrow(IAll)]/I2YC$WeightedIndicator
I2Add=subset(I2,I2$year>YearChange)
I2Add$WeightedIndicator=I2Add$WeightedIndicator*Ratio
I2Add$LowInt=I2Add$LowInt*Ratio
I2Add$UpInt=I2Add$UpInt*Ratio

IAll=rbind(IAll,I2Add)
IAll=subset(IAll,select=c("year","WeightedIndicator","LowInt","UpInt"))

gg=ggplot(IAll,aes(year,WeightedIndicator))+
  geom_line()+
  geom_pointrange(aes(ymin=LowInt, ymax=pmin(UpInt)))+
  ylim(0,max(UpInt))+
  ggtitle("Indicateur ONB Chauves-souris")+
  labs(x="Annee",y="")

print(gg)

fwrite(IAll,paste0("IndicatorCombinedForONB_",Sys.Date(),".csv"),sep=";")
