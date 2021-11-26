library(data.table)
library(ggplot2)

AT=fread("./output/AT_Maj200515Total.csv")

for (i in 1:length(unique(AT$espece)))
{
  ATi=subset(AT,AT$espece==unique(AT$espece)[i])
  gg=ggplot(ATi,aes(yearMean,StandEstimates))+
    geom_line()+
    geom_pointrange(aes(ymin=Standsdl, ymax=pmin(Standsdu,max(ATi$StandEstimates*1.3))))+
    ylim(0,max(ATi$StandEstimates*1.3))+
  ggtitle(unique(AT$espece)[i])+
    labs(x="Année",y="")
  
       print(gg)
  }

