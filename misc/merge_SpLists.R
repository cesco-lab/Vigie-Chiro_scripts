library(data.table)
spl1=fread("SpeciesList.csv")
spl2=fread("./Tadarida/SpeciesList_Norfolk.csv")


test=match(spl1$Esp,spl2$Esp)

Norfolk1=spl1$Norfolk

Norfolk2=spl2$Norfolk[test]
#Norfolk2[is.na(Norfolk2)]=""

test2=(Norfolk1==Norfolk2)
Esp_diff=subset(spl1$Esp,!test2)
subset(spl2)

SpNew=subset(spl2,!(spl2$Esp %in% spl1$Esp))
SpNew