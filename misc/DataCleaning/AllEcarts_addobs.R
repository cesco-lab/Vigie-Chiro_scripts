library(data.table)


AllEcart=fread("C:/Users/yvesb/Downloads/AllEcarts.csv",h=T)
Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv",h=T)


Shorts=AllEcart$`0`+AllEcart$`1`+AllEcart$`2`+AllEcart$`3`
All=apply(AllEcart[,2:ncol(AllEcart)],1,sum)
PropShorts=Shorts/All
hist(PropShorts)

TooShorts=subset(AllEcart,(PropShorts>0.8)&(All>10))
print(nrow(TooShorts))

AllEcart$TooShort=((PropShorts>0.8)&(All>10))
print(mean(AllEcart$TooShort))

RegularWmissing=AllEcart$`10`+AllEcart$`15`+AllEcart$`20`+AllEcart$`25`+AllEcart$`30`
AllLonger=apply(AllEcart[,12:ncol(AllEcart)],1,sum)
PropMissing=RegularWmissing/AllLonger
hist(PropMissing)

CheatKal=subset(AllEcart,(PropMissing>0.9)&(All>10))

CheatKal[sample.int(nrow(CheatKal),1),]

AllEcart$CheatKal=((PropMissing>0.9)&(All>10))

test=match(AllEcart$Var1,Particip$participation)

AllEcart$obs=Particip$observateur[test]
table(AllEcart$obs,AllEcart$CheatKal)

fwrite(AllEcart,"C:/Users/yvesb/Documents/VigieChiro/Raw/AllEcarts.csv",sep=";")
