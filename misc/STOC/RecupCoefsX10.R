library(data.table)

NewParam=fread("LabelsAudio_YvesBas.csv")
OldParam=fread("RSDB_HF_tabase3HF_sansfiltre.csv")
NewParam$Reftemps=NewParam$StTime/10
NewParam$Reffreq=NewParam$FreqMP*10

test=merge(NewParam,OldParam,by.x=c("Filename","CallNum")
           ,by.y=c("Filename","CallNum"))
head(test$Filename)
table(test$Filename)

test2=subset(OldParam,OldParam$Filename=="o_3769_20140722_065256_392.wav")
test2$StTime
test2$FreqMP
test3=subset(NewParam,NewParam$Filename=="o_3769_20140722_065256_392.wav")
test3$StTime
test3$Reftemps
test3$Reffreq


param=vector()
coef=vector()
for (i in 4:275)
{
  j=i+287
  ratio=test[,..i]/test[,..j]
    coef=c(coef,quantile(as.data.frame(ratio)[,1],0.5,na.rm=T))
  param=c(param,names(NewParam)[i])
}
Coeftable=data.frame(param,coef)
fwrite(Coeftable,"CoeftableX10.csv",sep=";")
