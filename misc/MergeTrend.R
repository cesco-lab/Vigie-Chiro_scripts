library(data.table)
TrendFolder="./output/MAJ200515Total"
#TrendFolder="./output/Tron50_wAnom_woint"
Annual=T

FLinearTrends=list.files(TrendFolder,pattern="tendanceGlobalEspece",recursive=T
                         ,full.names=T)

DataLT=list()
for (i in 1:length(FLinearTrends))
{
  FI=file.info(FLinearTrends[i])
  if(FI$size>10)
  {
    DataLT[[i]]=fread(FLinearTrends[i],integer64="numeric")
    #print(dim(DataLT[[i]]))
    #print(paste(i,class(as.data.frame(DataLT[[i]])[1,14])))
  }
}
LT=rbindlist(DataLT)
LT$pourcentage_variation=(LT$tendance^(LT$derniere_annee-LT$premiere_annee)-1)*100
#for (j in 1:floor(nrow(LT)/10))
barplot(LT$tendance,names.arg=LT$espece,las=2,cex.names=0.6)

if(Annual)
{
FAnnualTrends=list.files(TrendFolder,pattern="GLMalphatest_VarAnFY",recursive=T
                         ,full.names=T)

FAnnualTrends=subset(FAnnualTrends,grepl("_Coefs",FAnnualTrends))


DataLT=list()
for (i in 1:length(FAnnualTrends))
{
  FI=file.info(FAnnualTrends[i])
  if(FI$size>10)
  {
    DataLT[[i]]=fread(FAnnualTrends[i],integer64="numeric")
    #print(dim(DataLT[[i]]))
    EspSF=substr(tstrsplit(FAnnualTrends[i],split="/")[[4]]
                 ,11,nchar(tstrsplit(FAnnualTrends[i],split="/")[[4]]))
    DataLT[[i]]$espece=EspSF
  }
}
AT=rbindlist(DataLT)
#for (j in 1:floor(nrow(LT)/10))
AT=subset(AT,grepl("year",AT$term))
AT$espece_code=tstrsplit(AT$espece,split="_")[[2]]
table(AT$espece,AT$espece_code)

AT$yearMean=NA
AT$yearRange=NA
for (j in 1:nrow(AT))
{
  ListYear=gsub("year_as_factor","",AT$term[j])
  ListSplit=tstrsplit(ListYear,split="_")
  VectorSplit=unlist(ListSplit)
  AT$yearMean[j]=mean(as.numeric(VectorSplit)) 
  AT$yearRange[j]=max(as.numeric(VectorSplit))-min(as.numeric(VectorSplit)) 
}

ATnew=data.frame()
espece=levels(as.factor(AT$espece_code))
TEW=vector()
TRaw=vector()
for (i in 1:nlevels(as.factor(AT$espece)))
{
  ATe=subset(AT,AT$espece==levels(as.factor(AT$espece))[i])
  StandEstimates=exp(ATe$Estimate)/exp(ATe$Estimate[1])
  Standsdl=exp(ATe$Estimate-ATe$Std..Error)/exp(ATe$Estimate[1])
  Standsdu=exp(ATe$Estimate+ATe$Std..Error)/exp(ATe$Estimate[1])
  ATe$StandEstimates=StandEstimates
  ATe$Standsdl=Standsdl
  ATe$Standsdu=Standsdu
  ATnew=rbind(ATnew,ATe)
  ATe$yearScaled=scale(ATe$yearMean)
  me=lm(StandEstimates~ATe$yearScaled)
  TendancesEW=round(summary(me)$coefficients[2,1]/sd(ATe$yearMean)*
                      (max(ATe$yearMean)-min(ATe$yearMean))*100)
  TEW=c(TEW,TendancesEW)
  LTe=subset(LT,LT$espece==levels(as.factor(AT$espece))[i])
  TendancesLin=round(LTe$pourcentage_variation)
  TendancesRaw=round(((StandEstimates[length(StandEstimates)])-1)*100)
  TRaw=c(TRaw,TendancesRaw)
  Title=paste(levels(as.factor(AT$espece))[i]
              ,TendancesLin,TendancesRaw,TendancesEW,"%")
  #barplot(StandEstimates,main=Title)
  #estimate with equal weight for each years
  plot(ATe$yearMean,StandEstimates,main=Title,type="l")
  
  
}

#PC=aggregate(AT$Estimate,by=list(AT$year),FUN=mean)
#plot(PC$Group.1,PC$x)
fwrite(ATnew,paste0(dirname(TrendFolder),"/AT_",basename(TrendFolder),".csv"),sep=";")
OtherTrends=data.frame(espece,TEW,TRaw)
LTnew=merge(LT,OtherTrends,by="espece",all.x=T)
}else{
  LTnew=LT
}
fwrite(LTnew,paste0(dirname(TrendFolder),"/LT_",basename(TrendFolder),".csv"),sep=";")
