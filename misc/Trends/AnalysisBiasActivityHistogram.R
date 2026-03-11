library(data.table)


Years=c(2014:2024)
Pheno110=fread("C:/Users/ybas/Downloads/Phenosynth110.csv")
DataTrend=fread("C:/Users/ybas/Downloads/data03_SR_PF_idManual_2025-05-20_142645_cleanmic.csv")

Lpart=unique(DataTrend,by=c("participation","nuit"))
Lpart$nuit2=paste(Lpart$participation,Lpart$nuit)
Pheno110$nuit2=paste(Pheno110$participation,Pheno110$Nuit)
match8=match(Pheno110$nuit2,Lpart$nuit2)
PhenoTrend=subset(Pheno110,!is.na(match8))

Lsp=unique(PhenoTrend$espece)

Act=vector()
Year=vector()
Species=vector()
PicAll=vector()
RatioAll=vector()
CoefCorrAll=vector()
for (n in 1:length(Lsp)){
  print(Lsp[n])
  Pn=subset(PhenoTrend,PhenoTrend$espece==Lsp[n])
  Pn$year=year(Pn$Nuit)
  table(Pn$year)
  Ly=unique(Pn$year)
  for (u in 1:length(Ly)){
    Species=c(Species,Lsp[n])
    Year=c(Year,Ly[u])
    Pny=subset(Pn,Pn$year==Ly[u])
    Pnpheno=Pny[,5:114]
    AllAct=sum(Pnpheno)
    Act=c(Act,AllAct)
    ActHist=apply(Pnpheno,sum,MARGIN = 2)
    Pic=max(ActHist)
    PicAll=c(PicAll,Pic)
    Ratio=Pic/AllAct
    CoefCorr=AllAct/Pic
    CoefCorrAll=c(CoefCorrAll,CoefCorr)
    RatioAll=c(RatioAll,Ratio)
  }
}
Summary=data.frame(Species, Year
                         ,Act,PicAll
                         ,RatioAll,CoefCorrAll)
summary(Summary)
#Summary$CoefCorrection=1/Summary$RatioAll
fwrite(Summary,"SummaryHistPheno.csv",sep=";")


Summary=subset(Summary,Summary$Year %in% Years)


TrendCoef=vector()
Nsites=vector()
for (g in 1:length(Lsp)){
  Cg=subset(Summary,Summary$Species==Lsp[g])
  Cg=Cg[order(Cg$Year),]
  print(Lsp[g])
  print(plot(Cg$CoefCorrAll,main=Lsp[g]))
  #print(Cg$n_sites)
  #Nsites=c(Nsites,sum(Cg$n_sites))
  Cg$order=c(0:(nrow(Cg)-1))
  lm1=lm(CoefCorrAll~order,data=Cg)
  print(summary(lm1)$coefficients[1,1])
  print(summary(lm1)$coefficients[2,1])
  Trendg=(summary(lm1)$coefficients[2,1])/(summary(lm1)$coefficients[1,1])*nrow(Cg)
  print(Trendg)
  TrendCoef=c(TrendCoef,Trendg*100)
}
hist(TrendCoef,breaks=1000,xlim=c(-50,200))

Summary2=data.frame(Lsp,TrendCoef)
fwrite(Summary2,"SummaryAnalysisBiasActivityHistogram.csv",sep=";") 



