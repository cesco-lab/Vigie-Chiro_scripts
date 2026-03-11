library(data.table)

CoefCorr=fread("C:/Users/ybas/Downloads/output/coef_correction.csv")

Lsp=unique(CoefCorr$species)

TrendCoef=vector()
Nsites=vector()
for (g in 1:length(Lsp)){
  Cg=subset(CoefCorr,CoefCorr$species==Lsp[g])
  print(Lsp[g])
  print(plot(Cg$coef_corr,main=Lsp[g]))
  print(Cg$n_sites)
  Nsites=c(Nsites,sum(Cg$n_sites))
  Cg$order=c(0:(nrow(Cg)-1))
  lm1=lm(coef_corr~order,data=Cg)
  print(summary(lm1)$coefficients[1,1])
  print(summary(lm1)$coefficients[2,1])
  Trendg=(summary(lm1)$coefficients[2,1])/(summary(lm1)$coefficients[1,1])*nrow(Cg)
  TrendCoef=c(TrendCoef,Trendg*100)
}
hist(TrendCoef,breaks=100)

Summary=data.frame(cbind(Lsp,TrendCoef,Nsites))
fwrite(Summary,"SummaryAnalysisBiasActivity.csv",sep=";") 

