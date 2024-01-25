library(data.table)

Rdata="C:/Users/yvesb/Downloads/graph/models_VC_direct.rdata"

load(Rdata)


for (i in 1:length(models_VC_direct))
{
 dataSpi=models_VC_direct[[i]] 
 Trendsi=dataSpi$betabinomial$GLMCat$value
  test=predict(dataSpi$betabinomial$GLMCat)
}
