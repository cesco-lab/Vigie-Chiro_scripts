PowerIntLog=c(1/c(10:2),c(1:10)) #for power function
#PowerIntLog=c(-10:10) #for log function
#norm & scale
Power=vector()

for (i in 1:length(VarSimple))
{
  Vinit=(SpNuit_SLPAGN)[,VarSimple[i]]
  if(min(Vinit)!=max(Vinit))
  {
    Vmin=min(Vinit)
    VPos=Vinit-Vmin
    VLog=vector()
    #Stat=vector()
    #PVal=vector()
    M1=vector()
    M2=vector()
    for (j in 1:length(PowerIntLog))
    {
      LogTemp=VPos^PowerIntLog[j]
      #LogTemp=log10(VPos+10^PowerIntLog[j])
      VLog=cbind(VLog,LogTemp)
      #hist(LogTemp,main=PowerIntLog[j])
      #test=ks.test(scale(LogTemp),"pnorm")
      #Stat=c(Stat,test$statistic)
      #PVal=c(PVal,test$p.value)
      LogTS=scale(LogTemp)
      #if(quantile(LogTS,0.75)!=quantile(LogTS,0.25))
      #{
      #M1=c(M1,abs(quantile(LogTS,0.75)+quantile(LogTS,0.25))/(quantile(LogTS,0.75)-quantile(LogTS,0.25)))
      #}else{
      #  M1=c(M1,999)
      #}
      M2=c(M2,abs(max(LogTS)+min(LogTS))/(max(LogTS)-min(LogTS)))
      #print(j)
      #print(summary(LogTemp))
      #qqnorm(LogTemp,main=j)
    }
    
    Power=c(Power,PowerIntLog[which.min(M2)])
    
  }else{
    Power=c(Power,999)
  }
  print(VarSimple[i])
  print(Power[i])
  print(Sys.time())
}

#if(sd(M1)!=0){
#  M3=scale(M1)+scale(M2)
#}else{
#  M3=M2
#}

