op <- options(digits.secs = 3)
#sample(100)
set.seed(as.numeric(substr(Sys.time(),18,19))*as.numeric(substr(Sys.time(),21,23)))

NbHeure=rpois(100,1.52)
NbHeure=subset(NbHeure,NbHeure>0)

Sel=vector()
for (i in 1:(4*24))
{
  Proba=1/168/4
  Rand=sample(10000,1)
  #print(Rand)
  if(Rand<Proba*10000)
  {
    Sel=c(Sel,i)
  }
}
print(Sel)
for (i in ((4*24)+1):(7*24))
{
  Proba=1/168/4+1/72/4
  Rand=sample(10000,1)
  #print(Rand)
  if(Rand<Proba*10000)
  {
    Sel=c(Sel,i)
  }
}
print(Sel)
print(ceiling(Sel/24))
print(Sel-floor((Sel-1)/24)*24+3)
print(NbHeure[1:length(Sel)])

#bonus jour ferié
set.seed(as.numeric(substr(Sys.time(),18,19))*as.numeric(substr(Sys.time(),21,23)))
Selj=vector()
for (j in 1:24)
{
  {
    Proba=1/72/2
    Rand=sample(10000,1)
    #print(Rand)
    if(Rand<Proba*10000)
    {
      Selj=c(Selj,j)
    }
  }
}
print(Selj)
print(rpois(1,1.52))
