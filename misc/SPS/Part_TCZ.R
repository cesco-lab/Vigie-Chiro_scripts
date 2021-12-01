library(data.table)

PartRP=fread("C:/Users/yvesb/Documents/www/ListParRP.csv")
CL1="pscp -pw PghgEESz1717! ybas@cca.in2p3.fr:/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/tcz/"
CL2=".tar.gz "
DirOut="C:/Users/yvesb/Documents/www/tczRP"

dir.create(DirOut)

for (i in 1:nrow(PartRP))
{
  Refi=PartRP$participation[i]
  ti=try(system(paste0(CL1,Refi,CL2,DirOut)))
  if(!(ti %in% c(0,1))){stop()}
  print(i)
}
