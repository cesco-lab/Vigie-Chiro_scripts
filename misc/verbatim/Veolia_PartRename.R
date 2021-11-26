library(data.table)

taDir="K:/Yves/Verbatim/1017_MNHN/PI"
pidone=read.csv2("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/pimport.csv")
#pidone=read.csv("C:/Users/calba.DEVICTOR-144/Documents/p_import.csv")

pilog=read.csv2('C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/pimport.log'
                ,h=F)

dta=list.files(taDir,full.names=T)
dta=subset(dta,substr(basename(dta),1,2)=="20")

pilog2=subset(pilog$V1,grepl("Participation creee",pilog$V1))

pidone=pidone[(1:min(length(dta),nrow(pidone))),]
dta=dta[(1:min(length(dta),nrow(pidone)))]


C1=(substr(pidone$date_debut,7,10)==substr(basename(dta),1,4))
C2=(substr(pidone$date_debut,4,5)==substr(basename(dta),6,7))
C3=(substr(pidone$date_debut,1,2)==substr(basename(dta),9,10))
C4=(as.numeric(substr(pidone$date_debut,1,2))==as.numeric(substr(basename(dta),9,10))+1)

test=min(C1)+min(C2)+min(pmax(C3,C4))
if(test==3)
{
  for (i in 1:length(pilog2))
  {
    
    NewParinfo=tstrsplit(pilog2[i],split=" ")
    #NewParinfo2=tstrsplit(NewParinfo[[6]],split="_")
    NewPari=NewParinfo[[4]]
    
    ftai=list.files(dta[i],full.names=T)
    newName=gsub(basename(dta[i]),NewPari,ftai)
    dir.create(paste0(taDir,"/",NewPari))
    file.rename(from=ftai,to=newName)
  }
}
