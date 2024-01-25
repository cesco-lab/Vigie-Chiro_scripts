library(data.table)


SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")

Dep=c("59","62","80","76","14","50","35","22","29","56","44","85","17","33","40","64")

SLPF=subset(SiteLoc,grepl("Fixe",SiteLoc$site))

n=0
for (i in 1:length(Dep))
{
  SLi=subset(SLPF,grepl(paste0("Fixe-",Dep[i]),SLPF$site))
n=n+nrow(SLi)  
}

