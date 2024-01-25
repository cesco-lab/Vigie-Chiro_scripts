library(data.table)

TarDir="C:/Users/yvesb/Downloads/tarVC"
OutDir="D:/pourFormationOrthos2209"


ListTar=list.files(TarDir,full.names=T,pattern=".tar$")

for (i in 1:length(ListTar)){
  
  test=untar(ListTar[i],exdir=OutDir)
  if(test!=0){stop("erreur copie")}
}
