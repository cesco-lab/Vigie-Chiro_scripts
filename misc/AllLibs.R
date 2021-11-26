FolderScripts="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts"
ListScripts=list.files(FolderScripts,full.names=T,pattern=".r$",recursive=T
                       ,ignore.case=T)

Libs=vector()
for (i in 1:length(ListScripts))
{
  TextS=try(fread(ListScripts[i],sep=";",h=F),silent=T)
  if(class(TextS)!="try-error")
  {
  LibTS=subset(TextS$V1,grepl("library",TextS$V1))
  Libs=c(Libs,LibTS)
  }
  if("library(readr)" %in% LibTS)
  {
    print(basename(ListScripts[i]))
  }
}
AgLibs=aggregate(Libs,by=list(Libs),length)

AgLibs=AgLibs[order(AgLibs$x,decreasing=T),]

fwrite(AgLibs,"AgLibs.csv",sep=";")
