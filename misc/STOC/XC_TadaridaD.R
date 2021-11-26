DirToTreat=dir("D:/test",pattern="wn",full.names=T)
Parallel=4
DirPath="D:/Tadarida-D_forWindows/install"

for (i in 1:length(DirToTreat))
{
  #LW=list.files(DirToTreat[i],full.names=T,pattern=".wav$")
  #for (j in 1:length(LW))
  #{
    shell(cmd=paste0(DirPath,"/TadaridaD -t "
          ,Parallel," -x 1 -f 2 "     ,DirToTreat[i]))
  #}
}

