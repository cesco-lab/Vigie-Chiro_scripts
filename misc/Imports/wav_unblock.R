DirW="E:/PI_Brissac2109/par/6141ddd947c9b0c40f5bdcdb"

LW=list.files(DirW,pattern=".wav$",full.names=T,recursive=T)

NewName=paste0(dirname(dirname(LW)),"/",basename(LW))

file.rename(from=LW,to=NewName)

