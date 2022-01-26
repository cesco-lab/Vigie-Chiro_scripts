DirW="C:/Users/yvesb/Documents/www/wavNycnocRP-direct"

ListW=list.files(DirW)

ListSE=gsub(".wav","",ListW)

DataW=data.frame(FilToD=ListSE)

fwrite(DataW,"DataW.csv",sep=";")
