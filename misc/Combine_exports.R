library(data.table)

Flist=c("C:/wamp64/www/export_190907_190924.txt"
           ,"C:/wamp64/www/export_190924_191121.txt")
NewName="C:/wamp64/www/export_190907_191121.txt"

my.data=list()
for (i in 1:length(Flist))
{
  my.data[[i]]=fread(Flist[i])
}

datatot=rbindlist(my.data)

fwrite(datatot,NewName,sep=";")
