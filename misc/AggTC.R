install.packages("data.table")
library(data.table)
args="C:/Users/Yves Bas/Downloads/nuit_du_02-03-2018_au_03-03-2018"

tclist=list.files(args,pattern=".tc$",full.names=T,recursive=T)

my.data <- list()
for(f in 1:length(tclist)) {
  my.data[[f]] <- fread(tclist[[f]])
}
IdDetail=rbindlist(my.data)
SpMax<-max.col(IdDetail[,2:(ncol(IdDetail)-6)],ties.method = "first")
IdDetail$Id=colnames(IdDetail)[SpMax+1]
write.csv(IdDetail,"IdDetail.csv")
