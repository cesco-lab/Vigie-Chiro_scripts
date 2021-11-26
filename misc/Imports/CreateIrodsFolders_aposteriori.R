library(data.table)

DirToUpload="D:/PI_NDDL2104/upload"

DirsToCreate=list.dirs(DirToUpload)

RelToCreate=gsub(DirToUpload,"",DirsToCreate)
RelToCreate=RelToCreate[2:length(RelToCreate)]
Batch=paste0("imkdir /ccin2p3/home/ybas/transferts/",RelToCreate)

fwrite(data.frame(batch=Batch),"batchmkdir2.txt",col.names = F)


