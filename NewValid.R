OldValid=fread("C:/wamp64/www/ExportValidTot170220.csv")
FichNewValid="C:/wamp64/www/export_validtot180425.txt"

Valid=fread(FichNewValid)



NewValid=subset(Valid,!(Valid$donnee %in% levels(as.factor(OldValid$fichier))))
NewValid=subset(NewValid,NewValid$valid.espece!="")
table(NewValid$valid.espece)
NewValid$URL=paste0("https://vigiechiro.herokuapp.com/#/participations/",NewValid$participation)

fwrite(NewValid,paste0("NewValid"
                       ,substr(FichNewValid,nchar(FichNewValid)-9
                               ,nchar(FichNewValid)-4),".csv"))
