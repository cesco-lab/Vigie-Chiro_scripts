library(data.table)

Particip=fread("p_export_forLinux.csv")
PosteRTE=fread("C:/Users/yvesb/Documents/www/listepostesRTE.csv")

PosteRTE$site=paste0("Vigiechiro - Point Fixe-",PosteRTE$carre)


PartiPoste=merge(PosteRTE,Particip,by=c("site","point"))

fwrite(PartiPoste,"PartiPoste.csv")
