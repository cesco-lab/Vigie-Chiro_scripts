library(data.table)

U0=fread("C:/wamp64/www/utilisateurs_prev.txt")
U1=fread("C:/wamp64/www/utilisateurs.txt")

test=match(U1$identifiant,U0$identifiant)

Unouveau=subset(U1,!(U1$identifiant %in% U0$identifiant))
Unouveau$instruction=paste("INVITE vigie-chiro",Unouveau$email)
fwrite(Unouveau,"AInscrire.csv",sep=";")