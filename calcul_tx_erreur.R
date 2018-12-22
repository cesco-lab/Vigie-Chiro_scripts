#MARCHE SEULEMENT SUR DU MONO
rm(list = ls()) #nettoie tout
setwd("C:/Users/Yves Bas/Documents/Alexis")
library(data.table) #to handle large numbers of .ta files
library(boot) # to get inv.logit function

# output Tadarida
# compilation donnees
Donnees=fread("IdC2.csv")
Seuil=fread("180227_Referentiel_seuils_ProbEspHF_C2_filt_Car.csv")

Donnees=Donnees[order(-Ib),]
head(Donnees)
Donnees=unique(Donnees,by=c("participation","donnee","ProbEsp_C2bs"))

DonneeSeuil=merge(Donnees,Seuil,by.x="ProbEsp_C2bs",by.y="Espece")
test=match(Donnees$ProbEsp_C2bs,Seuil$Espece)
EspManquantes=subset(Donnees,is.na(test))
table(EspManquantes$ProbEsp_C2bs)

# SEUIL 50
DonneeS50=subset(DonneeSeuil,DonneeSeuil$Ib>DonneeSeuil$Seuil50)
ListPar=as.data.frame(levels(as.factor(DonneeS50$participation)))
colnames(ListPar)="Group.1"
TxErr50 = c()
for (i in 1:nlevels(as.factor(DonneeS50$ProbEsp_C2bs))){
  TxErr = 0
  Err = 0
  #subset de l'esp?ce
  DonneeSp=subset(DonneeS50,DonneeS50$ProbEsp_C2bs==levels(as.factor(DonneeS50$ProbEsp_C2bs))[i])
  
  Err = 1-inv.logit(DonneeSp$Int+DonneeSp$Pente*DonneeSp$Ib)
  
  TxErr = sum(Err)/nrow(DonneeSp)
  
  TxErr50 = c(TxErr50, TxErr)
  
  rm(TxErr, Err)
}
names(TxErr50) = levels(as.factor(DonneeS50$ProbEsp_C2bs))
TxErr50

# SEUIL 90
DonneeS90=subset(DonneeSeuil,DonneeSeuil$Ib>DonneeSeuil$Seuil90)
ListPar=as.data.frame(levels(as.factor(DonneeS90$participation)))
colnames(ListPar)="Group.1"
TxErr90 = c()
for (i in 1:nlevels(as.factor(DonneeS90$ProbEsp_C2bs))){
  TxErr = 0
  Err = 0
  #subset de l'esp?ce
  DonneeSp=subset(DonneeS90,DonneeS90$ProbEsp_C2bs==levels(as.factor(DonneeS90$ProbEsp_C2bs))[i])
  
  Err = 1-inv.logit(DonneeSp$Int+DonneeSp$Pente*DonneeSp$Ib)
  
  TxErr = sum(Err)/nrow(DonneeSp)
  
  TxErr90 = c(TxErr90, TxErr)
  
  rm(TxErr, Err)
}
names(TxErr90) = levels(as.factor(DonneeS90$ProbEsp_C2bs))
TxErr90

Table50=as.data.frame(TxErr50) #faire d'une matrice une data.frame
Table50$Species=rownames(Table50) #cr?er une colonne Id
rownames(Table50)=c(1:nrow(Table50)) #changer les num?ros de ligne
Table90=as.data.frame(TxErr90) #faire d'une matrice une data.frame
Table90$Species=rownames(Table90) #cr?er une colonne Id
rownames(Table90)=c(1:nrow(Table90)) #changer les num?ros de ligne

TableTxErr=merge.data.frame(Table50, Table90, by = "Species")
write.csv2(TableTxErr,"Tx_Err_50_90_PAY.csv")
