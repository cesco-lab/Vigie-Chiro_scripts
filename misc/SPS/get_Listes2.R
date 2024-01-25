library(data.table)

Selection=fread("C:/Users/ybas/Downloads/export_validtot220305_select_pourYves.csv")

#ne garde qu'une ligne par participation
Selection_Upar=unique(Selection,by="participation")

#supprime les idsite manquants (probable participations supprimées, à vérifier ?)
Selection_Upar=subset(Selection_Upar,!is.na(Selection_Upar$idsite))

#genere la ligne de commande pour récupérer les fichiers liste2.txt (listant les tar)
#et les range dans le repertoire Listes2 avec l'identifiant de la participation comme préfixe (pour les distinguer)
GetListe2=paste0("iget -r /inee/vigiechiro/vigiechiro-datastore2/"
                 ,Selection_Upar$idsite
                 ,"/",Selection_Upar$participation
,"/wav/liste2.txt /sps/mnhn/vigiechiro/vigiechiro-prod-datastore/TempTiphaine/Listes2/"
,Selection_Upar$participation,"_liste2.txt"
)

#conversion en data frame
GetListe2DF=data.frame(Command=GetListe2)

#ecriture avec un suffixe de la date pour la tracabilite
fwrite(GetListe2DF,paste0("GetListe2_",Sys.Date(),".csv"),sep=";")
