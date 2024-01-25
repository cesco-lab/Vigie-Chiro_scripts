library(data.table)


DataOpen=fread("C:/Users/yvesb/Downloads/DataOpeni.csv")




Weird=subset(DataOpen,DataOpen$score_max<0.5&DataOpen$nb_contacts_nd>0)
table(Weird$confiance_validateur)
table(Weird$confiance_observateur)
table(Weird$espece)
hist(Weird$score_max)
hist(DataOpen$score_max)
plot(Weird$nb_contacts_nd,Weird$score_max)
