library(data.table)

#r�cup�ration des donn�es participation
Particip=fread("C:/wamp64/www/p_export.csv")
A_ta=fread("C:/wamp64/www/archiveesta.txt",header=F)
A_wav=fread("C:/wamp64/www/archiveeswav.txt",header=F)
Valid=fread("export_validtot2018-09-03.csv")

Particip$Valid=(Particip$participation %in% Valid$participation)

#P_A_ta=subset(Particip,Particip$participation %in% A_ta$V1)
#P_A_wav=subset(Particip,Particip$participation %in% A_wav$V1)

Particip$archive_ta=(Particip$participation %in% A_ta$V1)
Particip$archive_wav=(Particip$participation %in% A_wav$V1)

write.csv2(Particip,"p_archive.csv",row.names=F)


#fwrite(P_A_ta,"P_A_ta.csv")
#fwrite(P_A_wav,"P_A_wav.csv")
