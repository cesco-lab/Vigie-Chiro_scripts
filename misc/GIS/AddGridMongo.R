library(mongolite)

connection_string = "mongodb://vigiechiro:4Zp9nh8d2YSVt5@ccmg01.in2p3.fr:27030,ccmg02.in2p3.fr:27030,ccmg03.in2p3.fr:27030/vigiechiro?replicaSet=rs0&readPreference=secondaryPreferred&socketTimeoutMS=6000000"

grid = mongo(collection="grille_stoc", db="vigiechiro", url=connection_string)

id_participation = "617a583d241aabd8e5de04bd"

resu = participations$find(query = paste('{"_id" : {"$oid":"',id_participation,'"}}',sep=""))

print(resu["site"])


