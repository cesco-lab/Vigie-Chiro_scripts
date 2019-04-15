library(data.table)

#args="tabase3HF_1015France_IdMan.csv"
args="C:/wamp64/www/Exhaustifs.csv"
#args[2]="C:/wamp64/www/export_validtot2018-07-26.csv"
args[2]="./Tadarida/rounds/compli_validation190220/ETVtot.csv"

ListE=fread(args[1])
IdV=fread(args[2])

IdV_E=subset(IdV,IdV$donnee %in% ListE$donnee)
IdV_NE=subset(IdV,!IdV$donnee %in% ListE$donnee)


fwrite(IdV_E,"IdExhaustifs.csv")
fwrite(IdV_NE,"IdNonExhaustifs.csv")

