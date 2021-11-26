EPA="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/extr_PF_ActNuit.r"

FRaw="D:/VigieChiro/Raw/"

args=""
args[3]=0
args[10]="D:/VigieChiro/Raw/Pheno100"
args[14]="time_int" #sunrise, sunset or time_int
args[15]=F #sort out doubtful data (probable hardware problems)
args[16]="C:/wamp64/www/export_validtot210216.txt"
args[17]=F #correct for validation or not

FileSource=list.files(FRaw,full.names=T)
FileSource=subset(FileSource
                  ,substr(basename(FileSource),1,16)=="DataLP_PF_export")

dir.create(args[10])

for (i in 1:length(FileSource))
{
  print(FileSource[i])
  print(Sys.time())
  args[4]=FileSource[i]
  for (j in (-5:104))
  {
    args[12]=(j+1)/100
    args[13]=(j)/100
  source(EPA)
    print(j)
    }
}