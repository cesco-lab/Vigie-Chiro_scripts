library(data.table)

FolderTA="K:/Yves/Verbatim/1017_MNHN/PI"
site="Vigiechiro - Point Fixe-780089"
point="Z2"
detecteur_enregistreur_type="Ultramic250k+Autrelogiciel"
commentaire="Capteur Verbatim equipe d un Ultramic 384k"
type_micro="Micro externe sans cornet"
hauteur_micro=5
C14="SOL"
detecteur_enregistreur_numero_serie=1017

f2pta <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 21, nchar(x)-7), ".", substr(x, nchar(x) - 5, nchar(x)-3), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")+7200
}

lfTA=list.files(FolderTA,full.names=T)

dateTA=subset(lfTA,substr(basename(lfTA),1,2)=="20")

datedeb=list()
datefin=vector()
for (i in 1:length(dateTA))
{
  print(paste(basename(dateTA[i]),Sys.time()))
  listTA=list.files(dateTA[i],pattern=".ta$")
  if(length(listTA)>0)
  {
    dd=f2pta(listTA[1])
    dd2=format(dd,format="%d/%m/%Y %H:%M")
    
    #datedeb=c(datedeb,f2pta(listTA[1]))
    datedeb=c(datedeb,dd2)
    df=f2pta(listTA[length(listTA)])
    df2=format(df,format="%d/%m/%Y %H:%M")
    
    datefin=c(datefin,df2)
  }
}
date_debut=datedeb
date_fin=datefin
pimp=data.frame(cbind(site,	date_debut,	date_fin,	point,	commentaire,
                      temperature_debut="",temperature_fin="",	couverture=""
                      ,	vent="",	detecteur_enregistreur_type
                      ,detecteur_enregistreur_numero_serie	,type_micro	,hauteur_micro
                      ,C14	,C15="",C16=""	,C17=""	,C18=""	,C19=""	,C20=""	,C21=""))

fwrite(pimp,"C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/p_import.csv",sep=";")