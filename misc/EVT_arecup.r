library(data.table)

EVTref="export_validtot2018-09-03.csv"
ListEVT=list.files(getwd(),pattern="export_validtot")

ListEVT_averif=subset(ListEVT,!ListEVT %in% EVTref)

TEVTref=fread(EVTref)

Tarecup=data.frame()
for (i in 1:length(ListEVT_averif))
{
  Taverif=fread(ListEVT_averif[i])
  Tdiff=subset(Taverif,!Taverif$donnee %in% TEVTref$donnee)
  Tdiff=subset(Tdiff,!Tdiff$donnee %in% Tarecup$donnee)
  Tarecup=rbind(Tarecup,Tdiff,fill=T,use.names=T)
print(i)
}

Tarecup_name=paste0(substr(EVTref,1,nchar(EVTref)-4),"_recup.csv")

fwrite(Tarecup,file=Tarecup_name)
