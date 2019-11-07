library(data.table)
library(corrplot)
ListTabTraits=c("ClimNiche_OccSL_bush-cricket","Saisies_traits_Sauterelles","GLMnonselect_DecOT2_AT81Coefs")



SpCol=c("Species","Species2","Esp","Espece")

setwd("C:/Users/Yves Bas/Documents/VigieChiro/Traits")

Tab1=fread(paste0(ListTabTraits[1],".csv"))
E1=match(names(Tab1),SpCol)
E1_col=subset(names(Tab1)[E1],!is.na(names(Tab1)[E1]))

Tab_merge=Tab1

if(length(ListTabTraits)>1)
{

for (i in 2:length(ListTabTraits))
{
  Tab2=fread(paste0(ListTabTraits[i],".csv"))
  E2=match(SpCol,names(Tab2))
  E2_col=subset(names(Tab2)[E2],!is.na(names(Tab2)[E2]))
  Tab_merge=merge(Tab_merge,Tab2,by.x=E1_col[1],by.y=E2_col[1])
}
}

SelCol= which(sapply(Tab_merge,is.numeric))

NameTab=paste0("TabMerge",ListTabTraits[length(ListTabTraits)],".csv")
fwrite(Tab_merge,NameTab)


Tab_num=Tab_merge[,SelCol,with=F]
Tab_num=subset(Tab_num,!is.na(Tab_num$DecPheno))

M=cor(Tab_num)

pal <- colorRampPalette(c("blue3","white","brown3"))

corrplot(M, method="circle",tl.cex=0.5,col=pal(200))

setwd("C:/Users/Yves Bas/Documents/")
