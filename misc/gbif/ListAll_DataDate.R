library(data.table)


ListAll=fread("C:/Users/ybas/Downloads/DataListAll.csv")
DateSel=fread("C:/Users/ybas/Downloads/DateSel.csv")
Date=15
PhylumSel=c("Tracheophyta","Basidiomycota")
PhylumSel=NA
ClassSel=c("Aves","Mammalia","Amphibia")
OrderSel=c("Orthoptera")
FamilySel=c("Cicadidae")

DateAll=merge(ListAll,DateSel,by.x="species",by.y="ListSpValide")


if(is.na(PhylumSel)){
  table(DateAll$class)
  DateAll1=subset(DateAll,DateAll$class %in% ClassSel)
  DateAll2=subset(DateAll,DateAll$order %in% OrderSel)
  DateAll3=subset(DateAll,DateAll$family %in% FamilySel)
  DateAll=rbindlist(list(DateAll1,DateAll2,DateAll3))
  }else{
    DateAll=subset(DateAll,DateAll$phylum %in% PhylumSel)
}

DataDate=subset(DateAll,((DateAll$PicSp>Date-15)&(DateAll$PicSp<Date+15))|
                  ((DateAll$PicSp>Date-15+360)&(DateAll$PicSp<Date+15+360))|
                  ((DateAll$PicSp>Date-15-360)&(DateAll$PicSp<Date+15-360)))

fwrite(DataDate,"DataDate.csv",sep=";")

