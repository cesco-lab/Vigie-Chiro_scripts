library(data.table)
SpipollData=fread("./VigieChiro/SPIPOLL/spipoll_insects_cache_view.csv")

SpipollData$TaxonFR=tstrsplit(SpipollData$insect_taxon,"<")[[2]]
SpipollData$TaxonFR=substr(SpipollData$TaxonFR,1,nchar(SpipollData$TaxonFR)-2)
SpipollData$TaxonFR=gsub(" ","_",SpipollData$TaxonFR)
SpipollData$TaxonFR=gsub(",","_",SpipollData$TaxonFR)
SpipollData$TaxonFR=gsub("/","_",SpipollData$TaxonFR)
#SpipollData$TaxonFR=gsub("|","_",SpipollData$TaxonFR)


#test=grepl("|",SpipollData$TaxonFR)

SpipollSample=subset(SpipollData,select=c("lat","long","datedebut"))
SpipollSampleLoc=subset(SpipollData,select=c("lat","long"))

SpipollSampleTot=unique(SpipollSample)
SpipollSampleLocTot=unique(SpipollSampleLoc)


for (i in 1:nlevels(as.factor(SpipollData$TaxonFR)))
{
  if(!grepl(">",levels(as.factor(SpipollData$TaxonFR))[i]))
  {
    DataSp=subset(SpipollSample
                  ,SpipollData$TaxonFR==levels(as.factor(SpipollData$TaxonFR))[i])
    
    DataSp$presence=1
    print(paste(i,nrow(DataSp),levels(as.factor(SpipollData$TaxonFR))[i]))
    DataSpU=unique(DataSp)
    DataSp_w0=merge(DataSpU,SpipollSampleTot,by=c("lat","long","datedebut"),all=T)  
    DataSp_w0$presence[is.na(DataSp_w0$presence)]=0
    fwrite(DataSp_w0,paste0("./VigieChiro/SPIPOLL/DataSp/DataSp_"
                            ,levels(as.factor(SpipollData$TaxonFR))[i]
                            ,".csv"))
  }
}
