library(data.table)
library(raster)
FDataSelRare="C:/Users/yvesb/Documents/VigieChiro/gbifData/DataSelDate/DataSelRare_Plant_56.csv"
Radius=9
#lou deves
LongOrigin=3.7671
LatOrigin=43.8231


DataSelRare=fread(FDataSelRare)


p <- SpatialPoints(cbind(LongOrigin,LatOrigin))
proj4string(p)="+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
pBuffer=buffer(p,width=Radius/70+0.04) #dirty



sp=vector()
gp=vector()
ngp=vector()
nsp=vector()
count=0
ncount=vector()
datepic=vector()
for (i in 1:nlevels(as.factor(DataSelRare$Group)))
{
  FileDG=paste0("C:/Users/yvesb/Documents/VigieChiro/gbifData/DataGroup/done/DataGroup2_"
                ,levels(as.factor(DataSelRare$Group))[i],"_FR.csv")
  if(file.exists(FileDG)){
    DataGroup=fread(FileDG)  
    
    DataGroup=subset(DataGroup,!is.na(DataGroup$decimalLongitude))
    coordinates(DataGroup)=c("decimalLongitude","decimalLatitude")
    crs(DataGroup)="+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
    DataCrop=crop(DataGroup,pBuffer)
    if(!is.null(DataCrop))
    {
      SpGroup=subset(DataSelRare
                     ,DataSelRare$Group==levels(as.factor(DataSelRare$Group))[i])  
      
      #print(paste(levels(as.factor(DataSelRare$Group))[i],nrow(SpGroup)
      #           ,nrow(DataCrop)))
      for (j in 1:nrow(SpGroup))
      {
        DataSp=subset(DataCrop,DataCrop$name==SpGroup$ListSpValide[j])
        sp=c(sp,SpGroup$ListSpValide[j])
        gp=c(gp,levels(as.factor(DataSelRare$Group))[i])
        nsp=c(nsp,nrow(DataSp))
        ngp=c(ngp,nrow(DataCrop))
        count=count+1
        ncount=c(ncount,count)
        print(paste(SpGroup$ListSpValide[j],nrow(DataSp),count))
        datepic=c(datepic,SpGroup$PicSp[j])
      }
    }
  }
}
plot(nsp,ngp)
DF=data.frame(sp,gp,datepic,nsp,ngp)

NewF=paste0(gsub(".csv","",FDataSelRare),"_",Radius,".csv")
fwrite(DF,NewF,sep=";")
