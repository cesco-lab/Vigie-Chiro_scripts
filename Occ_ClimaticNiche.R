library(data.table)

F_OccSL="./vigiechiro/Traits/GBIF/OccSL_bush-cricket.csv"
#TO DO: USE MORE PRECISE BIOCLIM LAYER
Bioclimdir="C:/Users/Yves Bas/Documents/SIG/Bioclim_5m/" #where bioclim TIFF files are



coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
OccSL=fread(F_OccSL)
ListTifiles=list.files(Bioclimdir,full.names=T)
Bioc1=raster(ListTifiles[1])


ThAve=vector()
ThSD=vector()
ThQ95=vector()
ThQ5=vector()

for (h in 1:nlevels(as.factor(OccSL$Esp)))
{
  print(paste(Sys.time(),levels(as.factor(OccSL$Esp))[h]))
Occ_h=subset(OccSL,OccSL$Esp==levels(as.factor(OccSL$Esp))[h])
  
    Sys.time()
  SpBioci=extract(Bioc1,Occ_h)
  Sys.time()
  SpBioci=subset(SpBioci,!is.na(SpBioci))
  
  ThAve=c(ThAve,mean(SpBioci))
  ThSD=c(ThSD,sd(SpBioci))
  ThQ95=c(ThQ95,quantile(SpBioci,0.95))
  ThQ5=c(ThQ5,quantile(SpBioci,0.05))
  
}

barplot(ThAve,names=levels(as.factor(OccSL$Esp)),las=2,cex.names=0.5)
barplot(ThSD,names=levels(as.factor(OccSL$Esp)),las=2,cex.names=0.5)
barplot(ThQ95-ThQ5,names=levels(as.factor(OccSL$Esp)),las=2,cex.names=0.5)
plot(ThAve,ThSD)
plot((ThQ95+ThQ5)/2,ThQ95-ThQ5)

plot(ThAve,(ThQ95+ThQ5)/2)
plot(ThQ95-ThQ5,ThSD)

ThRange=ThQ95-ThQ5
ThMed=(ThQ95+ThQ5)/2

ClimNiche=data.frame(cbind(Species=levels(as.factor(OccSL$Esp)),ThAve,ThSD,ThQ5,ThQ95,ThMed,ThRange))

dirname(F_OccSL)

fwrite(ClimNiche,paste0("./vigiechiro/Traits/ClimNiche_"
                        ,basename(F_OccSL)))
