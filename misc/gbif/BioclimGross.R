library(raster)

Nlong=360/30
Nlat=180/30
Precision=24
Precision=1

BiocData=getData('worldclim', var='bio', res=2.5)

for (i in 1:Nlong)
{
  TileLong=c(0:(30*Precision))/Precision+(i-1)*30-180
  for (j in 1:Nlat)
  {
    TileLat=c(0:(30*Precision))/Precision+(j-1)*30-60
    TileCoord=merge(as.data.frame(TileLong),as.data.frame(TileLat))
    TileCoord$id=c(1:nrow(TileCoord))
    
    sp::coordinates(TileCoord) <- c("TileLong","TileLat") #May be change pts arg to spatial object
    sp::proj4string(TileCoord) <- sp::CRS("+init=epsg:4326") # WGS 84
    message("Be careful, the function assumes that coordinates of pts are in WGS 84 projection")
    #stopifnot(sp::proj4string(clim) == sp::proj4string(pts)) #weird stuff about proj4string being different despite being the same CRS
    Sys.time()
    clim_pts <- raster::extract(BiocData, TileCoord, df = TRUE)
    Sys.time()
    
    for (k in 1:nrow(TileCoord))
    {
      if(is.na(clim_pts[k,2]))
      {
        Good=0
        jitter=0
        while(Good==0)
        {
          jitter=jitter+1/Precision
          newpoints=TileCoord[k,]
          newpoints=rbind(newpoints,newpoints,newpoints,newpoints)
          #
          ToModify=coordinates(newpoints)
          ToModify[1,1]=coordinates(TileCoord)[k,1]+jitter
          ToModify[2,1]=coordinates(TileCoord)[k,1]-jitter
          ToModify[3,2]=coordinates(TileCoord)[k,2]+jitter
          ToModify[4,2]=coordinates(TileCoord)[k,2]-jitter
          newpoints=as.data.frame(newpoints)
          coordinates(newpoints)=ToModify
          clim_new <- raster::extract(BiocData, newpoints, df = TRUE)
          climNonNA=subset(clim_new,!is.na(clim_new$layer.1))
          if(nrow(climNonNA)>0)
          {
            clim_pts[k,]=climNonNA[1,]
            Good=1
          }
          print(jitter)
        }
        
    
    
    
    }
}

test
