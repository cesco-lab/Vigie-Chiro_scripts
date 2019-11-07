Test=F
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Coord_Bioclim function to get bioclim data at locations
##' @param points table giving coordinates in WGS84 or file path of the table
##' @param names_coord vector of two values giving
##' @return table of 12 bioclimatic variable values
##' @author Yves Bas
Coord_Bioclim=function(points,names_coord,write=TRUE,id=NULL,merge_data=FALSE
                       ,plot=TRUE,output_sp = TRUE)
{
##  browser()
    library(data.table)
    library(sp)
    library(raster)
    FOccSL=points
    if(class(points)[1]=="character") OccSL=fread(paste0(FOccSL,".csv")) else OccSL <- points
    CoordH=names_coord

    testH=match(CoordH,names(OccSL))
    OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
    OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))


    ## Bioclimdir="C:/Users/Yves Bas/Documents/SIG/Bioclim_5m/" #where bioclim TIFF files are

    ## coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
    coordinates(OccSL) <- CoordH
    proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
    ## est meridien greenwhich
    r1=getData('worldclim',var='bio',res=0.5,lon=0,lat=45)
    ## ouest meridien greenwhic
    r2=getData('worldclim',var='bio',res=0.5,lon=-10,lat=45)


                                        #ListTifiles=list.files(args[2],full.names=T)

    SpBioc1=extract(r1,OccSL)
    SpBioc2=extract(r2,OccSL)


    SpBioc12=mapply(FUN=function(x,y) ifelse(is.na(x),y,x),SpBioc1,SpBioc2)
    SpBioc12=as.data.frame(matrix(SpBioc12,ncol=ncol(SpBioc1),nrow=nrow(SpBioc1)))
###  subset(c(1:nrow(SpBioc12)),is.na(SpBioc12$V1))
    OccSL=subset(OccSL,!is.na(SpBioc12$V1))
  #  if(merge)points=subset(points,!is.na(SpBioc12$V1))
    SpBioc12=subset(SpBioc12,!is.na(SpBioc12$V1))


    names(SpBioc12)=paste0("SpBioC",c(1:ncol(SpBioc12)))

                                        #AFAIRE = récupérer les données côtières qui donnent des NA dans worldclim
    if(merge_data) Bioclim=cbind(as.data.frame(OccSL),SpBioc12) else Bioclim=cbind(coordinates(OccSL),SpBioc12)
    if(write) {
        if(class(FOccSL)[1]=="character") {
            fwrite(Bioclim,paste0(FOccSL,"_Bioclim.csv"))
        } else {
            if(is.null(id)) id <- Sys.Date()
            fwrite(Bioclim,paste0(id,"_Bioclim.csv"))
        }
    }


    if(plot) {
     Bioclim_plot <- Bioclim
    coordinates(Bioclim_plot) <- CoordH

    SelCol=sample(names(SpBioc12),1)
     spplot(Bioclim_plot,zcol=SelCol,main=SelCol)
     if(class(FOccSL)[1]=="character") {
       png(paste0(FOccSL,"_Bioclim.png"))
       spplot(Bioclim_plot,zcol=SelCol,main=SelCol)
       dev.off()
         #savePlot(paste0(FOccSL,"_Bioclim.png"))
     } else {
         if(is.null(id)) id <- Sys.Date()
         savePlot(paste0(id,"_Bioclim.png"))
        }
    }


    if(output_sp)  coordinates(Bioclim) <- CoordH

    return(Bioclim)
}

if(Test)
{
                                        #for test
    Coord_Bioclim(
        points="./vigiechiro/GIS/PA_Thymus nitens" #table giving coordinates in WGS84
       ,names_coord=c("decimalLongitude","decimalLatitude") #vector of two values giving
    )
}
