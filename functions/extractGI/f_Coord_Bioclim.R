#'  Get bioclim data at given locations
#'
#' @param pts table giving coordinates in WGS84 or file path of the table
#' @param longlat character vector of length 2 given the names of the longitude
#' and latitude columns respectively 
#' @param clim raster of climatic data (e.g. obtained by raster::getData())
#' @return table of 12 bioclimatic variable values
#' @author Yves Bas
pt_test <- data.frame(long = c(3, 3, NA), lat = c(27, NA, NA))
debug(get_bioclim_data)
get_bioclim_data(pts = pt_test, longlat = c("long", "lat"),
  write = TRUE, id = NULL, clim = r1, merge_data = FALSE , plot = TRUE, sp_output = TRUE)

## est meridien greenwhich
r1 <- raster::getData('worldclim', var = 'bio', res = 10, lon = 0, lat = 45)
## ouest meridien greenwhic
r2 <- raster::getData('worldclim', var = 'bio', res = 10, lon = -10, lat = 45)
SpBioc1 = extract(r1, pts)
SpBioc2 = extract(r2, pts)

SpBioc12=mapply(FUN=function(x,y) ifelse(is.na(x),y,x),SpBioc1,SpBioc2)
SpBioc12=as.data.frame(matrix(SpBioc12,ncol=ncol(SpBioc1),nrow=nrow(SpBioc1)))

get_bioclim_data <- function (pts = NULL, longlat = c("long", "lat"),
  write=TRUE, id = NULL, clim = NULL, merge_data = FALSE , plot = TRUE, sp_output = TRUE) {

  # Load pts data.frame if necessary
  if (class(pts)[1] == "character") {
    stopifnot(file.exists(pts), "file does not exist")
    pts <- data.table::fread(pts)
  }

  # Remove pts with NA coordinates    
  na_pts_mask <- !is.na(pts[[longlat[1]]]) & !is.na(pts[[longlat[2]]])
  pts <- pts[na_pts_mask,]
  ## Bioclimdir="C:/Users/Yves Bas/Documents/SIG/Bioclim_5m/" #where bioclim TIFF files are

  # Convert pts to sp obj
  ## may be request directly a sp as pts 
  sp::coordinates(pts) <- longlat # May be change pts arg to spatial object
  sp::proj4string(pts) <- sp::CRS("+init=epsg:4326") # WGS 84
  message("Be careful, the function assumes that coordinates of pts are in WGS projection")

  # Extract relevant clim data
  clim_pts <- raster::extract(clim, pts)

  # Rm NA from  clim 
  clim_na_mask <- rowSums(is.na(clim_pts)) == 0
  pts <- pts[clim_na_mask, ]
  clim_pts <- clim_pts[clim_na_mask, , drop = FALSE]

  # Prepare output:

  # Necessary for f_biodiv_modgis.r:
  names(clim_pts) <- paste0("SpBioC", c(1:ncol(clim_pts)))

  # merge
  if (merge_data) {
    output <- data.frame(pts, clim_pts)
  }  else {
    output <- data.frame(sp::coordinates(pts), clim_pts)
  }

  # Write output: 
  if (write) {

    if (class(pts)[1] == "character") {
      data.table::fwrite(output, paste0(pts, "_Bioclim.csv"))
    } else {
      if(is.null(id))
	id <- Sys.Date()
      data.table::fwrite(output, paste0(id,"_Bioclim.csv"))
    }
  }

  # Plot
  if (plot) {

    bioclim_plot <- output 
    sp::coordinates(bioclim_plot) <- longlat

    clim_var <- sample(names(clim), 1)

    # Make a issue in sp for spplot 
    sp::spplot(bioclim_plot, zcol = clim_var, main = clim_var)
    plot(bioclim_plot)
    if (class(pts)[1] == "character") {
      png(paste0(pts, "_Bioclim.png"))
      sp::spplot(bioclim_plot, zcol = clim_var, main = clim_var)
      dev.off()
      #savePlot(paste0(pts,"_Bioclim.png"))
    } else {
      if(is.null(id)) {
	id <- Sys.Date()
      }
      savePlot(paste0(id,"_Bioclim.png"))
    }
  }

  if (output_sp) {
    sp::coordinates(bioclim) <- longlat
  }

  return(bioclim)

}

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
    if (merge_data) {
    
      Bioclim=cbind(as.data.frame(OccSL),SpBioc12) else Bioclim=cbind(coordinates(OccSL),SpBioc12)
    }
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
