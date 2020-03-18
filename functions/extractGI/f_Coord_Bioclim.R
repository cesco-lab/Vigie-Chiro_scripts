test=T
#'  Get bioclim data at given locations
#'
#' The function takes a observation points and a raster with climatic variables.
#' It returns a data.frame or a sp data.frame with climatic values at the
#' observation points.
#'
#' @param pts table giving coordinates in WGS84 or file path of the table
#' @param longlat character vector of length 2 given the names of the longitude
#' and latitude columns respectively
#' @param clim raster of climatic data (e.g. obtained by raster::getData())
#' @return sp data.frame or data.frame with clim values at pts coordinates
#' @author Yves Bas and Alain Danet

extract_clim <- function (pts = NULL, longlat = c("long", "lat"),
                          write=TRUE, id = NULL, clim = NULL
                          , merge_data = FALSE , plot = TRUE, sp_output = TRUE)
{
  
  # Load pts data.frame if necessary
  if (class(pts)[1] == "character") {
    pts_path <- pts
    stopifnot(file.exists(pts_path))
    pts <- data.table::fread(pts_path)
  plot=F
    }
  
  # Remove pts with NA coordinates    
  na_pts_mask <- !is.na(pts[[longlat[1]]]) & !is.na(pts[[longlat[2]]])
  pts <- pts[na_pts_mask,]
  ## Bioclimdir="C:/Users/Yves Bas/Documents/SIG/Bioclim_5m/" #where bioclim TIFF files are
  
  # Convert pts to sp obj
  ## may be request directly a sp as pts 
  sp::coordinates(pts) <- longlat # May be change pts arg to spatial object
  sp::proj4string(pts) <- sp::CRS("+init=epsg:4326") # WGS 84
  message("Be careful, the function assumes that coordinates of pts are in WGS 84 projection")
  #stopifnot(sp::proj4string(clim) == sp::proj4string(pts)) #weird stuff about proj4string being different despite being the same CRS
  
  # Extract relevant clim data
  clim_pts <- raster::extract(clim, pts, df = TRUE)
  clim_pts$ID <- NULL
  
  # Rm NA from clim 
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
    if (exists("pts_path")) {
      id=gsub(".csv","",pts_path)
      data.table::fwrite(output, paste0(id, "_Bioclim.csv")) 
      
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
    
    # Take a var randomly to plot
    clim_var <- sample(names(clim), 1)
    
    # Make a issue in sp for spplot
    if (nrow(bioclim_plot) > 1) {
      sp::spplot(bioclim_plot, zcol = clim_var, main = clim_var)
    } else {
      sp::plot(bioclim_plot)
      message("Less than 2 pts, using sp::plot instead of sp::spplot.")
    }
    
    if (exists("pts_path")) {
      stopifnot(nrow(bioclim_plot) > 1)
      #png(paste0(pts_path, "_Bioclim.png"))
      print(sp::spplot(bioclim_plot, zcol = clim_var, main = clim_var))
      #dev.off()
    }
    #else {
     # if(is.null(id)) {
      #  id <- Sys.Date()
    #  }
     # savePlot(paste0(id,"_Bioclim.png"))
    #}
  }
  
  
  return(output)
}

pt_test <- data.frame(long = c(3, 3, NA), lat = c(27, NA, NA))
clim_data <- raster::raster(nrows = 3, ncols = 3, xmn = 2, xmx = 4, ymn = 26, ymx = 28)
raster::values(clim_data) <- rnorm(length(raster::values(clim_data))) 
sp::proj4string(clim_data) <- sp::CRS("+init=epsg:4326")
extract_clim(pts = pt_test, longlat = c("long", "lat"),
             write = FALSE, id = NULL, clim = clim_data, merge_data = FALSE , plot = TRUE, sp_output = TRUE)


#' Load of download french worldclim data
#'
#' Wrapper around raster::getData to get french worldclim data at a resolution
#' of 0.5°.
#'
#' @inheritParams raster::getData
#' @param path A string containing the path in which save the worldclim data
#' @param res See raster::getData
#' @param var See raster::getData
#' @param ... More options to be supplied to raster::getData
#' @return a raster
#'
## Not run ##
## get_fr_worldclim_data()
## Not run ##

get_fr_worldclim_data <- function (path = ".", res = 0.5, var = "bio", ...) 
{
  
  # East and west of Greenwhich
  east_fr <- raster::getData(name = 'worldclim', path = path, res = res,
                             var = var, lon = 0, lat = 45, ...)
  west_fr <- raster::getData(name = 'worldclim', path = path, res = res,
                             var = var, lon = -10, lat = 45, ...)
  
  # Merge the two rasters
  fr <- raster::merge(east_fr, west_fr, overlap = TRUE)
  
  return(fr)
}
## Not run ##
#get_fr_worldclim_data()
if(test)
{
  extract_clim(
    merge_data=T
    ,
    write = T
    ,
    clim=get_fr_worldclim_data()
    ,
    #pts = "PrioCoord_2020-03-07_Tetraena_gaetula.csv"
    #pts = "./VigieChiro/GIS/coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv"
    pts = "C:/wamp64/www/sites_localites.txt"
    
         ,        
    #longlat = c("decimalLongitude", "decimalLatitude")
    #longlat = c("Group.1", "Group.2")
    longlat = c("longitude", "latitude")
    
      )
                             
}