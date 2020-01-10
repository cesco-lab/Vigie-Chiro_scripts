
##  Groupe de function pour récupérer les données météo




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title get weather anomalie form sample data
##' @param dsample data or data path or NULL
##' @param first_year if NULL first year of data sample
##' @param last_year if NULL last year of data sample
##' @param time_windows
##' @param nc_local TRUE to extract value in local file
##' @param nc_extract TRUE if you want use the extract_nc_value function
##' @param nc_data path of the Rdata of the weather data
##' @param nc_ref_file
##' @param var weather variables
##' @param dsample_colnames names of important columns in sample table defaut c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84")
##' @param output boolean export table in console
##' @param save boolean save or not the table
##' @param fileouput name of file to save the table
##' @return la table sample avec les anomalies des variables météo désiré à 1,3,9,27 et 81 jours
##' @importFrom data.table yday
##' @importFrom RnetCDF
##' @importFrom climateExtract
##' @importFrom ncdf4
##' @importFrom dyplr innerjoin
##' @author Romain Lorrilliere and Yves Bas
get_sample_weather <- function(dsample=NULL,first_year=NULL,last_year=NULL,temp_windows=c(0,3,9),
                               nc_local=TRUE,nc_extract=FALSE,
                               nc_data="C:/git/Vigie-Chiro_scripts/data_weather/table_weather_RDS_names.csv",
                               nc_ref_file="C:/git/Vigie-Chiro_scripts/data_weather/normal_weather_precipitation_mean_temp_1950-2000.rds",nc_rep="C:/git/Vigie-Chiro_scripts/data_weather",
                               var=c("precipitation","mean_temp"),
                               dsample_colnames=c("date"="date",
                                                  "longitude"="longitude",
                                                  "latitude"="latitude"),
                               output=TRUE,save=FALSE,fileouput=NULL) {

    library(data.table)
    library(RNetCDF)
    library(climateExtract) #https://github.com/RetoSchmucki/climateExtract
    library(ncdf4)
    library(dplyr)

    ## -------------------------------------------------------------------------
    ## set of parameters to debugging
    ## dsample <- fread("C:/git/VigieChiro/data/data_vigieChiro_DataRP_SpTron_90_site_55sp_withAbs.csv")
    ## dsample <- unique(dsample[,c("year","date","longitude","latitude"),with=FALSE])
##library(lubridate)
    ## dsample$date <- as.Date(substr(dsample$date,1,10),format="%d/%m/%Y")

  ##  dsample=dsample;first_year=NULL;last_year=NULL;time_windows=c(0,3,9);nc_local=TRUE;nc_extract=FALSE;nc_data="C:/git/Vigie-Chiro_scripts/data_weather/table_weather_Rdata_names.csv";nc_rep="C:/git/Vigie-Chiro_scripts/data_weather";nc_ref_file="C:/git/Vigie-Chiro_scripts/data_weather/normal_weather_precipitation_mean_temp_Inf--Inf.Rdata";var=c("precipitation","mean_temp");dsample_colnames=c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitude");output=TRUE;save=FALSE;fileoutput=NULL
    ## -------------------------------------------------------------------------

    dsample <- my_import_fread(dsample,"file of sample data")

    if("site_id" %in% colnames(dsample))
        colnames(dsample)[colnames(dsample)=="site_id"] <- setdiff(c("my_site_id",paste0("my_site_id_",1:10)),colnames(dsample))[1]

    dsample_colnames2 <- dsample_colnames[names(dsample_colnames) != dsample_colnames]
    if(length(dsample_colnames2)>0) for(i in 1:length(dsample_colnames2)) dsample[,names(dsample_colnames2)[i]] <-  dsample[,dsample_colnames2[i],with=FALSE]

    dsample$year <- format(as.Date(dsample$date),"%Y")

    if(is.null(first_year)) first_year <- min(dsample$year)
    if(is.null(last_year)) last_year <- max(dsample$year)

    dsample$longitudeRAW <- dsample$longitude
    dsample$latitudeRAW <-  dsample$latitude
    dsample$longitude <- (floor(dsample$longitude*4)/4)+0.125
    dsample$latitude <-  (floor(dsample$latitude*4)/4)+0.125
    dsample$julian=yday(dsample$date)

    ## ajout d'une colonne d'identifiant longitude latitude
    dsample$longlat <- paste(dsample$longitude,dsample$latitude)

    dsite <- unique(dsample[,c("longlat","longitude","latitude")])
    dsite$site_id <- 1:nrow(dsite)

    dsample <- inner_join(dsample,dsite)

    dsample <- dsample[order(dsample$date,dsample$site_id),]

    if(is.null(nc_ref_file)){
        print("select your nc file at Rdata format or your file of the table of file Rdata paths")
        ## provient de :
        ## precipitation <- extract_nc_value(2014,2019) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
        ## mean_temp <- extract_nc_value(2014,2019) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
        ## save(list=c("precipitation","mean_temp"),file="XXX.Rdata")
        nc_ref_file <- file.choose()
    }

    lnc_mean <- readRDS(nc_ref_file) ## changer ça en RDS
    for(v in var)
        lnc_mean[[v]]$longlat <- paste(lnc_mean[[v]]$longitude,lnc_mean[[v]]$latitude)



    lnc <- list()
    if(nc_local) {
        if(nc_extract) {
            for(v in var){
                cat(" - Variable:",v,"\n-----------------------------\n\n")
                lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = TRUE)
            } # END for(v in var){
        } else { # ELSE  if(nc_extract)
            if(is.null(nc_data)){
                print("select your nc file at Rdata format or your file of the table of file Rdata paths")
                ## provient de :
                ## precipitation <- extract_nc_value(2014,2019) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                ## mean_temp <- extract_nc_value(2014,2019) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
                ## save(list=c("precipitation","mean_temp"),file="XXX.Rdata")
                nc_data <- file.choose()

            } # END if(is.null(lnc))

            if(length(grep(".csv",nc_data))>0) {
                nc_data <- gen_import_fread(nc_data)
                lnc <- NULL

            } else {
                lnc <- readRDS(nc_data)
            }
        } # END  ELSE  if(nc_extract)
    } else { # ELSE if(dnc_local)
        for(v in var){
            cat(" - Variable:",v,"\n-----------------------------\n\n")
            lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = FALSE, clim_variable = v, grid_size = 0.25)
        } # END for(v in var){
    }# END ELSE if(dnc_local)


    weather <- list()
    nbOfCol <- length(temp_windows)
    for(v in var){
        weather[[v]] <- list()
        for(l in c("raw","anomaly","anomaly_st","ref_mean","ref_sd")){
            weather[[v]][[l]] <- data.frame(matrix(NA,nrow(dsample),nbOfCol))
            colnames(weather[[v]][[l]]) <- paste("tw",temp_windows,sep="_")
        } # END for(l in c("raw","anomaly","anomaly_st","ref_mean","ref_sd")){
    } # END for(v in var){


    end_year <- 0
    i <- 1

    while(i <=nrow(dsample)){
        year_i <- dsample$year[i]
        if(is.null(lnc)|year_i > end_year) {
            f <- which(nc_data$start <= year_i & nc_data$end >= year_i)
            file_nc <- nc_data$filename[f]
            end_year <- nc_data$end[f]
            start_year <- nc_data$start[f]
            if(is.null(nc_rep)) nc_rep <- choose.dir(caption="Select the directory that contain the Rdata of weather data")
            file_nc <- paste0(nc_rep,"/",file_nc)
            cat("Loading file:",file_nc)
            lnc <- readRDS(file_nc)
            cat("    DONE! \n")

        } # END if(is.null(lnc)|year_i > end_year) {

        dsite_i <- subset(dsite,site_id %in% subset(dsample,year >= start_year & year <= end_year)$site_id)
        ## ajout d'une colonne d'identifiant longitude latitude


        for(l in 1:length(lnc)) {
            ##l=1
            laVar <- names(lnc)[l]
            cat("Variable météo:",laVar,"\n--------------------------------\n\n")
            ## point.TM matrix date_extract in row and site_id in colmun
            point.TM <- point_grid_extract(lnc[[l]],dsite_i)

            point.TM$julian=yday(point.TM$date_extract)

            theColnames <- paste(paste(rep(var,each=3),rep(c("raw","ano","ano_st"),length(var)),temp_windows))
            nbOfCol <- length(temp_windows)

            ii <- i
            year_ii <- year_i
            while (year_ii <= end_year & ii <= nrow(dsample)) {
                                        #
                if (ii%%500==1){print(paste(ii,Sys.time()))}

                date_j0 <- dsample$date[ii]
                for(tw in temp_windows) {
                    dates <- as_date((as_date(date_j0) - tw + 1):(as_date(date_j0)))
                    dates_julian<- yday(dates)

                    match_site_id <- match(dsample$site_id[ii], colnames(point.TM))
                    match_date <- which(as_date(point.TM$date_extract) %in% dates)

                    val_raw <- mean(point.TM[match_date,match_site_id],na.rm=TRUE)

                    match_ref_long <- match(dsample$longitude[ii], lnc_mean[[v]]$longitude)
                    match_ref_lat <- match(dsample$latitude[ii], lnc_mean[[v]]$latitude)
                    match_ref_date <- which(lnc_mean[[v]]$iiulian %in% dates_julian)

                    ref_mean <- mean(lnc_mean[[laVar]]$value_mean_array[match_ref_long,match_ref_lat,match_ref_date],na.rm=TRUE)
                    ref_sd <- mean(lnc_mean[[laVar]]$value_sd_array[match_ref_long,match_ref_lat,match_ref_date],na.rm=TRUE)


                    val_anomalie <- val_raw - ref_mean
                    if(is.na(ref_sd)) val_anomalie_st <- NA else val_anomalie_st <- val_anomalie / ref_sd

                    name_tw <- paste("tw",tw,sep="_")

                    weather[[v]][["raw"]][ii,name_tw] <- val_raw
                    weather[[v]][["anomaly"]][ii,name_tw] <- val_raw
                    weather[[v]][["anomaly_st"]][ii,name_tw] <- val_raw
                    weather[[v]][["ref_mean"]][ii,name_tw] <- ref_mean
                    weather[[v]][["ref_sd"]][ii,name_tw] <- ref_sd

               }
                ii <- ii + 1
            } # END while (year_j <= end_year)
            ## dsample <- cbind(dsample,weather)
        } # END for(l in 1:length(lnc))
        i <- ii
    } # END while(i <=nrow(dsample)

    dsample <- cbind(dsample,weather)

    if(save) fwrite(dsample,fileouput)
    if(output) return(dsample)
}




##' import data
##' @title gen_import_fread
##' @param d table (data.frame, data.table) or path
##' @param descri text printer in the case of a file.choose()
##' @return data at data.table format
##' @author Romain Lorrilliere
##' @importFrom data.table fread
##'
gen_import_fread <- function(d=NULL,descri=NULL) {
    library(data.table)
    if (is.null(d)) {
        cat("select your data file\n")
        if(!is.null(descri)) cat(descri,"\n")
        d <- fread(file.choose())
    } else { # ELSE if (is.null(d))
        if(class(d)[1]=="character") {
            if(file.exists(d)) {
                cat("Importation:",d,"\n")
                d <- fread(d)
                cat("  DONE !\n")
            } else {
                print("File",d," does not exist, select the correct file:\n")
                d <- fread(file.choose())
            }
        }
        else {
            d <- data.table(d)
        }
    } # END ELSE if (is.null(d))
    return(d)
}





##' Extraction des données à partir des fichier nc téléchargé sur .....
##' @title nc2rdata
##' @param firstYear
##' @param lastYear
##' @param repOut path du répertoire de sortie
##' @return NA
##' @author Romain Lorrilliere
##' @importFrom climateExtract extract_nc_value
nc2rdata <- function(firstYear=1950,lastYear=NULL,repOut="data/") {
                                        #  firstYear=1950;lastYear=NULL;repOut="data/"
    library(climateExtract) #https://github.com/RetoSchmucki/climateExtract

    ## -------------------------------------------------------------------------
    ## set of parameters to debogging
    ## firstYear=1950;lastYear=NULL;repOut="data/"
    ## -------------------------------------------------------------------------

    if(is.null(lastYear)) lastYear <- as.numeric(format(Sys.time(),"%Y"))
    vecAn_start <- seq(firstYear,lastYear,5)
    vecAn_end <- sort(union(seq(firstYear+4,lastYear,5),lastYear))
    dAn <- data.frame(start=vecAn_start,end=vecAn_end)
    dAn$filename <- paste0(repOut,"data_meteo_temp_prec_ens_mean_0.25deg_reg_v20_",dAn$start,"-",dAn$end,".Rdata")
    dAn$start_real <- ifelse(dAn$start>1950,dAn$start -1,1950)
    print(dAn)
    flush.console()

    for(i in 1:nrow(dAn)) {
        cat("\n",paste(dAn[i,],collapse=" | "),"\n\n")
        flush.console()
        cat(" - precipitation: avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc\n")
        flush.console()
        precipitation <- extract_nc_value(dAn$start_real[i],dAn$end[i]) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
        cat(" - mean_temp : avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc\n")
        flush.console()
        mean_temp <- extract_nc_value(dAn$start_real[i],dAn$end[i]) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
        file <- dAn$filename[i]
        cat("SAVE ->",file)
        flush.console()
        save(list=c("precipitation","mean_temp"),file=file)
        cat("   DONE!\n\n")
    }
    write.csv(dAn,paste0(repOut,"table_weather_Rdata_names.csv"))
}




##' Extraction des données à partir des fichier nc téléchargé sur .....
##' @title nc2rdata
##' @param firstYear
##' @param lastYear
##' @param repOut path du répertoire de sortie
##' @return NA
##' @author Romain Lorrilliere
##' @importFrom climateExtract extract_nc_value
nc2rds <- function(firstYear=1950,lastYear=NULL,repOut="C:/git/Vigie-Chiro_scripts/data_weather") {
                                        #  firstYear=1950;lastYear=NULL;repOut="data/"
    library(climateExtract) #https://github.com/RetoSchmucki/climateExtract

    ## -------------------------------------------------------------------------
    ## set of parameters to debogging
    ## firstYear=1950;lastYear=NULL;repOut="data/"
    ## -------------------------------------------------------------------------

    if(is.null(repOut)) repOut <- choose.dir()

    if(is.null(lastYear)) lastYear <- as.numeric(format(Sys.time(),"%Y"))
    vecAn_start <- seq(firstYear,lastYear,5)
    vecAn_end <- sort(union(seq(firstYear+4,lastYear,5),lastYear))
    dAn <- data.frame(start=vecAn_start,end=vecAn_end)
    dAn$filename <- paste0("data_meteo_temp_prec_ens_mean_0.25deg_reg_v20_",dAn$start,"-",dAn$end,".rds")
    dAn$start_real <- ifelse(dAn$start>1950,dAn$start -1,1950)
    print(dAn)
    flush.console()

    for(i in 1:nrow(dAn)) {
        cat("\n",paste(dAn[i,],collapse=" | "),"\n\n")
        flush.console()
        cat("\n - precipitation: avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc\n")
        flush.console()
        precipitation <- extract_nc_value(dAn$start_real[i],dAn$end[i]) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc

        dim(str(precipitation))

        if(precipitation$variable_name != "rainfall") {
            while(precipitation$variable_name != "rainfall") {
                cat("\n!!! Error: the variable_name of the nc file should be 'rainfall'. \nChoose the good file\n rr_ens_mean_0.25deg_reg_v20.0e.nc\n")
                precipitation <- extract_nc_value(dAn$start_real[i],dAn$end[i]) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                dim(str(precipitation))
            }
        }

        cat("\n - mean_temp : avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc\n")
        flush.console()
        mean_temp <- extract_nc_value(dAn$start_real[i],dAn$end[i]) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc

        dim(str(mean_temp))

          if(mean_temp$variable_name != "mean temperature") {
            while(mean_temp$variable_name != "mean temperature") {
                cat("\n!!!Error: the variable_name of the nc file should be 'mean temperature'. \nChoose the good file\ntg_ens_mean_0.25deg_reg_v20.0e.nc\n")
                mean_temp <- extract_nc_value(dAn$start_real[i],dAn$end[i]) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                dim(str(mean_temp))
            }
          }

        if(repOut != "")  file <- paste0(repOut,"/",dAn$filename[i]) else file <- An$filename[i]
        cat("SAVE ->",file)
        flush.console()
        lnc <- list(precipitation,mean_temp)
        names(lnc) <- c("precipitation","mean_temp")
        saveRDS(lnc,file=file)
        cat("   DONE!\n\n")
    }
    write.csv(dAn,paste0(repOut,"/","table_weather_RDS_names.csv"))
}




##' Calcul de la référence des moyennes et précipitation journalière sur une période
##' @title assess_weather_ref
##' @param first_year
##' @param last_year
##' @param nc_one_file
##' @param nc_local
##' @param nc_extract
##' @param nc_data
##' @param nc_rep
##' @param file_nc_out
##' @param var
##' @param dsample_colnames
##' @param output
##' @param save
##' @param fileouput
##' @return
##' @author
assess_weather_ref <- function(first_year=1950,last_year=2000,
                               nc_one_file=FALSE,nc_local=TRUE,
                               nc_extract=FALSE,
                               nc_data="C:/git/Vigie-Chiro_scripts/data_weather/table_weather_RDS_names.csv",
                               nc_rep="C:/git/Vigie-Chiro_scripts/data_weather",
                               file_nc_out=NULL,var=c("precipitation","mean_temp"),
                               dsample_colnames=c("site_id"="site_id",
                                                  "date"="date",
                                                  "longitude"="longitude",
                                                  "latitude"="latitudeWGS84"),
                               output=TRUE,save=TRUE,fileouput=NULL) {

    library(lubridate)

    ## -------------------------------------------------------------------------
    ## set of parameters to debogging
    ##  first_year=1950;last_year=2000;nc_one_file=FALSE;nc_local=TRUE;nc_extract=FALSE;nc_data="data_weather/table_weather_Rdata_names.csv";nc_rep="data_weather";var=c("precipitation","mean_temp");dsample_colnames=c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84");output=TRUE;save=FALSE;fileouput=NULL;file_nc_out=NULL
    ## ------------------------------------------------------------------------


    start_process <- Sys.time()

    cat("\n Start:",format(start_process,"%Y-%m-%d %H:%M"),"\n")

    if(nc_one_file) {
        lnc <- list()
        if(nc_local) {
            if(nc_extract) {
                for(v in var){
                    cat(" - Variable:",v,"\n-----------------------------\n\n")
                    lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = TRUE)
                } # END for(v in var){
            } else { # ELSE  if(nc_extract)
                if(is.null(nc_data)){
                    print("select your nc file at rds format")
                    ## provient de :
                    ## precipitation <- extract_nc_value(2014,2019) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                    ## mean_temp <- extract_nc_value(2014,2019) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
                    ## save(list=c("precipitation","mean_temp"),file="XXX.Rdata")
                    lnc <- readRDS(file.choose())
                } else { # ELSE  if(is.null(lnc))
                    lnc <- readRDS(nc_data)
                }# END ELSE  if(is.null(lnc))
            } # END  ELSE  if(nc_extract)
        } else { # ELSE if(dnc_local)
            for(v in var){
                cat(" - Variable:",v,"\n-----------------------------\n\n")
                lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = FALSE, clim_variable = v, grid_size = 0.25)
            } # END for(v in var){
        }# END ELSE if(dnc_local)
    } else { # ELSE if(nc_one_file)
        if(is.null(nc_rep)) nc_rep <- choose.dir()
        ## nc_data table de description des Rdata
        nc_data_table<- my_import_fread(nc_data)
        if(!is.null(first_year)) nc_data_table <- subset(nc_data_table,end >= first_year)
        if(!is.null(last_year)) nc_data_table <- subset(nc_data_table,start <= last_year)
        nc_data <- NULL
    }# END ELSE if(nc_one_file)


    if(nc_one_file) {
        lnc_mean <- list()
        for(v in var){
            cat("Variable:",v,"\n")
            lnc[[v]]$julian_day <- yday(as.Date(lnc[[v]]$date_extract))

            array_sum <- array(NA,dim=c(dim(lnc[[v]]$value_array)[1],dim(lnc[[v]]$value_array)[2],366))
            array_nb <- array(NA,dim=c(dim(lnc[[v]]$value_array)[1],dim(lnc[[v]]$value_array)[2],366))

            cat(" - Mean assessment\n")

            for(j in 1:366){
                array_j <- lnc[[v]]$value_array[,,which(lnc[[v]]$julian_day == j)]

                array_sum[,,j] <- rowSums(array_j,dims=2,na.rm=TRUE)
                array_j[!is.na(array_j)] <- 1
                array_j[is.na(array_j)] <- 0
                array_nb[,,j] <- rowSums(array_j,dims=2,na.rm=TRUE)
            } #END for(j in 1:366){
            array_mean <- array_sum/array_nb

            array_sum_diff <- array(NA,dim=c(dim(lnc[[v]]$value_array)[1],dim(lnc[[v]]$value_array)[2],366))

            cat(" - Sd assessment\n")

            for(j in 1:366){
                array_j <- lnc[[v]]$value_array[,,which(lnc[[v]]$julian_day == j)]
                for(z in 1:dim(array_j)[3]) array_j[,,z] <- (array_j[,,z]-array_mean[,,j])^2
                array_sum_diff[,,j] <- rowSums(array_j,dims=2,na.rm=TRUE)
            } #END for(j in 1:366){
            array_sd <- array_sum_diff/array_nb


            years <- unique(year(lnc[[v]]$date_extract))


            date2000 <- format(as.Date(paste0(last_year,"-",1:366),"%Y-%j"),"%m-%d")

            lnc_mean[[v]] <- list(variable_name=lnc[[v]]$variable_name,value_mean_array=array_mean,value_sd_array=array_sd,longitude=lnc[[v]]$longitude,latitude=lnc[[v]]$latitude,date_extract=date2000,julian_day=1:366,years=years)
        } #END for(v in var){



    } else {# ELSE  if(nc_one_file) {

        larray <- NULL
        lnc <- list()
        cat("\n-------------------\n- Mean assessment -\n-------------------\n")

        for(i in 1:nrow(nc_data_table)) {
            file_i <- paste0(nc_rep,"/",nc_data_table$filename[i])
            start_i <-  nc_data_table$start[i]
            end_i <-  nc_data_table$end[i]
            start <- max(start_i,first_year)
            end <- min(end_i,last_year)
            cat("LOAD:",file_i)
            lnc <- readRDS(file_i)
            cat("  DONE\n")

            cat(" 0- Initialisation\n")

            for(v in var) {
                cat("  variable:",v,"\n")

                lnc[[v]][["year"]] <- year(lnc[[v]]$date_extract)
                lnc[[v]]$value_array <- lnc[[v]]$value_array[,,which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$year <- lnc[[v]]$year[which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$date_extract <- lnc[[v]]$date_extract[which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$julian_day <- yday(as.Date(lnc[[v]]$date_extract))

            } #END for(v in var) {

            ## initialisation des sortie et du calcul
            if(is.null(larray)) {
                larray <- list()
                for(vv in var) {
                    array_sum <- array(0,dim=c(dim(lnc[[vv]]$value_array)[1],dim(lnc[[vv]]$value_array)[2],365))
                    array_nb <- array(0,dim=c(dim(lnc[[vv]]$value_array)[1],dim(lnc[[vv]]$value_array)[2],365))
                    array_sum_diff <- array(0,dim=c(dim(lnc[[vv]]$value_array)[1],dim(lnc[[vv]]$value_array)[2],365))
                    larray[[vv]] <- list(array_sum=array_sum,array_nb=array_nb,array_sum_diff=array_sum_diff,years=NULL)
                }
            }

            cat(" 1- somme sur jour julien\n")
            for(v in var) {
                cat("  variable:",v,"\n")
                cat("     boucle jour julien: ")
                for(j in 1:365){
                    ##                    if(v=="mean_temp") browser()
                    if(j %% 30 == 0) cat(j,"")
                    array_j <- lnc[[v]]$value_array[,,which(lnc[[v]]$julian_day == j)]

                    if(length(dim(array_j))==3) {
                        larray[[v]]$array_sum[,,j] <- larray[[v]]$array_sum[,,j] + rowSums(array_j,dims=2,na.rm=TRUE)
                        array_j[!is.na(array_j)] <- 1
                        array_j[is.na(array_j)] <- 0
                        larray[[v]]$array_nb[,,j] <- larray[[v]]$array_nb[,,j] + rowSums(array_j,dims=2,na.rm=TRUE)
                    } else { # ELSE if(length(dim(array_j))==3) {
                        larray[[v]]$array_sum[,,j] <- larray[[v]]$array_sum[,,j] + array_j
                        array_j[!is.na(array_j)] <- 1
                        array_j[is.na(array_j)] <- 0
                        larray[[v]]$array_nb[,,j] <- larray[[v]]$array_nb[,,j] + array_j
                   } # END ELSE if(length(dim(array_j)) == 3) {
                } #END for(j in 1:366){
                larray[[v]]$years <- sort(union(larray[[v]]$years,lnc[[v]]$year))
                cat("\n")
            } #END  for(v in var) {
            cat("\n")
            for(v in var)  rm(v)

        }# END for(i in 1:nrow(nc_data_table)) {

        cat("  2- calcul moyenne:\n")
        for(v in var) {
            cat("  variable:",v,"   | ")
            for(i in 1:nrow(nc_data_table)) {
                cat(i,"")
                larray[[v]][["array_mean"]] <- larray[[v]]$array_sum/larray[[v]]$array_nb
            }
            cat("\n")
        }

        cat("\n-----------------\n- Sd assessment -\n-----------------\n")
        for(i in 1:nrow(nc_data_table)) {
            file_i <- paste0(nc_rep,"/",nc_data_table$filename[i])
            start_i <-  nc_data_table$start[i]
            end_i <-  nc_data_table$end[i]
            start <- max(start_i,first_year)
            end <- min(end_i,last_year)
            cat("LOAD:",file_i)
            lnc <- readRDS(file_i)
            cat("  DONE\n")

            cat(" 0- Initialisation\n")

            for(v in var) {
                cat("  variable:",v,"\n")

                lnc[[v]][["year"]] <- year(lnc[[v]]$date_extract)
                lnc[[v]]$value_array <- lnc[[v]]$value_array[,,which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$year <- lnc[[v]]$year[which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$date_extract <- lnc[[v]]$date_extract[which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$julian_day <- yday(as.Date(lnc[[v]]$date_extract))

            }

            cat(" 1- somme ecart moyenne sur jour julien\n")

            for(v in var) {
                cat("  variable:",v,"\n")
                cat("     boucle jour julien: ")
                for(j in 1:365){
                    if(j %% 30 == 0) cat(j,"")
                    array_j <- lnc[[v]]$value_array[,,which(lnc[[v]]$julian_day == j)]
                    if(length(dim(array_j)) == 3) {

                        for(z in 1:dim(array_j)[3])
                            array_j[,,z] <- (array_j[,,z]- larray[[v]][["array_mean"]][,,j])^2

                        larray[[v]]$array_sum_diff[,,j] <- larray[[v]]$array_sum_diff[,,j] + rowSums(array_j,dims=2,na.rm=TRUE)
                    } else { # ELSE if(length(dim(array_j)) == 3) {
                        array_j <- (array_j-larray[[v]][["array_mean"]][,,j])^2
                        larray[[v]]$array_sum_diff[,,j] <- larray[[v]]$array_sum_diff[,,j] + array_j
                    } # END ELSE if(length(dim(array_j)) == 3) {

                } #END for(j in 1:366){
                cat("\n")
            } #END  for(v in var) {
            cat("\n")
        }# END for(i in 1:nrow(nc_data_table)) {  1- somme ecart moyenne sur jour julien
        cat("  2- calcul ecart type:\n")
        for(v in var) {
            cat("  variable:",v,"   | ")
            for(i in 1:nrow(nc_data_table)) {
                cat(i,"")
                larray[[v]][["array_sd"]] <- larray[[v]]$array_sum_diff/larray[[v]]$array_nb
            }
            cat("\n")
        } # END for(v in var) {  2- calcul ecart type:


        cat("\n--------------------------\n- Preparation sauvegarde -\n--------------------------\n")
        lnc_mean <- list()
        cat("  variable:",v,"\n")
        for(v in var) {
            years <- unique(larray[[v]]$year)
            date_M_J<- format(as.Date(paste0(last_year,"-",1:366),"%Y-%j"),"%m-%d")

            lnc_mean[[v]] <- list(variable_name=lnc[[v]]$variable_name,value_mean_array=larray[[v]]$array_mean,value_sd_array=larray[[v]]$array_sd,longitude=lnc[[v]]$longitude,latitude=lnc[[v]]$latitude,date_extract=date_M_J,julian_day=1:366,years=years)
        } # END for( v in var) {

    }# END ELSE  if(nc_one_file) {

    if(is.null(file_nc_out))
        file_nc_out <- paste0("normal_weather_",paste(var,collapse="_"),"_",min(years),"-",max(years),".rds")

    file_save_nc <- paste0(nc_rep,"/",file_nc_out)

    cat("  -->", file_save_nc)
    saveRDS(lnc_mean,file=file_save_nc)
    cat("   DONE!\n")

    end_process <- Sys.time()

    cat("\n End:",format(end_process,"%Y-%m-%d %H:%M"),"\n")
    duration <- round(difftime(end_process,start_process,units="mins"))
    cat("    Duration:",duration,"minutes\n")

} # END assess_weather_ref <- function(


plot_nc_date <- function(nc=lnc_mean[["precipitation"]],var="value_mean_array",date="01-01",title="Precipitation",points=NULL,spatial_crop=NULL) {
    require(ggplot2)


    match_ref_date <- match(date, lnc_mean[[v]][["date_extract"]])
    data <- nc[[var]][,,match_ref_date]
    ggdata <- data.table(expand.grid(nc[["longitude"]],nc[["latitude"]]),as.vector(data))
    colnames(ggdata) <- c("longitude","latitude","value")

    gg <- ggplot(data=ggdata,aes(x=longitude,y=latitude,colour=value)) + geom_point()
    if(!is.null(points)) gg <- gg + geom_point(aes(x=points$longitude,y=points$latitude),colour="black",size=3)
    if(!is.null(spatial_crop)) gg <- gg + coord_cartesian(xlim = c(min(points$longitude) - 5,max(points$longitude) + 5), ylim = c(min(points$latitude) - 5,max(points$latitude) - 5))
    gg <- gg + labs(title=paste(title,date))
    print(gg)

}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param d
##' @param descri
##' @return
##' @author
my_import_fread <- function(d,descri=NULL) {
    library(data.table)
      if (is.null(d)) {
          cat("select your data file\n")
          if(!is.null(descri)) cat(descri,"\n")
        d <- fread(file.choose())
    } else { # ELSE if (is.null(d))
        if(class(d)[1]=="character") {
            if(file.exists(d)) {
                cat("Importation:",d,"\n")
                d <- fread(d)
                cat("  DONE !\n")
            } else {
                print("File",d," does not exist, select the correct file:\n")
                d <- fread(file.choose())
            }
        }
        else {
            d <- data.table(d)
        }
    } # END ELSE if (is.null(d))
    return(d)
}




fix_bug_rds_file <- function() {
    library(data.table)

    tab <- fread("../data_weather/table_weather_RDS_names.csv")

    var=c("precipitation","mean_temp")
    for(f in tab$filename){
        cat("\n",f,"\n")
        file <- paste0("../data_weather/",f)

        lnc <- readRDS(file)
        print(str(lnc))
        names(lnc) <- var
        print(str(lnc))
        saveRDS(lnc,file)
        cat("DONE !\n")
    }



}
