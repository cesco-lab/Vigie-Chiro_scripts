
##  Groupe de function pour récupérer les données météo





##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title get weather anomalie form sample data
##' @param dsample data or data path or NULL
##' @param first_year if NULL first year of data sample
##' @param last_year  if NULL last year of data sample
##' @param nc_local TRUE to extract value in local file
##' @param nc_extract TRUE if you want use the extract_nc_value function
##' @param nc_data  path of the Rdata of the weather data
##' @param var weather variables
##' @param dsample_colnames names of important columns in sample table defaut c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84")
##' @param output boolean export table in console
##' @param save boolean save or not the table
##' @param fileouput name of file to save the table
##' @return la table sample avec les anomalies des variables météo désiré à 1,3,9,27 et 81 jours
##' @author Romain Lorrilliere à partir d'un code de Yves Bas
get_sample_weather <- function(dsample=NULL,first_year=NULL,last_year=NULL,nc_local=TRUE,nc_extract=FALSE,nc_data=NULL,nc_rep=NULL,var=c("precipitation","mean_temp"),dsample_colnames=c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84"),output=TRUE,save=FALSE,fileouput=NULL) {

    library(data.table)
    library(RNetCDF)
    library(climateExtract) #https://github.com/RetoSchmucki/climateExtract
    library(ncdf4)

      dsample=dsample;first_year=NULL;last_year=NULL;nc_local=TRUE;nc_extract=FALSE;nc_data=NULL;var=c("precipitation","mean_temp");dsample_colnames=c("site_id"="INSEE","date"="DATE_NIGHT_POSIX","longitude"="X_CENTROID","latitude"="Y_CENTROID");output=TRUE;save=FALSE;fileoutput=NULL


    dsample <- my_import_fread(dsample,"file of sample data")
    nc_data <- my_import_fread(nc_data,"file of sample data")


    dsample_colnames2 <- setdiff(names(dsample_colnames),colnames(dsample))
    if(length(dsample_colnames2)>0) for(n in dsample_colnames2) dsample[,n] <-  dsample[,dsample_colnames[names(dsample_colnames)==n],with=FALSE]

    dsample$year <- format(as.Date(dsample$date),"%Y")

    if(is.null(first_year)) first_year <- min(dsample$year)
    if(is.null(last_year)) last_year <- max(dsample$year)

    lnc <- list()
    if(nc_local) {
        if(nc_extract) {
            for(v in var){
                cat(" - Variable:",v,"\n-----------------------------\n\n")
                lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = TRUE)
            } # END for(v in var){
        } else { # ELSE  if(nc_extract)
            if(is.null(nc_data)){
                print("select your nc file at Rdata format")
                ## provient de :
                ## precipitation <- extract_nc_value(2014,2019) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                ## mean_temp <- extract_nc_value(2014,2019) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
                ## save(list=c("precipitation","mean_temp"),file="XXX.Rdata")
                load(file.choose())
            } else { # ELSE  if(is.null(lnc))
                load(nc_data)
            }# END ELSE  if(is.null(lnc))
            for(v in var)
                lnc[[v]] <-  get(v)
        } # END  ELSE  if(nc_extract)
    } else { # ELSE if(dnc_local)
        for(v in var){
            cat(" - Variable:",v,"\n-----------------------------\n\n")
            lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = FALSE, clim_variable = v, grid_size = 0.25)
        } # END for(v in var){
    }# END ELSE if(dnc_local)

    dsample$longitudeRAW <- dsample$longitude
    dsample$latitudeRAW <-  dsample$latitude
    dsample$longitude <- (floor(dsample$longitude*4)/4)+0.125
    dsample$latitude <-  (floor(dsample$latitude*4)/4)+0.125

    dsite <- unique(dsample[,c("site_id","longitude","latitude")])

    for(l in 1:length(lnc)) {

        laVar <- names(lnc)[l]
        cat("Variable météo:",laVar,"\n--------------------------------\n\n")

        point.TM <- point_grid_extract(lnc[[l]],dsite)

        Jour=yday(point.TM$date_extract)

        weather <- data.frame(matrix(NA,nrow(dsample),5))
        colnames(weather) <- paste(laVar,c("A1","A3","A9","A27","A81"),sep="_")

        for (i in 1:nrow(dsample)) {
                                        # cat(i,"")
            if (i%%100==1){print(paste(i,Sys.time()))}
            MatchLongLat=match(paste(dsample$longitude[i],dsample$latitude[i]),paste(dsite$longitude,dsite$latitude))
            MatchDate=match(dsample$date[i],as.character(point.TM$date_extract))
            T1=point.TM[MatchDate,(MatchLongLat+1)]
            D1=point.TM$date_extract[MatchDate]
            J1=yday(D1)
            J1_30=match(Jour,J1)
            N1=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J1_30)))
            weather$AT1[i]=T1-N1

            browser()
            T3=mean(point.TM[(MatchDate-2):MatchDate,(MatchLongLat+1)])
            J3=c((J1-2):J1) #last 3 days
            J3=J3-floor((J3-1)/365)*365 #to keep in 1:365 domain
            J3_30=match(Jour,J3)
            N3=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J3_30)))
            weather$AT3[i]=T3-N3

            T9=mean(point.TM[(MatchDate-8):MatchDate,(MatchLongLat+1)])
            J9=c((J1-8):J1) #last 9 days
            J9=J9-floor((J9-1)/365)*365 #to keep in 1:365 domain
            J9_30=match(Jour,J9)
            N9=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J9_30)))
            weather$AT9[i]=T9-N9

            T27=mean(point.TM[(MatchDate-26):MatchDate,(MatchLongLat+1)])
            J27=c((J1-26):J1) #last 27 days
            J27=J27-floor((J27-1)/365)*365 #to keep in 1:365 domain
            J27_30=match(Jour,J27)
            N27=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J27_30)))
            weather$AT27[i]=T27-N27

            T81=mean(point.TM[(MatchDate-80):MatchDate,(MatchLongLat+1)])
            J81=c((J1-80):J1) #last 81 days
            J81=J81-floor((J81-1)/365)*365 #to keep in 1:365 domain
            J81_30=match(Jour,J81)
            N81=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J81_30)))
            weather$AT81[i]=T81-N81
        } #END for (i in 1:nrow(dsample))
        dsample <- cbind(dsample,weather)
    } # END for(l in 1:length(lnc))

    if(save) fwrite(dsample,fileouput)


    if(output) return(dsample)

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
assess_weather_ref <- function(first_year=1950,last_year=2000,nc_one_file=FALSE,nc_local=TRUE,nc_extract=FALSE,nc_data="data_weather/table_weather_Rdata_names.csv",nc_rep="data_weather",file_nc_out=NULL,var=c("precipitation","mean_temp"),dsample_colnames=c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84"),output=TRUE,save=FALSE,fileouput=NULL) {

  #  first_year=1950;last_year=2000;nc_one_file=FALSE;nc_local=TRUE;nc_extract=FALSE;nc_data="data_weather/table_weather_Rdata_names.csv";nc_rep="data_weather";var=c("precipitation","mean_temp");dsample_colnames=c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84");output=TRUE;save=FALSE;fileouput=NULL;file_nc_out=NULL

    library(lubridate)

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
                    print("select your nc file at Rdata format")
                    ## provient de :
                    ## precipitation <- extract_nc_value(2014,2019) # avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                    ## mean_temp <- extract_nc_value(2014,2019) # avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
                    ## save(list=c("precipitation","mean_temp"),file="XXX.Rdata")
                    load(file.choose())
                } else { # ELSE  if(is.null(lnc))
                    load(nc_data)
                }# END ELSE  if(is.null(lnc))
                for(v in var)
                    lnc[[v]] <-  get(v)
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
            load(file_i)
            cat("  DONE\n")

            cat(" 0- Initialisation\n")
            lnc <- list()
            for(v in var) {
                cat("  variable:",v,"\n")
                lnc[[v]] <-  get(v)
                lnc[[v]][["year"]] <- year(lnc[[v]]$date_extract)
                lnc[[v]]$value_array <- lnc[[v]]$value_array[,,which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$year <- lnc[[v]]$year[which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$date_extract <- lnc[[v]]$date_extract[which(lnc[[v]]$year >= start & lnc[[v]]$year <= end)]
                lnc[[v]]$julian_day <- yday(as.Date(lnc[[v]]$date_extract))
            }

             ## initialisation des sortie et du calcul
            if(is.null(larray)) {
                larray <- list()
                for(vv in var) {
                    array_sum <- array(0,dim=c(dim(lnc[[vv]]$value_array)[1],dim(lnc[[vv]]$value_array)[2],365))
                    array_nb <- array(0,dim=c(dim(lnc[[vv]]$value_array)[1],dim(lnc[[vv]]$value_array)[2],365))
                    array_sum_diff <- array(0,dim=c(dim(lnc[[vv]]$value_array)[1],dim(lnc[[vv]]$value_array)[2],365))
                    larray[[vv]] <- list(array_sum=array_sum,array_nb=array_nb,array_sum_diff=array_sum_diff)
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
            load(file_i)
            cat("  DONE\n")

             cat(" 0- Initialisation\n")
            lnc <- list()
            for(v in var) {
                cat("  variable:",v,"\n")
                lnc[[v]] <-  get(v)
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
                    ##browser()
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
        file_nc_out <- paste0("normal_weather_",paste(var,collapse="_"),"_",min(years),"-",max(years),".Rdata")

    file_save_nc <- paste0(nc_rep,"/",file_nc_out)

    cat("  -->", file_save_nc)
    save(lnc_mean,file=file_save_nc)
    cat("   DONE!\n")

   end_process <- Sys.time()

    cat("\n End:",format(end_process,"%Y-%m-%d %H:%M"),"\n")
    duration <- round(difftime(end_process,start_process,units="mins"))
    cat("    Duration:",duration,"minutes\n")

}
