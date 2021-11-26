##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param id
##' @param donneesAll
##' @param donneesName
##' @param method
##' @param only_direct
##' @param only_exp
##' @param seuilOccu
##' @param col_sp
##' @param col_date_julien
##' @param col_site
##' @param col_nbcontact
##' @param assessIC
##' @param listSp
##' @param tabsp
##' @param first_year
##' @param last_year
##' @param figure
##' @param description
##' @param tendanceSurFigure
##' @param tendanceGroupSpe
##' @param seuilSignif
##' @param seuilAbond
##' @param ecritureStepByStep
##' @param doBeep
##' @param VarEffect
##' @return
##' @author Romain Lorrilliere
main.glm <- function(id=NULL
                     ,
                     donneesAll=list("data/data_vigieChiro_DataRP_SpTron_90_site_55sp_withAbs.csv","data/data_vigieChiro_DataRP_SpTron_50_site_55sp_withAbs.csv")
                     ,
                     donneesName=c("90","50")
                     ,
                     method="glmmTMB"
                     ,
                     family="nbinom2"
                     ,
                     only_direct="Auto"
                     ,
                     only_exp="Auto"
                     ,
                     seuilOccu=2
                     ,
                     col_sp="espece"
                     ,
                     col_date_julien="julian"
                     ,
                     col_site="site"
                     ,
                     col_nbcontact="nb_contacts"
                     ,
                     assessIC= TRUE
                     ,
                     listSp=NULL
                     ,
                     tabsp="library/SpeciesList.csv"
                     ,
                     first_year=NULL
                     ,
                     last_year=NULL
                     ,
                     figure=TRUE
                     ,
                     description=c("Abondances brutes","Occurrences"
                                   ,"Proportion","Nombre de sites")
                     ,
                     tendanceSurFigure=TRUE
                     ,
                     tendanceGroupSpe = FALSE
                     ,
                     seuilSignif=0.05
                     ,
                     seuilAbond=NA
                     ,
                     ecritureStepByStep=TRUE
                     ,
                     doBeep=FALSE
                     ,
                     VarEffect=""
                     ,
                     RandomEffect="1|site"
                     ) {
  
  ##    id="testGLMTMB_aggregate";donneesAll=list("data/data_vigieChiro_DataRP_SpTron_90_site_55sp_withAbs.csv","data/data_vigieChiro_DataRP_SpTron_50_site_55sp_withAbs.csv");donneesName=c("90","50");assessIC= TRUE;listSp=NULL;annees=NULL;figure=TRUE;description=c("Abondances brutes","Occurrences","Proportion","Nombre de sites");tendanceSurFigure=TRUE;tendanceGroupSpe = FALSE;seuilOccu=3;seuilAbond=NA;ecritureStepByStep=FALSE;   seuilSignif <- 0.05;tabsp="library/SpeciesList.csv"; col_sp="espece";col_date_julien="julian";col_site="site";col_nbcontact="nb_contacts_strict";col_year="year";doBeep=FALSE;first_year=NULL;last_year=NULL;only_direct = "Auto";seuilOccu=3
  
  require(arm)
  require(ggplot2)
  require(data.table)
  
  RandomTerm=paste0("+",RandomEffect)
  start <- Sys.time() ## heure de demarage
  
  cat("\n\n ============================================= \n")
  cat("     START: ",format(start, "%d-%m-%Y %HH%M"),"  ... \n ")
  cat(" ============================================= \n")
  
  if(is.null(id))
    id <- Sys.Date()
  nb_data <- length(donneesAll)
  
  
  
  if(length(donneesName) != nb_data) stop("le nombre de nom des donnees 'donneesName' ne correspond pas au nombre de données !!\n")
  
  donnees <- NULL
  for(i in 1:length(donneesAll)) {
    
    if(class(donneesAll[[i]])[1] == "character")
      donneesAll[[i]] <- fread(donneesAll[[i]])
    
    if("data" %in% colnames(donneesAll[[i]])) {
      colnames(donneesAll[[i]])[colnames(donneesAll[[i]])=="data"] <- "data_ex"
    }
    
    donneesAll[[i]]  <- data.table(data=donneesName[i],donneesAll[[i]] )
    donnees <- rbind(donnees,donneesAll[[i]] )
  }
  
  filtreAn <- FALSE
  if(is.null(first_year)) first_year <- min(donnees$year) else filtreAn <- TRUE
  if(is.null(last_year)) last_year <- max(donnees$year) else filtreAn <- TRUE
  
  annees <- first_year:last_year
  
  if(filtreAn) donnees <- subset(donnees,year >= first_year & year <= last_year)
  
  
  if(!(is.null(listSp)))
    donnees <- subset(donnees,espece %in% listSp) else listSp <- unique(donnees$espece)
  
  dir.create(paste("Output/",id,sep=""),showWarnings=FALSE)
  filesaveAn <-  paste("Output/",id,"/variationsAnnuellesEspece_",id,".csv",sep = "")
  filesaveTrend <-  paste("Output/",id,"/tendanceGlobalEspece_",id,".csv",sep = "")
  filesavedgg <-  paste("Output/",id,"/table_ggplot_",id,".csv",sep = "")
  
  ##   fileSaveGLMs <-  paste("Output/",id,"/listGLM_",id,sep = "")
  
  
  ## tabsp table de reference des especes
  if(class(tabsp) == "character")
    tabsp <- fread(tabsp)
  colnames(tabsp)[colnames(tabsp)=="Esp"] <- "espece"
  colnames(tabsp)[colnames(tabsp)=="NomFR"] <- "nom_espece"
 # tabsp <- subset(tabsp,Group == "bat")
  
  
  #donnees <- filtreAnalyse(donnees,tabsp)
  
  
  ##vpan vecteur des panels de la figure
  allowedValue <- c("Occurrences","Nombre de sites","Proportion","Abondances brutes")
  noAllowedDescription <- setdiff(description,allowedValue)
  if(length(noAllowedDescription)>0) {
    cat(" WARNINGS !!! description value not allowed: ",noAllowedDescription,"\n")
    cat(" These values are exclude \n")
    description <- intersect(description,allowedValue)
    cat("   -> ", description,"\n")
  }
  
  vpan <- c("Variation abondance",description)
  
  print("L1063")
  ## description des data
  
  dysite <- aggregate(nb_contacts_strict~year + site + expansion_direct + espece +data ,donnees,max)
  dysite$occ <- ifelse(dysite$nb_contacts_strict > 0, 1,0)
  dy <- aggregate(cbind(dysite$nb_contacts_strict,dysite$occ)~ year +expansion_direct + espece + data,dysite,sum)
  colnames(dy)[5:6] <- c("nb_contacts","occ")
  
  
  dsample <- aggregate(nb_contacts_strict~year + expansion_direct + espece + data,unique(donnees[,c("nb_contacts_strict","year","site","expansion_direct","espece","data")]),length)
  colnames(dsample)[5] <- "nb_site"
  dy <- inner_join(dy,dsample)
  dy$proportion <- dy$occ / dy$nb_site
  
  
  
  
  ## Ordre de traitement des especes
  
  
  spOrdre <- aggregate(nb_contacts_strict~espece,data=subset(donnees,data=donneesName[1]),sum)
  spOrdre <- inner_join(spOrdre,tabsp)
  
  spOrdre <- spOrdre[order(spOrdre$nb_contacts_strict,decreasing = TRUE),]
  
  
  listSp <- spOrdre$espece[spOrdre$espece %in% listSp]
  
  nbSp <- length(listSp)
  ## analyse par espece
  print("L1093")
  ## affichage des especes conservé pour l'analyse
  cat("\n",nbSp," Espèces conservées pour l'analyse\n\n",sep="")
  rownames(tabsp) <- tabsp$espece
  tabCons <-data.table(subset(spOrdre,select=c("espece","nom_espece")))
  print(tabCons)
  
  cat("\n",sep="")
  cat(paste(listSp,collapse=", "))
  
  cat("\n\n",sep="")
  flush.console()
  
  dy <- inner_join(dy,tabCons)
  
  
  
  dy_direct <- subset(dy,expansion_direct == "direct", select= setdiff(colnames(dy),"expansion_direct"))
  colnames(dy_direct)[4:7] <- paste( colnames(dy_direct)[4:7],"direct",sep="_")
  dy_exp <-  subset(dy,expansion_direct == "exp",select= setdiff(colnames(dy),"expansion_direct"))
  colnames(dy_exp)[4:7] <- paste( colnames(dy_exp)[4:7],"exp",sep="_")
  dDescri <- full_join(dy_direct,dy_exp)
  
  ## initialisation de la liste de sauvegarde
  dgg <- NULL
  i <- 1
  for(j in c(6,7,8,5)) {
    name <- colnames(dy)[j]
    cat(name,"->",allowedValue[i],"\n")
    dgg <- rbind(dgg,data.table(id=id,data=dy$data,espece=dy$espece,nom_espece=dy$nom_espece,year=dy$year,year_var=0,val=dy[,j],LL = NA,UL=NA,catPoint=NA,pval=NA,courbe= dy$expansion_direct,courbe2=paste(name,dy$expansion_direct,sep="_"),panel=allowedValue[i]))
    i <- i+1
  }
  
  tabAn_f_sp <- NULL
  tab_f_sp <- NULL
  dAn <- NULL
  dTrend <- NULL
  using_ExpDirect <- data.table(expand.grid(data=donneesName,espece=listSp),exp=NA,direct=NA)
  
  i=1
  for (i  in 1:length(listSp)) {
    
    print("L1134")
    sp <- listSp[i]
    nomSp <- tabCons$nom_espece[i]
    cat("\n========================================\n",sep="")
    cat("(",i,"/",nbSp,") ",sp," | ", nomSp,"\n",sep="")
    cat("========================================\n",sep="")
    
    
    flush.console()
    ## d data pour l'espece en court
    for(dn in donneesName) {
      ## dn <- donneesName[1]
      cat("         -  data:",dn," -\n")
      cat("         ================\n")
      
      d <- subset(donnees,espece==sp & data == dn)
      
      if(length(unique(d$sample_cat)==1)){
        VarEffect=subset(VarEffect,VarEffect!="sample_cat")
      }
      
      
      
      
      ## verification si données expansion seront utilisées
      if(only_direct == "Auto") {
        ## median of the expansion occurence
        #med_occ_exp <- median(subset(dDescri,espece == sp & data == dn)$occ_exp,na.rm=TRUE)
        if(nrow(dy_exp)==0)
        {
          without_exp=T
        }else{
        sum_occ_exp <- sum(subset(dDescri,espece == sp & data == dn)$occ_exp,na.rm=TRUE)
        without_exp <- (sum_occ_exp < seuilOccu)
        }
      } else {
        
        without_exp <- sp %in% only_direct
      }
      
      if(min(d$expansion_direct=="direct")==1){
        without_exp=T
      }
      
      
      using_ExpDirect  <-  using_ExpDirect[espece ==sp & data == dn,exp:= !without_exp]
      if(without_exp) 
      {
        VarEffect=subset(VarEffect,VarEffect!="expansion_direct")
        d <- subset(d,expansion_direct != "exp")
        if("nb_contacts_strict" %in% colnames(d))
        {
          d <- subset(d,!is.na(nb_contacts_strict))
        }
        #cat("\n !! Number of occurence in expansion (",med_occ_exp,") inf to",seuilOccu,"\n   data expansion will not used !!\n")
        print("no data in expansion")
        }
      
      ## verification si données direct seront utilisées
      if(only_exp == "Auto") {
        ## median of the expansion occurence
        med_occ_direct <- median(subset(dDescri,espece == sp & data == dn)$occ_direct,na.rm=TRUE)
        without_direct <- (med_occ_direct < seuilOccu)
      } else {
        without_direct <- sp %in% only_exp
      }
      
      using_ExpDirect  <-  using_ExpDirect[espece ==sp & data == dn,direct:= !without_direct]
      if(without_direct) d <- subset(d,expansion_direct != "direct" & !is.na(nb_contacts_strict))
      
      if(without_direct) cat("\n !! Number of occurence in direct (",med_occ_direct,") inf to",seuilOccu,"\n   data direct will not used !!\n")
      
      
      if(without_direct & without_exp) {
        cat(" mediane des occurences direct et expansion inferieure ou égale à ",seuilOccu," \n")
        cat("   espece trop rare\n")
        cat(" -> modèle non réalisé\n\n")
        
      } else {  # ELSE if(without_direct & without_exp)
        
        print("L1188")
        
        
        ## des variable annees
        annee <- sort(unique(donnees$year))
        nbans <- length(annee)
        pasdetemps <- nbans-1
        firstY <- min(annee)
        lastY <- max(annee)
        
        
        
        med_occ <- median(subset(dgg, espece == sp & courbe2 == "occ_direct" &  data == dn)$val)
        
        print("L1416")
        
        cat("\nModèle tendance\n---------------\n")
        
          if(method == "glmmTMB") {
            theta_f <- NA
            repout <- paste("./output/",id,"/",sep="")
            dir.create(dirname(repout))
            dir.create(repout)
          
            myListEffect = VarEffect
          if(without_exp | without_direct) myListEffect <- setdiff(myListEffect,"expansion_direct")
          if(length(unique(d$mat))==1){myListEffect <- setdiff(myListEffect,"mat")}
            #myListEffect <- setdiff(myListEffect,"SpBioC1")
            #myListEffect <- setdiff(myListEffect,"SpBioC12")
            if(length(unique(d$NWN_NCEP_0_0))==1){myListEffect <- setdiff(myListEffect,"NWM_NCEP_0_0")}
            
            md_c <- try(Sp_GLM_short(dataFile=id,varInterest=col_nbcontact
                                   ,listEffects=myListEffect,interactions=NA
                                   ,formulaRandom=RandomTerm
                                   ,selSample=1e10
                                   ,tagModel=paste0("GLMalphatest_tendancesFY",id,"_",sp)
                                   ,family=family,asfactor=NA,data=d,repout=repout
                                   ,checkRepout=TRUE,saveFig=TRUE,output=TRUE
                                   ,doBeep=doBeep,printFormula=TRUE),silent=TRUE)
          
          if(class(md_c)[1] != "try-error") {
            smd_c <- md_c[[2]]
            print("L1513")
            vif_c_mean <- mean(smd_c$VIF)
            vif_c_max <- max(smd_c$VIF)
            theta_c <- sigma(md_c[[1]])
            smd_c <- smd_c[smd_c$term=="year",]
            coefan <- smd_c$coef
            trend <- round(coefan,3)
            ## pourcentage de variation sur la periode
            estimate <- smd_c$Estimate
            
            pasdetemps <- length(unique(d$year))-1
            pourcentage <- round((exp((coefan-1)*pasdetemps)-1)*100,3)
            pval <- smd_c[,5]
            erreuran <- smd_c[,3]
            ## erreur standard
            erreurannee_c <- erreuran*coefan
            vif_c <- smd_c$VIF
            ic_inf_sim <-  round(smd_c$ICinf,3)
            ic_sup_sim <-  round(smd_c$ICsup,3)
            
            ## tab_c_sp table utile pour la realisation des figures
            
            tab_c_sp <- data.frame(Est=trend,
                                   LL=ic_inf_sim, UL=ic_sup_sim,
                                   pourcent=pourcentage,signif=pval<seuilSignif,pval,
                                   vif=vif_c,vif_mean=vif_c_mean,vif_max=vif_c_max)
            
            trendsignif <- tab_c_sp$signif
            pourcent <- pourcentage
            ## surdispersion
            
            ## affectation des tendence EBCC
            catEBCC <- NA
            if(assessIC)  catEBCC <- affectCatEBCC(trend = tab_c_sp$Est,pVal = tab_c_sp$pval,ICinf=as.vector(tab_c_sp$LL),ICsup=as.vector(tab_c_sp$UL)) else catEBCC <- NA
            ## table complete de resultats
            
            vecLib <-  NULL
            if(is.na(vif_c_mean)) {
              catIncert <- "Incertain"
              if(is.na(vif_c_mean)) vecLib <- paste(vecLib,"VIF tendance non calculable")
            } else { # ELSE  if(is.na(vif_c_mean))
              print(paste(vif_c_mean,vif_c_max,theta_f,theta_c))
                          if(is.na(theta_f)){theta_f=theta_c}
                if( vif_c_mean > 2 | vif_c_max > 5 | theta_f
                  < .1 | theta_f > 10 | theta_c < .1 | theta_c > 10 ) {
                catIncert <- "Incertain"
                
                if(vif_c_mean > 2) vecLib <- c(vecLib,"moyenne vif tendance sup à 2")
                if(vif_c_max > 5) vecLib <- c(vecLib,"max vif tendance sup à 5")
                if(theta_f < 0.1) vecLib <- c(vecLib," theta variation inf à 0.1")
                if(theta_c < 0.1) vecLib <- c(vecLib," theta tendance inf à 0.1")
                if(theta_f > 10) vecLib <- c(vecLib," theta variation sup à 10")
                if(theta_c > 10) vecLib <- c(vecLib," theta tendance sup à 10")
              } else {
                catIncert <-"bon"
              }
            } # END ELSE  if(is.na(vif_c_mean))
            raisonIncert <-  paste(vecLib,collapse=" et ")
            
            print("L1574")
            tabTrend_sp <- data.frame(
              id,data=dn,espece=sp,nom_espece = nomSp ,indicateur = "",
              nombre_annees = pasdetemps,premiere_annee = firstY
              ,derniere_annee = lastY
              ,direct=!without_direct,expansion=!without_exp,
              tendance = as.vector(tab_c_sp$Est) 
              ,  IC_inferieur=ic_inf_sim , IC_superieur = ic_sup_sim ,
              pourcentage_variation=as.vector(pourcent),
              erreur_standard = as.vector(round(erreurannee_c,4))
              , p_value = round(pval,3),
              vif = vif_c,vif_mean=vif_c_mean,vif_max=vif_c_max,
              significatif = trendsignif,categorie_tendance_EBCC=catEBCC,
              mediane_occurrence_direct=ifelse(is.null(tabAn_f_sp),NA,median(tabAn_f_sp$occ_direct
                                               ,na.rm=TRUE)) ,
              mediane_occurrence_exp=ifelse(is.null(tabAn_f_sp),NA
                                            ,median(tabAn_f_sp$occ_exp
                                                    ,na.rm=TRUE)) ,
              theta_variation = theta_f,theta_tendance = theta_c,
              valide = catIncert,raison_incertitude = raisonIncert)
          } # END if(class(md_c)[1] != "try-error")
          
        } # END if(method == "glmmTMB")
        dTrend <- rbind(dTrend,tabTrend_sp)
        
      } # END ELSE if(med_occ <= 2)
      
    } # END for(dn in donneesName)
    
    ##   if(assessIC)  listGLMsp <- list(list(glm1,glm1.sim,md_c,md_c.sim)) else  listGLMsp <- list(list(glm1,md_c))
    ##   names(listGLMsp)[[1]] <-sp
    ##   fileSaveGLMsp <- paste(fileSaveGLMs,"_",sp,".Rdata",sep="")
    
    ##   save(listGLMsp,file=fileSaveGLMsp)
    ##   cat("--->",fileSaveGLMsp,"\n")
    ##   flush.console()
    
    if(ecritureStepByStep) {
      write.csv2(dgg,filesavedgg,row.names=FALSE,quote=FALSE)
      cat("--->",filesavedgg,"\n")
      write.csv2(dAn,filesaveAn,row.names=FALSE,quote=FALSE)
      cat("--->",filesaveAn,"\n")
      write.csv2(dTrend,filesaveTrend,row.names=FALSE,quote=FALSE)
      cat("--->",filesaveTrend,"\n")
      
      flush.console()
      
    }
    
    
    print("L1617")
    ## les figures
    if(figure) {
      ## table complete pour la figure en panel par ggplot2
      ## table pour graphe en panel par ggplot2
      ## les figures
      dggSp <- subset(dgg,espece == sp)
      
      using <- melt(using_ExpDirect[espece == sp],id.vars=c("data","espece"),variable.name="courbe",value.name="used",variable.factor=FALSE)
      ## si aucun model alors on garde toutes les courbes de description
      if(any(using$used)) {
        using <- using[,espece := as.character(espece)]
        using <- using[,data:= as.character(data)]
        
        dggSp <- data.table(full_join(dggSp,using))
        dggSp <- dggSp[is.na(used),used := TRUE]
        dggSp <- dggSp[used==TRUE]
      }
      if(!(is.null(dTrend)))
        dTrendSp <- subset(dTrend,espece==sp) else dTrendSp <- data.frame(espece=NULL,panel=NULL)
      theCatIncert <- "incertain"
      if(nrow(dTrendSp)>0)
        theCatIncert <- ifelse(any(dTrendSp$valide == "bon"),"bon","incertain")
      ggplot.espece(dgg=dggSp,dTrend=dTrendSp,id=id,sp=sp,valide=theCatIncert,hline.data="auto",tendanceSurFigure=tendanceSurFigure,seuilOccu=seuilOccu,vpan = vpan)
      
    } # END  if(figure)
    
    
    
    
  } # END for(sp in listSp)
  
  fwrite(dgg,filesavedgg,row.names=FALSE,quote=FALSE,sep=";")
  cat("--->",filesavedgg,"\n")
  if(!is.null(dAn))
  {
    fwrite(dAn,filesaveAn,row.names=FALSE,quote=FALSE,sep=";")
    cat("--->",filesaveAn,"\n")
  }
  if(!is.null(dTrend))
  {
  fwrite(dTrend,filesaveTrend,row.names=FALSE,quote=FALSE,sep=";")
  cat("--->",filesaveTrend,"\n")
  }
  
  flush.console()
  
  end <- Sys.time() ## heure de fin
  diff <- end-start
  diff <- paste(round(diff,1),units(diff))
  print("L1662")
  cat("\n\n=====================================================\n")
  cat("\n  ", format(end, "%d-%m-%Y %HH%M")," -> ",diff,"\n")
  cat("\n=====================================================\n\n")
}


