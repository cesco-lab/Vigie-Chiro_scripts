library(data.table)

# Set directories
Directory_start = "/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/TempBMRE/tbusscha2_E-x10-5s"
Directory_output = "/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/"
# Directory_start = "E:/chamelink_TEST-x10-5s/Wuustwezel_Sterbos_oostelijke dreef_(51.396165, 4.553054)_Augustus-oktober-2021/augustus_2021"
# Directory_output = "E:/chamelink_TEST-output"

# Read metadata
Metadata_table = fread("/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/TempBMRE/metadata/Thomas_Busschaert_Metadata_table_2022WGS84_created.csv")
# Metadata_table = fread("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL/Cedric_Hamelink_Metadata_table.csv")
print("nb participations dans Metadata :")
print(nrow(Metadata_table))

# Clean empty folders
folders <- list.dirs(Directory_start, recursive = TRUE)
for(folder in folders){ # run it one time to remove empty participation directories
  if(length(dir(folder)) == 0){
    unlink(folder, recursive = TRUE)
  }
}
folders <- list.dirs(Directory_start, recursive = TRUE)
for(folder in folders){ # run it second time to remove empty site directories
  if(length(dir(folder)) == 0){
    unlink(folder, recursive = TRUE)
  }
}

# List sites to rename
list_sites = list.files(Directory_start, recursive = FALSE, include.dirs = TRUE, full.names = TRUE)
list_sites=subset(list_sites,!grepl(".csv",list_sites))
list_sites=subset(list_sites,!grepl(".xls",list_sites))
list_sites=subset(list_sites,!grepl(".txt",list_sites))
print("nb sites dans Directory_start :")
print(length(list_sites))

# Rename files
for (i in 1:length(list_sites)){ # for each site
  print(list_sites[i])
  list_part= list.files(list_sites[i], recursive = FALSE, include.dirs = TRUE, full.names = TRUE)
  DataSite=subset(Metadata_table,Metadata_table$Site==basename(list_sites[i]))
  if(nrow(DataSite)==0){stop("site manquant dans Metadata")}
  for (j in 1:length(list_part)){ # for each participation
    print(list_part[j])
    list_wav1=list.files(list_part[j],pattern=".wav$",full.names=T)
    list_wav2=list.files(list_part[j],pattern=".WAV$",full.names=T)
    list_wav=c(list_wav1,list_wav2)
    list_ta=list.files(list_part[j],pattern=".ta$",full.names=T)
    list_wav_ta=c(list_wav,list_ta) # list wav and ta files
    DataPart=subset(DataSite,DataSite$Participation==basename(list_part[j])) # retrieve participation metadata
    if(nrow(DataPart)==0){stop("participation manquante dans Metadata")}
    if(nrow(DataPart)>1){stop("participation doublon dans Metadata")}
    Anneej=substr(DataPart$StartDate,7,10)
    if(substr(Anneej,1,2)!="20"){stop("annee aberrante")}
    Prefix=paste0(gsub("Vigiechiro - Point Fixe-","Car",DataPart$Carre),"-"
                  ,Anneej,"-Pass1-",DataPart$Point,"-")
    NewDir=paste0(Directory_output,"/",DataPart$idsite) 
    dir.create(NewDir) # create site directory
    NewDir2=paste0(NewDir,"/",DataPart$idparticipation) 
    dir.create(NewDir2) # create participation directory
    NewName=paste0(NewDir,"/",Prefix,basename(list_wav_ta))
    IfExist=file.exists(NewName) # list files that already exist
    NewName=subset(NewName,!IfExist)
    list_wav_ta=subset(list_wav_ta,!IfExist)
    test=file.rename(from=list_wav_ta,to=NewName) # rename + move files (because their name now is a new directory)
    print(table(test))
    if(min(test)==0){stop("renommage incomplet")}
  }
  
}
















