library(data.table)
library(rinat)
library(rgbif)
op <- options(digits.secs = 3)

Abonnements=c("anasacuta","benoitnabholz","crebassa","curcu34","danielbizet","ddelon","didierbas"
              ,"elodie8162","flomatutini","greg9498","guillaume_papuga","inuksuk_34790","jan_perret"
              ,"jeremiefevrier","jfj","lenymercier","philippe-rabaute","philippe_geniez"
              ,"pierrehenrifabre","pierrettenyssen","pops34","raphaailes","simben","tdisca"
              ,"thomas_cuypers1"
)
#Inat=list.files("Download"C:,pattern="observations-",full.names=T)
Ranks=c("SUBSPECIES","SPECIES","GENUS","FAMILY","ORDER","CLASS","PHYLUM","KINGDOM")
Origin=c(43.82621972271223, 3.769683525667756)


options(scipen = 999)

MyData=get_inat_obs_user("yvesbas",100000)

for (z in 1:15){
InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
  
Type=sample(c(1:3),1) #myself, near and all



# FInat=file.info(Inat)
# sel=which.max(FInat$ctime)
# DataInat=fread(unzip(Inat[sel]))
# LInat=unique(DataInat$scientific_name)


InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
NumSel=sample(nrow(MyData),1)
SelSp=MyData$scientific_name[NumSel]
SelId=MyData$taxon_id[NumSel]
print(SelSp)
print(SelId)

# test=name_backbone(name=SelSp)
# test$kingdom
# test$phylum
test=name_usage(name=SelSp)
if("nubKey" %in% names(test)){
RightOne=subset(test$data,test$data$key==test$data$nubKey)
}else{
  RightOne=test$data
}
if(RightOne$synonym[1]){
  testR=grepl(RightOne$rank[1],names(RightOne),ignore.case = T)
if(sum(testR)==0){stop("rank missing 50")}
  NumR=which.max(testR)
  RightOne$canonicalName=RightOne[1,NumR]
  }
#RightOne=subset(RightOne,RightOne$synonym==F)
#RightOne=subset(test$data,test$data$synonym==F)

#taxon shift

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
TaxonShift=sample(c(0,1),1)

if(TaxonShift){
  #parents or children
  
  InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
  set.seed(InitialSeed)
  Parents=sample(c(0,1),1)
  if(Parents){
    
    InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
    set.seed(InitialSeed)
    NumShifts=sample(c(0,1),1)
    while(NumShifts==1){
      TaxonShift=TaxonShift+NumShifts
      InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
      set.seed(InitialSeed)
      NumShifts=sample(c(0,1),1)
    }
    RankO=match(RightOne$rank[1],Ranks)
    if(is.na(RankO)){stop("rank missing")}
    RankSel=min(length(Ranks),RankO+TaxonShift)
    ColSel=which.max(grepl(Ranks[RankSel],names(test$data),ignore.case=T))  
    TaxSel=as.character(RightOne[1,ColSel])
    TaxInat=try(get_inat_obs(taxon_name=TaxSel,meta=T,maxresults = 1))
    if(class(TaxInat)=="try-error"){
      TaxInat=try(get_inat_obs(taxon_name=TaxSel,meta=T,maxresults = 1
                               ,year=year(Sys.Date())))
      if(class(TaxInat)=="try-error"){
        TaxInat=try(get_inat_obs(taxon_name=TaxSel,meta=T,maxresults = 1
                                 ,year=year(Sys.Date()),month=1,day=1))
      }
    }
    TaxId=TaxInat$meta$found
    
    
  }else{ #children
    
    InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
    set.seed(InitialSeed)
    NumShifts=sample(c(0,1),1)
    while(NumShifts==1){
      TaxonShift=TaxonShift+NumShifts
      
      InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
      set.seed(InitialSeed)
      NumShifts=sample(c(0,1),1)
    }
    RankO=match(RightOne$rank[1],Ranks)
    if(is.na(RankO)){stop("rank missing")}
    RankSel=max(1,RankO-TaxonShift)
    Childs=name_usage(RightOne$key,data="children")
    ChildNames=Childs$data$canonicalName
    
    InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
    set.seed(InitialSeed)
    TaxSel=Childs$data[sample.int(nrow(Childs$data),1),]
    print(TaxSel$canonicalName)
    TaxInat=try(get_inat_obs(taxon_name=TaxSel$canonicalName,meta=T,maxresults = 1))
    if(class(TaxInat)=="try-error"){
      TaxInat=try(get_inat_obs(taxon_name=TaxSel$canonicalName,meta=T,maxresults = 1
                  ,year=year(Sys.Date())))
      if(class(TaxInat)=="try-error"){
        TaxInat=try(get_inat_obs(taxon_name=TaxSel$canonicalName,meta=T,maxresults = 1
                                 ,year=year(Sys.Date()),month=1,day=1))
      }
    }
    TaxId=TaxInat$meta$found
  }
}else{
  TaxId=SelId
  TaxSel=RightOne
}



#geographical filter

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
ShiftOrNot=sample(c(0,1),1)
if(ShiftOrNot){
  
  InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
  set.seed(InitialSeed)
GeoShift=sample.int(1e10,1)
 GeoShiftInDegrees=((GeoShift/1e10)^5)*50
 
 InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
 set.seed(InitialSeed)
 Angle=sample(c(1:1000),1)/1000*2*pi
 NewLatitude=Origin[1]+sin(Angle)*GeoShiftInDegrees
 NewLongitude=Origin[2]+cos(Angle)*GeoShiftInDegrees*2
 #NOrigin=c(NewLatitude,NewLongitude)
 
 }else{
   NewLatitude=Origin[1]
   NewLongitude=Origin[2]
    
}
# LatMax=min(90,Origin[1]+WidthInDegrees)
# LatMin=max(-90,Origin[1]-WidthInDegrees)
# LongMax=min(180,Origin[2]+WidthInDegrees*2)
# LongMin=max(-180,Origin[2]-WidthInDegrees*2)

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
NumShifts=sample(c(0,1),1)
RadiusShift=1
while(NumShifts==1){
  RadiusShift=RadiusShift+NumShifts
  
  InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
  set.seed(InitialSeed)
  NumShifts=sample(c(0,1),1)
}

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
Radius=sample(c(1:9),1)*10^(RadiusShift)
Radius=as.character(Radius)

# DataSel=try(get_inat_obs(taxon_name=TaxSel,geo=T
#                      ,bounds=c(LatMin,LongMin,LatMax,LongMax),maxresults=10000))
# DataSeli=data.frame()
# if(class(DataSel)=="try-error"){
#   for (y in 2001:year(Sys.Date())){
#     DataSely=get_inat_obs(taxon_name=TaxSel,geo=T
#                               ,bounds=c(LatMin,LongMin,LatMax,LongMax),year=y,maxresults=10000)
#     if(nrow(DataSely)==10000){stop("missing data")}
#     print(y)
#   }
# }


# NumShifts=sample(c(0,1),1)
# ConfShift=0
# while(NumShifts==1){
#   ConfShift=ConfShift+NumShifts
#   NumShifts=sample(c(0,1),1)
# }

#RG or not

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
RG=sample(c(0,1),1)
Verif=ifelse(RG,"needs_id","any")


if(Type==1){
  User="yvesbas"
  
}else{
  if(Type==2){
    #stop("coder autres")
    InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
    set.seed(InitialSeed)
    User=sample(Abonnements,1)
    
  }else{
    User="any"
  }
}
print(User)
URL=paste0("https://www.inaturalist.org/observations?lat=",NewLatitude,"&lng=",NewLongitude
,"&locale=fr&place_id=any&radius=",Radius,"&subview=map&taxon_id=",TaxId,ifelse(User=="any",""
                                                                                ,paste0("&user_id="
                                                                                        ,User))
,"&verifiable=any&quality_grade=",Verif)
URL

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
NumShifts=sample(c(0,1),1)
Shift=0
while(NumShifts==1){
  Shift=Shift+NumShifts
  
  # InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
  # set.seed(InitialSeed)
  NumShifts=sample(c(0,1),1)
}

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
Num=sample(c(1:20),1)+Shift*20


InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
TimeShift=sample(c(0,1),1)

InitialSeed=as.numeric(substr(Sys.time(),21,23))+as.numeric(substr(Sys.time(),18,19))*1000+(as.numeric(substr(Sys.time(),15,16)))*60000
set.seed(InitialSeed)
TimeShift2=sample.int(1e6,1)

print(URL)
print(Num)

TimeShift*TimeShift2/1e6*119

}
