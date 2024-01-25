library(data.table)

DirAll="C:/Users/yvesb/Documents/VigieChiro/gbifData/Raw"
OutputName="C:/Users/yvesb/Documents/VigieChiro/gbifData/Raw_hours.csv"
SpeciesAll=fread("SpeciesAll.csv",h=T,sep=";")

FAll=list.files(DirAll,full.names=T)

All_list=list()
for (i in 1:length(FAll)){
  All_list[[i]]=fread(FAll[i])
}

DataPol=rbindlist(All_list)

Hy=hour(DataPol$eventDate)#+1+HeureEte
DataPol=subset(DataPol,!is.na(D))
My=minute(Datay$eventDate)
Sy=second(Datay$eventDate)
HyReal=subset(Hy,My!=0|Sy!=0)
DatayH=subset(Datay,My!=0|Sy!=0)

ListSpNew=unique(DataPol$species)
Heure=vector()
for (y in 1:length(ListSpNew)){
  
  Datay=subset(DataPol,DataPol$species==ListSpNew[y])
  
  if(length(HyReal)==0){
    Heure=c(Heure,"H")
  }
  if(length(HyReal)==1){
    Heurey=paste0(HyReal-2,"H",HyReal+3)
    Heure=c(Heure,Heurey)
  }
  # if(length(HyReal)==2){
  #   stop("code 2A")
  #   Heurey=paste0(HyReal-2,"H",HyReal+2)
  #   Heure=c(Heure,Heurey)
  # }
  # 
  if(length(HyReal)>1){
    #stop("code")
    #Monthy=month(DatayH$eventDate)
    # if(length(HyReal)==2){
       HyRestricted=HyReal
    # }else{
    #   #HyRestricted=subset(HyReal,Monthy==month(Sys.Date()))
    #   HyRestricted=HyReal
    #   if(length(HyRestricted)<3){
    #     HyRestricted=HyReal
    #   }
    # }
    # if(length(HyRestricted)>1){
       TabH=as.data.frame(table(HyRestricted))
       TabH=TabH[order(TabH$Freq,decreasing=T),]
       TabH$CS=cumsum(TabH$Freq)
       SelMax=min(which(TabH$CS>max(TabH$CS)*0.67))
       HourSel=as.numeric(as.character(TabH$HyRestricted[1:SelMax]))
       HourSel2=ifelse(HourSel<12,HourSel+24
                       ,HourSel)
       Start1=min(HourSel)
       End1=max(HourSel)
       Start2=min(HourSel2)
       End2=max(HourSel2)
       Range1=End1-Start1
       Range2=End2-Start2
       if(Range1<Range2){
         Heurey=paste0(Start1-(Start1>23)*24,"H",End1-(End1>22)*24+1)
       }else{
         Heurey=paste0(Start2-(Start2>23)*24,"H",End2-(End2>22)*24+1)
       }
       Heure=c(Heure,Heurey)  
       
      #  Start1=floor(quantile(HyRestricted,0.125))
      # End1=ceiling(quantile(HyRestricted,0.875))
      # HyNight=HyRestricted+(HyRestricted<=12)*24
      # Start2=floor(quantile(HyNight,0.125))
      # End2=ceiling(quantile(HyNight,0.875))
      # if((End1-Start1)<(End2-Start2)){
      #   if(length(HyRestricted)==2){
      #     Start1=Start1-1
      #     End1=End1+1
      #   }
      #   Heurey=paste0(Start1,"H",End1+1)
      #   Heure=c(Heure,Heurey)
      # }else{
      #   if(length(HyRestricted)==2){
      #     Start2=Start2-1
      #     End2=End2+1
      #   }
      #   Heurey=paste0(Start2-(Start2>23)*24,"H",End2-(End2>22)*24+1)
      #   Heure=c(Heure,Heurey)
      #   
      #}
    # }else{
    #   stop("code 0B")
    # }
    # 
  }
  if(y%%300==1){print(paste(y,length(ListSpNew),Sys.time(),ListSpNew[y],Heure[length(Heure)]))}      
}



SpHeure=data.frame(ListSpNew,Heure)

fwrite(SpHeure,"SpHeure.csv",sep=";")

test=subset(Heure,substr(ListSpNew,1,5)=="Idaea")
