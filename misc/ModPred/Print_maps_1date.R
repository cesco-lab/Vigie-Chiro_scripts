library(data.table)
library(ggplot2)
library(dismo)
library(sf)
library(tidyverse)
library(viridis)
library(gganimate)
library(beepr)

arg <- "./SIG/Limite_administrative/France_dep_L93.shp" # french contour
arg[2] <- "SpeciesList.csv" # species list
arg[3] <- "./www/predVC2108" # repertory with outputs from Predict_act
arg[4] <- "./VigieChiro/ModPred/Maps2108" #repertory for png
#arg[5] <- "C:/Users/croemer01/Documents/GT Eolien/Donnees_parcs/SIG/Mats_service_TOTAL.shp" # wind turbines in France

dir.create(arg[4])

# Load french contour
france <- read_sf(arg[1])
france_f <- france %>% 
  #  filter(as.numeric(INSEE_REG)>6) %>% # use only metropolitan France regions
  # dplyr::summarise() %>%  # merge all region polygons to obtain the contour of France
  st_transform(4326) # reproject to WGS84

# Load french wind turbines
#WT_FR <- read_sf(arg[5])

#Load species list
sp_list <- fread(arg[2])

# Load predictions
list_file <- list.files(arg[3],recursive=FALSE,pattern="*.csv$")
ls2 = paste(paste0(arg[3],"/",list_file, sep=""))
ld <- lapply(ls2, function(x) read_csv(x))
ld <- mapply(cbind, ld, "Species"=tstrsplit(list_file,split="_")[[1]], SIMPLIFY=F) # add column with species name
ld <- mapply(cbind, ld, "Month"=tstrsplit(list_file,split="_")[[4]], SIMPLIFY=F) # add column with month name
ld <- mapply(cbind, ld, "Day"=tstrsplit(list_file,split="_")[[3]], SIMPLIFY=F) # add column with day name

file_bind <- do.call("rbind",ld)

# Back-transform predictions
file_bind$pred=(10^(file_bind$pred)-1)

for (i in 1:length(names(table(file_bind$Species)))) { # For each species
  
  Sp = names(table(file_bind$Species))[i]
  
  print(Sp)
  
  dataa = subset(file_bind, file_bind$Species == Sp)
  
  # Plot info
  full_latin_name <- subset(sp_list, sp_list$Esp==gsub("VC50","",Sp)
  )$'Scientific name'
  my.month.name <- Vectorize(function(n) c("January","February","March","April", "May","June",
                                           "July","August", "September", "October", "November",
                                           "December")[n])
  
  # Scale color gradient based on periods of high activity and prediction > 0.1
  #PourMaxScale=subset(dataa, (as.numeric(dataa$Month)<11 & as.numeric(dataa$Month)>8)) 
  PourMaxScale=dataa 
  
  MaxScale=quantile(subset(PourMaxScale$pred,PourMaxScale$pred>0.1),0.98)
  if(is.na(MaxScale)){MaxScale=0.1}
  ScaleLimit=c(0, MaxScale)
  
  # Plot predictions for each month ####
  for (j in 1:length(names(table(dataa$Month)))){
    for (k in 1:length(names(table(dataa$Day)))){
      png(filename=paste0(arg[4], "/", Sp,"_", names(table(dataa$Month))[j],"_",names(table(dataa$Day))[k],".png"), width = 3000, height = 2500, res=300)
      
      dataa_Month=subset(dataa, dataa$Month==names(table(dataa$Month))[j] & dataa$Day==names(table(dataa$Day))[k])
      Month_name <- my.month.name(as.numeric(names(table(dataa$Month))[j]))
      Day_name <- as.numeric(names(table(dataa$Day)[k]))
      
      print(Month_name)
      
      plot <- ggplot()+
        
        geom_point(data = dataa_Month, 
                   mapping = aes(x=Group.1, y=Group.2, col=pred), size=0.1) +
        
        scale_color_viridis(name = "Number of \nbat passes/night",
                            limits = ScaleLimit, 
                            oob = scales::squish,
                            option = "A") +
        
        geom_sf(data= france_f, size=0.1, fill=alpha("lightgrey", 0), colour = "black") +
        
        guides(size = FALSE) +
        
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              plot.subtitle = element_text(size=12)) +
        
        labs(title = paste0(full_latin_name, "\n", Day_name, " of ", Month_name),
             subtitle = paste("Number of bat passes per night : ",
                              "Mean = ",
                              round(mean(as.data.frame(dataa_Month)$pred),1),
                              ", Max = ",
                              round(max(as.data.frame(dataa_Month)$pred),1),
                              sep="")) +
        ylab("") +
        xlab("") 
      
      print(plot)
      dev.off()
    }
  }
}  


beep(2)
