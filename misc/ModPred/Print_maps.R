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
file_bind$pred=10^(file_bind$pred)

for (i in 1:length(names(table(file_bind$Species)))) { # For each species
  
  Sp = names(table(file_bind$Species))[i]
  Sp=gsub("VC50","",Sp)
  
  print(Sp)
  
  dataa = subset(file_bind, file_bind$Species == Sp)
  
  # Plot info
  full_latin_name <- subset(sp_list, sp_list$Esp==Sp)$'Scientific name'
  my.month.name <- Vectorize(function(n) c("January","February","March","April", "May","June",
                                           "July","August", "September", "October", "November",
                                           "December")[n])
  
  # Scale color gradient based on periods of high activity and prediction > 0.1
  PourMaxScale=subset(dataa, (as.numeric(dataa$Month)<11 & as.numeric(dataa$Month)>8)) 
  MaxScale=quantile(subset(PourMaxScale$pred,PourMaxScale$pred>0.1),0.98)
  if(is.na(MaxScale)){MaxScale=0.1}
  ScaleLimit=c(0, MaxScale)
  
  # Plot predictions for each month ####
  for (j in 1:length(names(table(dataa$Month)))){
    for (k in 1:2){
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
  
  # # Plot predictions as a gif for each month ####
  # 
  # dataa$Month_char <- my.month.name(as.numeric(dataa$Month))
  # 
  # plotGIF <- ggplot()+
  #   
  #   geom_point(data = dataa, 
  #              mapping = aes(x=Group.1, y=Group.2, col=pred), size=0.1) +
  #   
  #   scale_color_viridis(name = "Number of \nbat passes/night",
  #                       limits = ScaleLimit, 
  #                       oob = scales::squish,
  #                       option = "A") +
  #   
  #   geom_sf(data= france_f, size=1, fill=alpha("lightgrey", 0), colour = "black") +
  #   
  #   guides(size = FALSE) +
  #   
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(),
  #         axis.line = element_line(colour = "black"),
  #         plot.subtitle = element_text(size=12)) +
  #   
  #   labs(title = paste0(full_latin_name, "\n", "{previous_state}"),
  #        subtitle = paste("Number of bat passes per night : ",
  #                         "Mean = ",
  #                         round(mean(as.data.frame(dataa)$pred),1),
  #                         ", Max = ",
  #                         round(max(as.data.frame(dataa)$pred),1),
  #                         sep="")) +
  #   ylab("") +
  #   xlab("") +
  #   transition_states(
  #     states = Month_char, 
  #     transition_length = 3,
  #     state_length = 3)
  # 
  # # print(plotGIF)
  # 
  # animate(plotGIF)
  # anim_save(paste0(arg[4], "/", Sp,"_", "GIF",".gif"))
  # 
  
  # Plot difference in prediction value ####
  
  dataa_january=subset(dataa, dataa$Month=="1" & dataa$Day=="15")
  dataa_april=subset(dataa, dataa$Month=="4" & dataa$Day=="15")
  dataa_july=subset(dataa, dataa$Month=="7" & dataa$Day=="15")
  dataa_sept=subset(dataa, dataa$Month=="9" & dataa$Day=="15")
  
  dataa_diff=data.frame(Group.1=dataa_april$Group.1, Group.2=dataa_april$Group.2,
                        pref_diff_january_july=dataa_july$pred-dataa_january$pred,
                        pred_diff_april_july=dataa_july$pred-dataa_april$pred,
                        pred_diff_july_sept=dataa_sept$pred-dataa_july$pred)
  
  # Plot predictions for the difference between April and July
  png(filename=paste0(arg[4], "/", Sp,"_", "Diff_April_July",".png"), width = 3000, height = 2500, res=300)
  
  mid <- mean(dataa_diff$pred_diff_april_july)
  
  plot <- ggplot()+
    
    geom_point(data = dataa_diff, 
               mapping = aes(x=Group.1, y=Group.2, col=pred_diff_april_july)) +
    
    scale_color_gradientn(colors = c("darkred", "red", "white", "blue", "darkblue"),
                          name = "Difference in \nnumber of \nbat passes/night",
                          values = scales::rescale(c(min(dataa_diff$pred_diff_april_july),mid-2,mid,mid+2, max(dataa_diff$pred_diff_april_july)))
    ) +
    
    geom_sf(data= france_f, size=0.1, fill=alpha("lightgrey", 0), colour = "black") +
    
    guides(size = FALSE, alpha = FALSE) +
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.subtitle = element_text(size=12)) +
    
    labs(title = paste0(full_latin_name, "\n", "Difference between April and July"),
         subtitle = "red = left from, blue = arrived to") +
    ylab("") +
    xlab("") 
  
  print(plot)
  dev.off()
  
  # Plot predictions for the difference between July and September
  png(filename=paste0(arg[4], "/", Sp,"_", "Diff_July_Sept",".png"), width = 3000, height = 2500, res=300)
  
  mid <- mean(dataa_diff$pred_diff_july_sept)
  
  plot <- ggplot()+
    
    geom_point(data = dataa_diff, 
               mapping = aes(x=Group.1, y=Group.2, col=pred_diff_july_sept)) +
    
    scale_color_gradientn(colors = c("darkred", "red", "white", "blue", "darkblue"),
                          name = "Difference in \nnumber of \nbat passes/night",
                          values = scales::rescale(c(min(dataa_diff$pred_diff_july_sept),mid-2,mid,mid+2, max(dataa_diff$pred_diff_july_sept)))
    ) +
    
    geom_sf(data= france_f, size=0.1, fill=alpha("lightgrey", 0), colour = "black") +
    
    #    geom_sf(data= WT_FR, colour = "black") +
    
    guides(size = FALSE) +
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.subtitle = element_text(size=12)) +
    
    labs(title = paste0(full_latin_name, "\n", "Difference between July and September"),
         subtitle = "red = left from, blue = arrived to") +
    ylab("") +
    xlab("") 
  
  print(plot)
  dev.off()
  
  # Plot predictions for the difference between January and July
  png(filename=paste0(arg[4], "/", Sp,"_", "Diff_January_July",".png"), width = 3000, height = 2500, res=300)
  
  mid <- mean(dataa_diff$pref_diff_january_july)
  
  plot <- ggplot()+
    
    geom_point(data = dataa_diff, 
               mapping = aes(x=Group.1, y=Group.2, col=pref_diff_january_july)) +
    
    scale_color_gradientn(colors = c("darkred", "red", "white", "blue", "darkblue"),
                          name = "Difference in \nnumber of \nbat passes/night",
                          values = scales::rescale(c(min(dataa_diff$pref_diff_january_july),mid-2,mid,mid+2, max(dataa_diff$pref_diff_january_july)))
    ) +
    
    geom_sf(data= france_f, size=0.1, fill=alpha("lightgrey", 0), colour = "black") +
    
    guides(size = FALSE, alpha = FALSE) +
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.subtitle = element_text(size=12)) +
    
    labs(title = paste0(full_latin_name, "\n", "Difference between January and July"),
         subtitle = "red = left from, blue = arrived to") +
    ylab("") +
    xlab("") 
  
  print(plot)
  dev.off()
  
  
  
  
}


beep(2)
