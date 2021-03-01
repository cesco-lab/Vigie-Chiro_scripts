#authors: Jean-François Julien, Yves Bas, Simon Benateau and Charlotte Roemer
library(dplyr)
library(ggplot2)
library(ggvis)
library(shiny)
library(data.table)
library(lubridate)
library(clipr)

options(shiny.maxRequestSize=30*1024^2)

# Define server logic required to draw a scatter plot
shinyServer(function(input, output,session) {
  
  observeEvent(input$do, {
      
    #import participation file
    donneesParticipation <- reactive({
      
      ms = 4
      SpeciesList <- fread("SpeciesList.csv", encoding = "Latin-1")
      SpeciesList$color=factor(SpeciesList$Esp)
      groupes=unique(SpeciesList$GroupFR)
      especes=unique(SpeciesList$Esp)
      infile <- input$fileParticipation
      if (is.null(infile)){
        return(NULL)      
      }
      #if(!exists("AlleYoupi5")){
      AlleYoupi5 = fread(infile$datapath)
      #}
      
      ####### Construction de la colonne DateHeure compatible POSIX
      #bidouille pour protocoles routiers/pedestres dans les
      if(substr(AlleYoupi5$`nom du fichier`[1],1,3)=="Cir"){
        FileInfo=tstrsplit(AlleYoupi5$`nom du fichier`,split="-")
        NumTron=as.numeric(gsub("Tron","",FileInfo[[4]]))
        Heure <- substr(AlleYoupi5$`nom du fichier`,nchar(AlleYoupi5$`nom du fichier`)-11+ms,nchar(AlleYoupi5$`nom du fichier`)-8+ms)
        DateHeure=(NumTron-1)*365+as.numeric(Heure)/3
        DateHeure=as.Date.numeric(DateHeure,origin="01-01-1900")
      }else{
        
      DateHeure <- substr(AlleYoupi5$`nom du fichier`,nchar(AlleYoupi5$`nom du fichier`)-22+ms,nchar(AlleYoupi5$`nom du fichier`)-8+ms)
      DateHeure=ymd_hms(DateHeure,tz="Europe/Paris") 
      }
      
      AlleYoupi5$DateHeure=DateHeure
      AlleYoupi5$Date_nuit=as.Date(DateHeure-12*3600)
      
      
      #Creation de variables pour l'app Shiny
      AlleYoupi5$Affiche <- paste(AlleYoupi5$`nom du fichier`, " sp: ", AlleYoupi5$tadarida_taxon, "Confiance: ", as.character(round(AlleYoupi5$tadarida_probabilite,1), sep=""))
      AlleYoupi5$duree_sequence=AlleYoupi5$temps_fin-AlleYoupi5$temps_debut
      test=match(AlleYoupi5$tadarida_taxon,SpeciesList$Esp)
      AlleYoupi5$groupe=SpeciesList$GroupFR[test]
      AlleYoupi5$color=SpeciesList$color[test]
      params <- c("frequence_mediane", "duree_sequence","temps_debut", "temps_fin")
      AlleYoupi5=as.data.frame(AlleYoupi5)
      #if(!exists("AlleYoupi7")){
      AlleYoupi7 <- AlleYoupi5 #tableau avec validations ? sauver
      #}
      AlleYoupi8 <<- AlleYoupi7[0, ] #tableau qui s'affiche dans le dernier onglet de l'appli (validations faites)
      gpnames <-append("Tous", sort(as.character(unique(AlleYoupi5$Groupe))))
      spnames <-append("Toutes", sort(as.character(unique(AlleYoupi5$tadarida_taxon))))
      timespan <- max(DateHeure) - min(DateHeure)
      sliderlabel <- paste("Intervalle depuis: ", min(AlleYoupi5$DateHeure), "  jusqu'Ã  ", max(AlleYoupi5$DateHeure), sep = "")
      mintemps <- min(DateHeure)
      maxtemps <- max(DateHeure)
      fichierslash <- gsub("\\\\", "/", infile$datapath)
      coupe <- unlist(strsplit(fichierslash,"/"))
      titre <- substr(coupe[length(coupe)], 1, nchar(coupe[length(coupe)])-4)
      fichiervu <<- isolate(gsub(".csv","_Vu.csv",input$fileParticipation))
      AlleYoupi5
    })
    
    
    wavdir <- reactive({
      wavdir <- input$wavdirChoice
    })
    
    # test only
    #output$testStr <- renderTable({
    # AlleYoupi5 <- wavdir()
    # head(AlleYoupi5)
    #})
    
    
   # output$paramschoix <- renderUI({
    #  df <- donneesParticipation()
    #  if (is.null(df)) return(NULL)
    #  params <- c("frequence_mediane", "duree_sequence","temps_debut", "temps_fin")
     # selectInput("paramschoix", label = "Choisissez un paramÃ¨tre -> ordonnÃ©es).", choices = params, selected = "frequence_mediane")
   #  })
    
    output$idchoix <- renderUI({
      df <- donneesParticipation()
      if (is.null(df)) return(NULL)
      selectInput("idchoix", #groupe ? afficher
                  "Groupe :",
                  c("Tous",
                    sort(unique(as.character(df$groupe))))
                  ,selected = "Chauve-souris")
    })
    
    output$especechoix <- renderUI({
      df <- donneesParticipation()
      if (is.null(df)) return(NULL)
      selectInput("especechoix", #esp?ce ? afficher
                  "EspÃ¨ce :",
                  c("Toutes",
                    sort(unique(as.character(df$tadarida_taxon)))))
    })
    
    output$heures <- renderUI({
      df <- donneesParticipation()
      if (is.null(df)) return(NULL)
      mintemps <- min(df$DateHeure)
      maxtemps <- max(df$DateHeure)
      sliderInput("heures",
                  label = paste("Intervalle depuis: ", mintemps, "  jusqu'Ã  ", maxtemps, sep = ""),
                  min = 0, max = 100, value = c(0, 100), width = "89%")
    })
    
    
    
    sp <- reactive({
      #if(!exists("AlleYoupi5")){
      AlleYoupi5 <- donneesParticipation()%>%
        droplevels
      #}
      if(!exists("AlleYoupi8")){AlleYoupi8 <- AlleYoupi5[0, ]} #tableau qui s'affiche dans le dernier onglet de l'appli (validations faites)
      
      if (is.null(AlleYoupi5)) return(NULL)
      if (is.null(input$idchoix)) return(NULL)
      timespan <- max(AlleYoupi5$DateHeure) - min(AlleYoupi5$DateHeure)
      
      #parametre <- AlleYoupi5[,input$paramschoix] #selection du param?tre ? afficher en ordonn?e
      #AlleYoupi6 <- cbind(AlleYoupi5,parametre)
      AlleYoupi6 <- cbind(AlleYoupi5,parametre=AlleYoupi5$frequence_mediane)
      mintemps <- min(AlleYoupi6$DateHeure) + timespan*input$heures[1]/100 #d?but axe abscisse d?fini par le sliderinput dans ui.R
      maxtemps <- min(AlleYoupi6$DateHeure) + timespan*input$heures[2]/100 #fin axe abscisse
      toplot <- subset(AlleYoupi6, AlleYoupi6$DateHeure >= mintemps & AlleYoupi6$DateHeure <= maxtemps & AlleYoupi6$tadarida_probabilite >= input$conf[1] &  AlleYoupi6$tadarida_probabilite <= input$conf[2]) #+s?lection sur les indices de confiance
      toplot <- subset(toplot,toplot$frequence_mediane>=input$frequence_mediane[1]) #selection par fr?quence m?diane (pour ?viter d'afficher des fr?quences inutiles)
      toplot <- subset(toplot,toplot$frequence_mediane<=input$frequence_mediane[2])
      
      if (input$idchoix != "Tous")  subset(toplot, toplot$groupe == input$idchoix)
      else {
        if (input$especechoix != "Toutes")
        {subset(toplot, toplot$tadarida_taxon == input$especechoix)
        }else {  toplot }
      }
    })
    
    
    
    
    observe({
      req(sp())
      #if(!exists("AlleYoupi5"))
      #{
      AlleYoupi5 <- donneesParticipation()
      #}
      wavdir <- wavdir()
      submit0 <- 0 #initialisation du fichier s?lectionner sur le graphe par click ?
      
      #browser()
      
      # readr::write_rds(sp(), "sp_shiny.rds")
      # mysp <- readr::read_rds("sp_shiny.rds")
      
      sp() %>%
        ggvis(~DateHeure, ~parametre, key:= ~Affiche) %>%
        
        layer_points(size = ~tadarida_probabilite*20, fill = ~color, stroke = 1, shape = ~color) %>%
        #layer_points(size = ~tadarida_probabilite*2, fill = ~factor(tadarida_taxon), stroke = 1) %>%
        set_options(width = 820, height = 540, padding = padding(5, 90, 40, 120)) %>%
        hide_legend("stroke") %>%
        hide_legend("size") %>%
        add_legend(c("shape","fill"),
                   title = "Especes") %>% #l?gende des formes/groupes
        hide_legend("size") %>%
        add_tooltip(function(data){
          soundexe <- paste(unlist(strsplit(data$Affiche, " ")[1])[1], ".wav", sep="");
          #soundexe <- paste(wavdir, "\\", unlist(strsplit(data$Affiche, " ")[1])[1], ".wav", sep="");
          #ConfigSyrinx[7,1]=paste0("Sound file name=",soundexe);
          #ConfigSyrinx[8,1]=paste0("Sound file title=",basename(soundexe));
          #fwrite(ConfigSyrinx,"temp.dsp");
          #shell.exec("temp.dsp")}, "click") %>%
          #shell.exec(soundexe)}, "click") %>%
          write_clip(soundexe)}, "click") %>%
        
        add_tooltip(function(data){ qui <- which(AlleYoupi5$Affiche == data$Affiche) #affichage d'?tiquette en fonction de la position du curseur
        ; output$table2 <- renderTable(AlleYoupi5[qui, ])
        reactiveValues()
        
        if (input$submit > submit0) { #si on a cliqu? sur "valider"
          AlleYoupi5[qui, 8:9] <<- isolate(c(input$espececorrige,input$probacorrige))
          if(!exists("AlleYoupi8")){AlleYoupi8 <- AlleYoupi5[0, ]} #tableau qui s'affiche dans le dernier onglet de l'appli (validations faites)
          AlleYoupi8 <<- isolate(unique(rbind(AlleYoupi5[qui, ],AlleYoupi8))) #incr?mente les validations dans AlleYoupi8
          #AlleYoupi7 <<- isolate(AlleYoupi5) #tableau avec validations ? sauver
          AlleYoupi7 <<- isolate(unique((rbind(AlleYoupi8,AlleYoupi5))
                                        )) #tableau avec validations ? sauver
          AlleYoupi7<<-unique(as.data.table(AlleYoupi7),by=c("nom du fichier","tadarida_taxon"))
          AlleYoupi7<<-AlleYoupi7[order(AlleYoupi7$`nom du fichier`),]
          submit0 <<- input$submit}
        output$table3 <- renderDataTable({AlleYoupi8 }) #affiche AlleYoupi8 dans le dernier onglet
        output$table4 <- renderDataTable({AlleYoupi7 }) #affiche AlleYoupi8 dans le dernier onglet
        
        #  Sauver imm?diatement cette table modifi?e.
        }
        , "click") %>%
        add_tooltip(function(data){ paste0(data$Affiche)}, "hover") %>% #d?finir l'affichage quand on "survole" des points dans le graphe
        bind_shiny("plot", "plot_ui")
      
    })
    
    # output$table <- renderDataTable({
    #data <- AlleYoupi5
    # if (input$idchoix != "Tous"){
    #    data <- data[data$groupe == input$idchoix,]
    #   }
    #if (input$groupechoix != "Tous"){
    #   data <- data[data$groupe == input$groupechoix,]
    #  }
    #   if (input$especechoix != "Toutes"){
    #  data <- data[data$tadarida_taxon == input$especechoix,]
    #  }
    
    #   data <- data[data$tadarida_probabilite >= input$conf[1] & data$tadarida_probabilite <= input$conf[2],]
    #   addRadioButtons <- paste0('<input type="radio" name="rown" value="', 1:nrow(data), '">')
    #   cbind(Ouvrir=addRadioButtons, data)
    # },
    # options = list(iDisplayLength = 100)
    # )
    #output$rowno <- renderPrint({ rown })
    output$downloadData <- downloadHandler(
      #filename="temp.csv",
      filename=function(){fichiervu},
      content = function(file) {
        fwrite(AlleYoupi7, file,row.names=F,na="",sep=";")
      })
    
    #on.exit(rm(list= ls()))
    #onStop(function() rm(list=c("AlleYoupi7","AlleYoupi8","fichiervu")))
  })
})
