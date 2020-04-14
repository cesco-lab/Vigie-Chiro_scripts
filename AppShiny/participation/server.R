library(shiny)
# Define server logic required to draw a scatter plot
shinyServer(function(input, output) {
  sp <- reactive({
    parametre <- AlleYoupi5[,input$paramschoix] #selection du paramètre à afficher en ordonnée
    AlleYoupi6 <- cbind(AlleYoupi5,parametre) 
    mintemps <- min(AlleYoupi6$DateHeure) + timespan*input$heures[1]/100 #début axe abscisse défini par le sliderinput dans ui.R
    maxtemps <- min(AlleYoupi6$DateHeure) + timespan*input$heures[2]/100 #fin axe abscisse
    toplot <- subset(AlleYoupi6, AlleYoupi6$DateHeure >= mintemps & AlleYoupi6$DateHeure <= maxtemps & AlleYoupi6$tadarida_probabilite >= input$conf[1] &  AlleYoupi6$tadarida_probabilite <= input$conf[2]) #+sélection sur les indices de confiance
    toplot <- subset(toplot,toplot$frequence_mediane>=input$frequence_mediane[1]) #selection par fréquence médiane (pour éviter d'afficher des fréquences inutiles)
    toplot <- subset(toplot,toplot$frequence_mediane<=input$frequence_mediane[2])
    
    if (input$idchoix != "Tous")  subset(toplot, toplot$groupe == input$idchoix)
    else {
        if (input$especechoix != "Toutes")
        {subset(toplot, toplot$tadarida_taxon == input$especechoix)
          }else {  toplot }
  }
    })
  # Expression that generates a scatter plot. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  # A simple visualisation. In shiny apps, need to register observers
  # and tell shiny where to put the controls
observe({
  req(sp())
  
  #browser()
  
  # readr::write_rds(sp(), "sp_shiny.rds")
  # mysp <- readr::read_rds("sp_shiny.rds")
  
  sp() %>%
    ggvis(~DateHeure, ~parametre, key:= ~Affiche) %>%

   layer_points(size = ~tadarida_probabilite*20, fill = ~factor(tadarida_taxon), stroke = 1, shape = ~factor(tadarida_taxon)) %>%
    #layer_points(size = ~tadarida_probabilite*2, fill = ~factor(tadarida_taxon), stroke = 1) %>%
    set_options(width = 820, height = 540, padding = padding(5, 90, 40, 120)) %>%
    hide_legend("stroke") %>%
    hide_legend("size") %>%
    add_legend("shape", orient = "left") %>% #légende des formes/groupes
    add_legend("fill") %>% #légende des couleurs/espèces
    hide_legend("size") %>% 
    add_tooltip(function(data){ soundexe <- paste(wavdir, "\\", unlist(strsplit(data$Affiche, " ")[1])[1], ".wav", sep="");
    #ConfigSyrinx[7,1]=paste0("Sound file name=",soundexe);
    #ConfigSyrinx[8,1]=paste0("Sound file title=",basename(soundexe));
    #fwrite(ConfigSyrinx,"temp.dsp");
    #shell.exec("temp.dsp")}, "click") %>%
    shell.exec(soundexe)}, "click") %>%
    add_tooltip(function(data){ qui <- which(AlleYoupi5$Affiche == data$Affiche) #affichage d'étiquette en fonction de la position du curseur
    ; output$table2 <- renderTable(AlleYoupi5[qui, ])
    reactiveValues()

    if (input$submit > submit0) { #si on a cliqué sur "valider"
      AlleYoupi7[qui, 1:2] <<- isolate(c(input$groupecorrige, input$espececorrige))
      AlleYoupi8 <<- isolate(unique(rbind(AlleYoupi8, AlleYoupi7[qui, ]))) #incrémente les validations dans AlleYoupi8
      submit0 <<- input$submit}
    output$table3 <- renderDataTable({AlleYoupi8 }) #affiche AlleYoupi8 dans le dernier onglet
    #  Sauver imm?diatement cette table modifi?e.
    }
    , "click") %>%
    add_tooltip(function(data){ paste0(data$Affiche)}, "hover") %>% #définir l'affichage quand on "survole" des points dans le graphe
    bind_shiny("plot", "plot_ui")

})
  
  output$table <- renderDataTable({
    data <- AlleYoupi5
    if (input$idchoix != "Tous"){
      data <- data[data$groupe == input$idchoix,]
    }
    if (input$groupechoix != "Tous"){
      data <- data[data$groupe == input$groupechoix,]
    }
    if (input$especechoix != "Toutes"){
      data <- data[data$tadarida_taxon == input$especechoix,]
    }

    data <- data[data$tadarida_probabilite >= input$conf[1] & data$tadarida_probabilite <= input$conf[2],]
    addRadioButtons <- paste0('<input type="radio" name="rown" value="', 1:nrow(data), '">')
    cbind(Ouvrir=addRadioButtons, data)
  },
  options = list(iDisplayLength = 100)
  )
  output$rowno <- renderPrint({ rown })
  output$downloadData <- downloadHandler(
    filename = function() {fichiervu},
    content = function(file) {
      write.csv(AlleYoupi7, file)})
  #   }
  # )
})
