library(shiny)
# Define server logic required to draw a scatter plot
shinyServer(function(input, output) {
  sp <- reactive({
    parametre <- AlleYoupi5[,input$paramschoix]
    AlleYoupi6 <- cbind(AlleYoupi5,parametre)
    mintemps <- min(AlleYoupi6$DateHeure) + timespan*input$heures[1]/100
    maxtemps <- min(AlleYoupi6$DateHeure) + timespan*input$heures[2]/100
    toplot <- subset(AlleYoupi6, AlleYoupi6$DateHeure >= mintemps & AlleYoupi6$DateHeure <= maxtemps & AlleYoupi6$SuccessProb >= input$conf[1] &  AlleYoupi6$SuccessProb <= input$conf[2])
    if (input$idchoix != "Tous")  subset(toplot, toplot$Id == input$idchoix)
    else {
        if (input$especechoix != "Toutes")
        {subset(toplot, toplot$SpMaxF2 == input$especechoix)
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

   layer_points(size = ~SuccessProb*20, fill = ~factor(SpMaxF2), stroke = 1, shape = ~factor(SpMaxF2)) %>%
    #layer_points(size = ~SuccessProb*2, fill = ~factor(SpMaxF2), stroke = 1) %>%
    set_options(width = 820, height = 540, padding = padding(5, 90, 40, 120)) %>%
    hide_legend("stroke") %>%
    hide_legend("size") %>%
    add_legend("shape", orient = "left") %>%
    add_legend("fill") %>%
    hide_legend("size") %>%
    add_tooltip(function(data){ soundexe <- paste(wavdir, "\\", unlist(strsplit(data$Affiche, ".wav")[1])[1], ".wav", sep="");
    shell.exec(soundexe)}, "click") %>%
    add_tooltip(function(data){ qui <- which(AlleYoupi5$Affiche == data$Affiche)
    ; output$table2 <- renderTable(AlleYoupi5[qui, ])
    reactiveValues()

    if (input$submit > submit0) {
      AlleYoupi7[qui, 1:2] <<- isolate(c(input$groupecorrige, input$espececorrige))
      AlleYoupi8 <<- isolate(unique(rbind(AlleYoupi8, AlleYoupi7[qui, ])))
      submit0 <<- input$submit}
    output$table3 <- renderDataTable({AlleYoupi8 })
    #  Sauver imm?diatement cette table modifi?e.
    }
    , "click") %>%
    add_tooltip(function(data){ paste0(data$Affiche)}, "hover") %>%
    bind_shiny("plot", "plot_ui")

})
  
  output$table <- renderDataTable({
    data <- AlleYoupi5
    if (input$idchoix != "Tous"){
      data <- data[data$Id == input$idchoix,]
    }
    if (input$groupechoix != "Tous"){
      data <- data[data$Groupe == input$groupechoix,]
    }
    if (input$especechoix != "Toutes"){
      data <- data[data$SpMaxF2 == input$especechoix,]
    }

    data <- data[data$SuccessProb >= input$conf[1] & data$SuccessProb <= input$conf[2],]
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
