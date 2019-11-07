# ui.R pour TadaridaShinyVigie-Chiro
library(shiny)
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(""),# ("Analyse Tadarida"),

  # Sidebar with controls to select parameters to plot, group, species etc.
  # Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(width = 2,
               selectInput("paramschoix",

                           label = "Choisissez un paramètre -> ordonnées).",
                           choices = params,
                           selected = "frequence_mediane")
               ,
               selectInput("idchoix",
                           "Groupe :",
                           c("Tous",
                             sort(unique(as.character(AlleYoupi5$groupe))))
                           ,selected = "Chauve-souris")
               ,
               selectInput("especechoix",
                           "Espèce :",
                           c("Toutes",
                             sort(unique(as.character(AlleYoupi5$tadarida_taxon)))))
               ,
               sliderInput("conf",
                           label = "Indice de confiance de l'espèce :",
                           min = 0, max = 1, value = c(0.5, 1))
               ,
               sliderInput("frequence_mediane",
                           label = "Frequence mediane",
                           min = 0, max = 250, value = c(0, 120))
               
               
               ,
               selectInput("espececorrige",
                           "Espèce correction:",
                           c("Confirme", especes))
               ,
               actionButton("submit","Valider")
               ,
               downloadButton('downloadData', 'Sauver les corrections')
               # # Pour appVigie-Chiro (sorties Tadarida.csv)
               # sliderInput("confg",
               # label = "Indice de confiance du groupe :",
               # min = 0, max = 10, value = c(0, 10)),
               # sliderInput("nbcris",
               # label = "Nombre de cris dans le contact :",
               # min = 0, max = 10, value = c(0, 10)),
               # sliderInput("buzz",
               # label = "Indice de buzz :",
               # min = 0, max = 10, value = c(0, 10)),
               # sliderInput("social",
               # label = "Indice de cri social :",
               # min = 0, max = 10, value = c(0, 10))
               # ,
               # downloadButton('downloadData', 'Sauvegarder les contacts modifiés')
               # ,
               # verbatimTextOutput("rowno")
  ),

  # Show a tabset that includes a plot and a time slider.
  mainPanel(
    tabsetPanel(
      tabPanel(titre,
               shiny::column(9,
                             ggvisOutput("plot")),
               shiny::column(11, offset = 1,
                             sliderInput("heures",
                                         label = paste("Intervalle depuis: ", mintemps, "  jusqu'à ", maxtemps, sep = ""),
                                         min = 0, max = 100, value = c(0, 100), width = "89%"))
              # ,
               #shiny::column(12,
                #             sliderInput("frequence_mediane",
                 #                     min = 0, max = 130, value = c(0, 130)
               
      ),

      tabPanel("Table", dataTableOutput(outputId="table")),
      tabPanel("Dernier fichier ouvert"
               ,
               tableOutput(outputId="table2")
               ,
               dataTableOutput(outputId="table3")
               # ,
               # wellPanel(
               # textInput('groupecorrige', "nouveau groupe","")
               # ,
               # textInput('espececorrige', "nouvelle espèce","")
               # ,
               # actionButton("submit","Valider")
      )
      # shiny::column(3,
      # selectInput("groupecorrige",
      # "Groupe :",
      # c("Tous",
      # groupes)))
      # ,
      # shiny::column(3, offset = 4,
      # selectInput("espececorrige",
      # "Espèce :",
      # c("Toutes",
      # colnames(seqScores))))
      # ,
      # shiny::column(3, offset = 3,
      # actionButton("update", "Valider la correction"))
      #       ,
      #      tableOutput(outputId="table3")
    )
  ))
)
