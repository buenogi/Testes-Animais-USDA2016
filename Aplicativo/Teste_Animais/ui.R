library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
fluidPage(
    theme = shinytheme("journal"),
    # Application title
    titlePanel("Testagem em animais - USDA 2016"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(radioButtons("utilizado",
                                  "Utilizados em pesquisa:",
                                  choices = list("sim","não"),
                                  selected = "sim", 
                                  inline = T),
                     radioButtons("dor",
                                  "Exposição a dor:",
                                  choices = list("sim","não"),
                                  selected = "sim", 
                                  inline = T),
                     radioButtons("droga",
                                  "Anestesia/analgesia:",
                                  choices = list("sim","não"),
                                  selected = "sim", 
                                  inline = T),
                     selectInput("especie",
                                 "Selecione a especie:",
                                 choices = c("C. porcellus" = "cavia_p",
                                             "Outras espécies" = "outras_especies",
                                             "Coelhos" = "coelhos",
                                             "Hamsters" = "hamsters",
                                             "Primatas não humanos" = "primatas_nao_humanos",
                                             "Cães" = "caes",
                                             "Porcos" = "porcos",
                                             "Animais de fazenda" = "animais_de_fazenda",
                                             "Gatos" = "gatos",
                                             "Ovelhas" = "ovelhas"),
                                 multiple = T)
                    
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotly::plotlyOutput("freqPlot"),
        )
    )
)

