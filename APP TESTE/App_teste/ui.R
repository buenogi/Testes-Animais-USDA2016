library(shiny)
library(shinythemes)
library(leaflet)

navbarPage(title = "Animal test insights",
           # Painel 1 ---------------
           tabPanel(title = "Home", 
                    "content 1"),
           # Painel 2 -----------------------
           tabPanel(title = "Espécies ",
                    fluidPage(
                      theme = shinytheme("journal"),
                      titlePanel("Testagem em animais - USDA 2016"),
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("denominador",
                                       label = "Finalidade: ",
                                       choices = c("Descritiva",
                                                   "Comparativa"),
                                       inline = T,
                                       selected = "Descritiva"),
                          radioButtons("utilizado",
                                       "Utilizados em pesquisa:",
                                       choices = list("sim","nao", "todos"),
                                       selected = "sim", 
                                       inline = T),
                          conditionalPanel(
                            condition = "input.utilizado == 'sim'",
                            radioButtons("dor",
                                         "Exposição a dor:",
                                         choices = list("sim","nao", "todos"),
                                         selected = "sim", 
                                         inline = TRUE)),
                          conditionalPanel(
                            condition = "input.utilizado == 'sim'& input.dor == 'sim'",
                            radioButtons("droga",
                                         "Anestesia/analgesia:",
                                         choices = list("sim","nao", "todos"),
                                         selected = "sim", 
                                         inline = T)),
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
                                      multiple = T,
                                      selectize = T)
                        ),
                        mainPanel(tabsetPanel(type = "tabs",
                                              tabPanel("Gráfico", plotly::plotlyOutput("freqPlot")),
                                              tabPanel("Sumário", tableOutput("resumo")),
                                              tabPanel("Tabela", tableOutput("table"))
                        )
                        )
                      ))),
           # Painel 3 ---------------
           tabPanel(title = "Estados",
                    fluidPage(
                      theme = shinytheme("journal"),
                      titlePanel("Estados"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("n_animais",
                                      label = "Nº de animais",
                                      min = 0,
                                      max = 40000,
                                      step = 10,
                                      dragRange = T, 
                                      value = c(0,40000))),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                               tabPanel("Ranking", plotOutput("PlotEstados")),
                                                tabPanel("Mapa", plotly::plotlyOutput("PlotEstadosMapa"))
                          #                     tabPanel("Sumário", tableOutput("summary")),
                          #                     tabPanel("Tabela", tableOutput("table")
                          )
                        )
                        )
                      )
                    ),
           # Painel 4 --------------------
           tabPanel(title = "Sobre",
                    mainPanel(
                      # leafletOutput("map"),
                              highchartOutput('mapa',height = "500px"))
                    )
)


