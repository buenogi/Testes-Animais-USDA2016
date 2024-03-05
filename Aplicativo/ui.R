library(shiny)
library(shinythemes)
library(bslib)
library(leaflet)


navbarPage( bg = "#052935",
            # tags$head(
            #   tags$style(type="text/css", "body {fixed-top: 70px;}")
              
  title = "Testagem em animais - USDA 2016",
           # Painel 1 ---------------
           tabPanel(title = "Home",
                    fluidPage(
                    theme = bs_theme(version = 3, bootswatch = 'journal',
                                     bg = "white",
                                     fg = "#052935",
                                     primary = "#e64a19",
                                     font_scale = 1.5,
                                     base_font = font_google("Poppins", local = TRUE)
                                     ),
                    # theme = shinytheme("journal"),
                      column(1,""),
                      column(10,
                    imageOutput("imagemprincipal"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    hr(),
                    h2(strong("Descrição do projeto")),
                    p(style="text-align: justify; font-size = 30px",
                      "Anualmente, desde 1971, o USDA (Departamento de Agricultura 
                    dos Estados Unidos) coleta as informações de animais 
                    empregados em diferentes instâncias de experimentação. 
                    Esta prática vai de encontro a Lei de Bem-estar Animal 
                    (Animal Welfare Act).
                    Os estudos são caracterizados em quatro classes de 
                    acordo com a exposição a dor e ao estresse, conforme indicado abaixo:"),
                    h6("Categoria B - Animais foram criados mas não utilizados;"),
                    h6("Categoria C - Animais foram utilizados mas não houve exposição a dor;"),
                    h6("Categoria D - Animais foram utilizados e houve exposição a dor com terapia;"),
                    h6("Categoria E - Animais foram utilizados e houve exposição a dor sem terapia."),
                    hr(),
                    div(h5("Composição geral da utilização de animais para experimentação com base no anuário de 2016 do USDA"), align = "center"),
                    div(plotly::plotlyOutput("composicaoGeral", width = "800px", height = "800px"), align = "center"),
                    p(style="text-align: justify; font-size = 30px",
                      "Este aplicativo tem como objetivo descrever os dados
                    reportados ao USDA a respeito da utilização de animais em 
                    pesquisa nos EUA em 2016. Na barra de navegação é possível
                    encontrar abas para avaliação das frequencias de utilização 
                    dos animais por espécie em cada categoria de estudo e
                    ranqueamento dos estados nos quais as espécies são mais utilizadas.
                    Os dados brutos e referentes aos anuários de outros anos
                    podem ser encontrados na página do", 
                    tags$a(href="https://www.aphis.usda.gov/aphis/ourfocus/animalwelfare/sa_obtain_research_facility_annual_report/ct_research_facility_annual_summary_reports", "USDA.")),
                    br(),
                    br(),
                    br(),
                    br(),
                    br()
                    ))),
           # Painel 2 -----------------------
           tabPanel(title = "Espécies",
                    fluidPage(
                      titlePanel("Avaliação por espécie"),
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
                                       choiceValues = list("sim","nao", "todos"),
                                       choiceNames = list("Sim", "Não", "Todos"),
                                       selected = "sim", 
                                       inline = T),
                          conditionalPanel(
                            condition = "input.utilizado == 'sim'",
                            radioButtons("dor",
                                         "Exposição a dor:",
                                         choiceValues = list("sim","nao", "todos"),
                                         choiceNames = list("Sim", "Não", "Todos"),
                                         selected = "sim", 
                                         inline = TRUE)),
                          conditionalPanel(
                            condition = "input.utilizado == 'sim'& input.dor == 'sim'",
                            radioButtons("droga",
                                         "Anestesia/analgesia:",
                                         choiceValues = list("sim","nao", "todos"),
                                         choiceNames = list("Sim", "Não", "Todos"),
                                         selected = "sim", 
                                         inline = T)),
                          selectInput("especie",
                                      "Selecione a especie:",
                                      choices = c("Porquinho-da-índia" = "cavia_p",
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
                      # theme = shinytheme("journal"),
                      titlePanel("Utilização de animais em experimentação por estados"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("n_animais",
                                      label = "Nº de animais",
                                      min = 0,
                                      max = 30000,
                                      step = 10,
                                      dragRange = T, 
                                      value = c(0,30000)),
                          radioButtons("utilizado2",
                                       "Utilizados em pesquisa:",
                                       choiceValues = list("sim","nao", "todos"),
                                       choiceNames = list("Sim", "Não", "Todos"),
                                       selected = "sim", 
                                       inline = T),
                          conditionalPanel(
                            condition = "input.utilizado2 == 'sim'",
                            radioButtons("dor2",
                                         "Exposição a dor:",
                                         choiceValues = list("sim","nao", "todos"),
                                         choiceNames = list("Sim", "Não", "Todos"),
                                         selected = "sim", 
                                         inline = TRUE)),
                          conditionalPanel(
                            condition = "input.utilizado2 == 'sim'& input.dor2 == 'sim'",
                            radioButtons("droga2",
                                         "Anestesia/analgesia:",
                                         choiceValues = list("sim","nao", "todos"),
                                         choiceNames = list("Sim", "Não", "Todos"),
                                         selected = "sim", 
                                         inline = T)),
                          selectInput("especie2",
                                      "Selecione a especie:",
                                      choices = c("Porquinho-da-índia" = "cavia_p",
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
                                      selectize = T)),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Mapa", leafletOutput("mapa"),
                                       "Ranking", plotOutput("PlotEstados")))
                                      # ,
                                      # tabPanel("Mapa", leafletOutput("mapa"))
                                      #                     tabPanel("Sumário", tableOutput("summary")),
                                      #                     tabPanel("Tabela", tableOutput("table")
                          
                        )
                      )
                    )
           ),
           # Painel 4 --------------------
           tabPanel(title = "Sobre",
                    fluidPage(
                    column(3,imageOutput("imagemfinal"),
                           br(),
                           br(),
                           br(),
                          div(imageOutput("logoufpr"), align = "center")),
                    column(7, h3("Desenvolvimento:"),
                           br(),
                           br(),
                           h5("Gislayne de Paula Bueno"),
                           h6("Elementos de programação aplicados a estatística"),
                           h6("Estatística - UFPR 2º semestre - 2023"),
                           tags$a(href="https://github.com/buenogi", "GitHub"),
                          )
                           
                                 )),
                           
                    )
                  


