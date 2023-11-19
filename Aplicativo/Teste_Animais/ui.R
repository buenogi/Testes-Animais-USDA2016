library(shiny)
library(shinythemes)
library(bslib)

  


navbarPage( bg = "#052935",
            tags$head(
              tags$style(type="text/css", "body {fixed-top: 70px;}")
            ),
  title = "Animal test insights",
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
                    Esta prática vai de encontro ao Ato de Bem-estar Animal 
                    (*Animal Welfare Act*). Este relatório foi realizado a partir
                    da análise das observações coletadas pelo USDA referentes ao 
                    ano de 2016. O relatório tem como objetivo descrever os dados
                    reportados ao USDA a respeito da utilização de animais em 
                    pesquisa nos EUA em 2016. Foram investigadas quais são as 
                    espécies mais utilizadas por categoria de estudo e os estados
                    nos quais cada espécie é mais utilizada. Adicionalmente 
                    buscou-se avaliar se há diferença na composição dos grupos
                    com relação aos tipos de estudo e investigar fatores que
                    motivam a utilização de algumas espécies em detrimento de
                    outras."),  
                    br(),
                    p(style="text-align: justify; font-size = 30px","A partir da utilização de metodologias de análise
                    exploratória de dados e inferência básica foi evidenciado
                    que no ano de 2016 os foram utilizados em média 460 animais
                    por estado dos EUA sendo sua maioria no estado da California.
                    A maior parte dos animais utilizados em experimentação não
                    foram submetidos a dor. A composição dos grupos de animais
                    e espécies varia de acordo com a categoria de estudo.
                    Foi encontrada correlação positiva com relação ao uso de
                    animais e número de universidades por estado. EM paralelo,
                    foi encontrada correlação negativa com relação a escolha das
                    espécies para utilização em pesquisa, o peso corporal e o custo
                    de manutenção por unidade animal. O código e o relatório detalhado
                    utilizado para as análises e construção deste aplicativo podem ser enontrados em",
                              a(href ="https://github.com/buenogi"),"."),
                    hr(),
                              div(plotly::plotlyOutput("composicaoGeral", width = "800px", height = "800px"), align = "center"))
                    )),
           # Painel 2 -----------------------
           tabPanel(title = "Espécies ",
                    fluidPage(
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
                      titlePanel("Estados"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("n_animais",
                                      label = "Nº de animais",
                                      min = 0,
                                      max = 40000,
                                      step = 10,
                                      dragRange = T, 
                                      value = c(0,40000)),
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
                                      tabPanel("Ranking", plotOutput("PlotEstados")),
                                      # tabPanel("Mapa", plotOutput("PlotEstadosMapa"))
                                      #                     tabPanel("Sumário", tableOutput("summary")),
                                      #                     tabPanel("Tabela", tableOutput("table")
                          )
                        )
                      )
                    )
           ),
           # Painel 4 --------------------
           tabPanel(title = "Sobre")
)


