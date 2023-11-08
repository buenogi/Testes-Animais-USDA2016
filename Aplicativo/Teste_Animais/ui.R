library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
fluidPage(
    theme = shinytheme("journal"),
    # Application title
    titlePanel("Testagem em animais - USDA 2016"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
                    sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h2("Sobre:"),
          br(),
          h4("Anualmente o USDA (Departamento de Agricultura dos Estados Unidos) coleta as informações de animais empregados em 
      pesquisas utilizadas em diferentes instâncias de experimentação. Esta
      prática vai de encontro ao", em('Animal Welfare Act.'),"Este aplicativo foi realizado a partir da análise das observações
      coletadas pelo USDA no ano de 2016."),
          br(),
            plotOutput("distPlot")
        )
    )
)
