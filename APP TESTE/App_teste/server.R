## Server.R
library(shiny)
library(tidyverse)
library(jsonlite)
library(sf)
library(rnaturalearth)

# Dados ------------------------------------------------------------------------

dados <- read_csv(file = "data/dados_processados.csv")
US <- read_sf("data/USA_latlong.csv")

#Conversão de  variáveis--------------------------------------------------------

colunas <- list("estado", "especie","utilizado","dor","droga")
for(coluna in colunas){
  dados[[coluna]] <-  as.factor(dados[[coluna]])
}


# Funções-----------------------------------------------------------------------
source("03_Frequencia.R")
# Server -----------------------------------------------------------------------
function(input, output, session) {

  dadosReact <- reactiveVal(dados)
  FILTRADOS <- reactiveValues(R1 = NULL)
  FILTRADOSN <- reactiveValues(R2 = NULL) 
  
  observe({
    uti <- input$utilizado
    pain <- input$dor
    drug <- input$droga
    sp <- input$especie
    
    dados_filtrados <- dadosReact()
    
    if (uti == "todos") {
      R1 <- dados_filtrados %>%
        filter(especie %in% sp)
    } else if (uti == "nao") {
      R1 <- dados_filtrados %>%
        filter(dor == "não") %>%
        filter(especie %in% sp)
    } else if (pain == "todos") {
      R1 <- dados_filtrados %>%
        filter(utilizado == "sim") %>%
        filter(especie %in% sp)
    } else if (pain == "nao") {
      R1 <- dados_filtrados %>%
        filter(utilizado == "sim" & dor == "não") %>%
        filter(especie %in% sp)
    } else if (uti == "sim" & pain == "sim" & drug == "todos") {
      R1 <- dados_filtrados %>%
        filter(utilizado == "sim" & dor == "sim") %>%
        filter(especie %in% sp)
    } else if (uti == "sim" & pain == "sim" & drug == "nao") {
      R1 <- dados_filtrados %>%
        filter(utilizado == "sim" & dor == "sim" & droga == "não") %>%
        filter(especie %in% sp)
    } else {
      R1 <- dados_filtrados %>%
        filter(utilizado == "sim" & dor == "sim" & droga == "sim") %>%
        filter(especie %in% sp)
    }
    
    print(R1)
    
    FILTRADOS$R1 <- R1
  })
  
  FreqFiltrados <- reactiveValues(resultado = NULL)
  
  observe({
    sp <- input$especie
    
    if (input$denominador == "Comparativa") {
      NAnimaistotal <- sum(FILTRADOS$R1$n_animais)
      FreqFiltrados$resultado <- Frequencia(FILTRADOS$R1, NAnimaistotal, var = "especie")
    } else {
      NAnimaistotal <- sum(dados$n_animais)
      FreqFiltrados$resultado <- Frequencia(FILTRADOS$R1, NAnimaistotal, var = "especie") %>%
        filter(especie %in% sp)
    }
    print(FreqFiltrados$resultado)
  })
  
  observe({
    N1 <- input$n_animais[1]
    N2 <- input$n_animais[2]
    
    R2 <- FILTRADOS$R1%>%
      filter(n_animais > N1 & n_animais < N2)
    
    R2 <- left_join(R2, US, by = c("estado"= "estado"))
    
    R2$nome <- str_to_title(R2$nome)
    print(R2)
    FILTRADOSN$R2 <- R2
    })
  
  output$freqPlot <- plotly::renderPlotly({
    
    P1 <- FreqFiltrados$resultado%>%
      ggplot(aes( y = freqRel,x = foo, fill = reorder(especie, freqRel)))+
      geom_bar(position = "stack", stat = "identity",width = 0.5)+
      scale_fill_manual(values = c("cavia_p" = "#052935",
                                   "outras_especies" = "#00525b",
                                   "coelhos" = "#007e72",
                                   "hamsters" = "#45ab79",
                                   "primatas_nao_humanos" = "#98d574",
                                   "caes" = "#d0db5e",
                                   "porcos" = "#face4b", 
                                   "animais_de_fazenda" = "#f7b22d",
                                   "gatos" = "#ee7014",
                                   "ovelhas" = "#e64a19"),
                        labels = c("cavia_p" = "C. porcellus",
                                   "outras_especies" = "Outras espécies",
                                   "coelhos" = "Coelhos",
                                   "hamsters" = "Hamsters",
                                   "primatas_nao_humanos" = "Primatas não humanos",
                                   "caes" = "Cães",
                                   "porcos" = "Porcos",
                                   "animais_de_fazenda" = "Animais de fazenda",
                                   "gatos" = "Gatos",
                                   "ovelhas" = "Ovelhas"))+
      labs(x = "Animais",
           y = "(%)",
           fill = "Espécie",
           title = "Frequencia de cada espécie da composição geral")+
      scale_y_continuous(labels = scales::label_percent())+
      coord_flip()+
      theme_minimal()+
      theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))
    plotly::ggplotly(P1)
  })
  
  output$PlotPaises <- renderPlot({
   P2 <-  FILTRADOSN$R2 %>%
      mutate(nome = reorder(nome, n_animais),
             especie = reorder(especie, n_animais)) %>%
      ggplot(aes(x = nome, y = n_animais, fill = especie)) +
      geom_col(position = "stack", stat = "identity") +
      scale_fill_manual(values = c(
        "cavia_p" = "#052935",
        "outras_especies" = "#00525b",
        "coelhos" = "#007e72",
        "hamsters" = "#45ab79",
        "primatas_nao_humanos" = "#98d574",
        "caes" = "#d0db5e",
        "porcos" = "#face4b", 
        "animais_de_fazenda" = "#f7b22d",
        "gatos" = "#ee7014",
        "ovelhas" = "#e64a19"
      ), labels = c(
        "cavia_p" = "C. porcellus",
        "outras_especies" = "Outras espécies",
        "coelhos" = "Coelhos",
        "hamsters" = "Hamsters",
        "primatas_nao_humanos" = "Primatas não humanos",
        "caes" = "Cães",
        "porcos" = "Porcos",
        "animais_de_fazenda" = "Animais de fazenda",
        "gatos" = "Gatos",
        "ovelhas" = "Ovelhas")) +
      coord_flip() +
      labs(y = "Nº de animais", 
           x = "Estado", 
           fill = "Espécie")+
      theme_minimal()+
      theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))
   # plotly::ggplotly(P2)
 
  })
  P2
}
