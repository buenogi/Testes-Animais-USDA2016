## Server.R
library(shiny)
library(tidyverse)

# Dados ------------------------------------------------------------------------

dados <- read_csv(file = "data/dados_processados.csv")

#Conversão de  variáveis--------------------------------------------------------

colunas <- list("estado", "especie","utilizado","dor","droga")
for(coluna in colunas){
  dados[[coluna]] <-  as.factor(dados[[coluna]])
}
# Funções-----------------------------------------------------------------------
source("03_Frequencia.R")
# Server -----------------------------------------------------------------------
function(input, output, session) {
  filtrados <- reactive({

    x <- dados %>% filter(especie %in% input$especie)
    
    uti <- input$utilizado
    dor <- input$dor
    droga <- input$droga
    
    if(uti == "nao"){
      y <- x %>% filter(utilizado == "não")
    }else if( dor == "nao"){
      y <- x %>% filter(utilizado == "sim" & dor == "não")
    }else if(droga == "sim"){
      y <- x %>% filter(utilizado == "sim" & dor == "sim" & droga == "sim")
    }else if(droga == "nao"){
      y <- x %>% filter(utilizado == "sim" & dor == "sim" & droga == "não")
    }
    return(y)
  })
  
  
  
  den_selecionado <- reactive(input$denominador)
    
    FreqFiltrados <- reactive({if (den_selecionado() == "Comparativa") {
      NAnimaistotal <- sum(filtrados()$n_animais)
      FreqSP <- Frequencia(filtrados(), NAnimaistotal, var = "especie")
    } else {
      NAnimaistotal <- sum(dados$n_animais)
      FreqSP <- Frequencia(dados, NAnimaistotal, var = "especie") %>%
        filter(especie %in% input$especie)
    }
    return(FreqSP)
      })


    output$freqPlot <- plotly::renderPlotly({

        P1 <- FreqFiltrados()%>%
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

}
