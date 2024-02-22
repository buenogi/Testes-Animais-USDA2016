#Server.R
library(shiny)
library(tidyverse)
library(stringr)
library(jsonlite)
library(sf)
library(rnaturalearth)
library(plotly)
library(shinythemes)
library(bslib)
library(bsicons)
library(htmlwidgets)
library(leaflet)
library(RColorBrewer) 

# Funções-----------------------------------------------------------------------
source("03_Frequencia.R")
source("02_AnimalSum.R")
# Dados ------------------------------------------------------------------------

dados <- read.csv(file = "data/dados_processados.csv")
US <- read_sf("data/USA_latlong.csv")
dados <- left_join(dados, US, by = c("estado"= "estado")) 
dados <- dados[!(dados$estado %in% c("HI", "AK", "PR")), ]
dados$nome <- str_to_title(dados$nome)
dados$latitude <- as.numeric(dados$latitude)
dados$longitude <- as.numeric(dados$longitude)
copia <- dados

#Conversão de  variáveis--------------------------------------------------------

colunas <- list("estado", "especie","utilizado","dor","droga","nome")
for(coluna in colunas){
  dados[[coluna]] <-  as.factor(dados[[coluna]])
}

# Paletas

Principal_2 <-  c("Porquinho-da-índia" = "#052935",
                  "Outras espécies" = "#00525b",
                  "Coelhos" = "#007e72",
                  "Hamsters" = "#45ab79",
                  "Primatas não humanos" = "#98d574",
                  "Cães" = "#d0db5e",
                  "Porcos" = "#face4b", 
                  "Animais de fazenda" = "#f7b22d",
                  "Gatos" = "#ee7014",
                  "Ovelhas" = "#e64a19")



rotulos <- c(
  "cavia_p" = "C. porcellus",
  "outras_especies" = "Outras espécies",
  "coelhos" = "Coelhos",
  "hamsters" = "Hamsters",
  "primatas_nao_humanos" = "Primatas não humanos",
  "caes" = "Cães",
  "porcos" = "Porcos",
  "animais_de_fazenda" = "Animais de fazenda",
  "gatos" = "Gatos",
  "ovelhas" = "Ovelhas")

# Server -----------------------------------------------------------------------
function(input, output, session) {
  output$imagemprincipal <- renderImage({
  # Imagem cabeçalho  
    list(src = "www/Design sem nome(1).png",
         width = "100%",
         height = 500)
    
  }, deleteFile = F)
  
  # Gŕafico de composição geral-------------------
  output$composicaoGeral <- renderPlotly({
    plot_ly(labels = c("Utilizados", "Categoria B\nNão utilizados", 
                       "Submetidos a dor", "Categoria C\nNão submetidos a dor",
                       "Categoria D\nReceberam terapia", "Categoria E\nNão receberam terapia"),
            parents = c("","","Utilizados","Utilizados",
                        "Submetidos a dor", "Submetidos a dor"),
            values = c(85.65,14.34,33.31,52.34,25.86,7.44),
            type = "sunburst",
            branchvalues = "total",
            hoverinfo = list("label+value+percent entry",
                             "label+value+percent entry",
                             "label+value+percent entry",
                             "label+value+percent entry",
                             "label+value+percent entry",
                             "label+value+percent entry"),
            hovertemplate = list("<b>%{label}</b><br>Percentual: %{value}(%)<br>Composição: %{percentEntry}<extra></extra>",
                                 "<b>%{label}</b><br>Percentual: %{value}(%)<br>Composição: %{percentEntry}<extra></extra>",
                                 "<b>%{label}</b><br>Percentual: %{value}(%)<br>Composição: %{percentEntry}<extra></extra>",
                                 "<b>%{label}</b><br>Percentual: %{value}(%)<br>Composição: %{percentEntry}<extra></extra>",
                                 "<b>%{label}</b><br>Percentual: %{value}(%)<br>Composição: %{percentEntry}<extra></extra>",
                                 "<b>%{label}</b><br>Percentual: %{value}(%)<br>Composição: %{percentEntry}<extra></extra>",
                                 "<b>%{label}</b><br>Percentual: %{value}(%)<br>Composição: %{percentEntry}<extra></extra>"),
            marker = list(colors = c("#e64a19", "#052935",
                                     "#ee7014", "#00525b",
                                     "#45ab79", "#f7b22d")))%>%
      layout(font = list(size = 22))
  })
  # Painel 1 ------------------------
  
  dadosReact <- reactiveVal(dados)
  FILTRADOS <- reactiveValues(R1 = NULL)
  # FILTRADOSN <- reactiveValues(R2 = NULL) 
  
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
    
    # print(R1)
    
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
    # print(FreqFiltrados$resultado)
  })
  
  # observe({
  #   N1 <- input$n_animais[1]
  #   N2 <- input$n_animais[2]
  #   
  #   R2 <- FILTRADOS$R1%>%
  #     filter(n_animais > N1 & n_animais < N2)
  #   
  #   print(R2)
  #   FILTRADOSN$R2 <- R2
  # })
  
  output$freqPlot <- plotly::renderPlotly({
    
    P1 <- FreqFiltrados$resultado%>%
      ggplot(aes( y = freqRel,x = foo, fill = reorder(especie, freqRel),
                  text =  paste0("Especie: ", FreqFiltrados$resultado$especie,
                                 "\nPercentual: ", round((FreqFiltrados$resultado$freqRel*100),2))))+
      geom_bar(position = "stack", stat = "identity",width = 0.5
               )+
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
                        labels = c("cavia_p" = "Porquinho-da-índia",
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
           title = "Frequencia por espécie")+
      scale_y_continuous(labels = scales::label_percent())+
      coord_flip()+
      theme_minimal()+
      theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))
    plotly::ggplotly(P1, tooltip = "text")
  })
  
  output$resumo <- renderTable({
    RESUMO <- FILTRADOS$R1%>%
      group_by(especie)%>%
      summarise(minimo = min(n_animais),
                "1º Quantil" = quantile(n_animais, 0.25),
                "Mediana" = quantile(n_animais, 0.5),
                "3º Quantil" = quantile(n_animais, 0.75),
                maximo = max(n_animais),
                media = mean(n_animais),
                desvio = sd(n_animais))
    # print(RESUMO)
    })
  
  output$table <- renderTable({
    FreqFiltrados$resultado
  })
   
  
  # Painel 2 -------------------------------------------------------------------
   FILTRADOSP2 <- reactiveValues(R3 = NULL)
   FILTRADOSN <- reactiveValues(R2 = NULL)

   observe({
     uti <- input$utilizado2
     pain <- input$dor2
     drug <- input$droga2
     sp <- input$especie2
     
     dados_filtrados <- dadosReact()
     
     if (uti == "todos") {
       R3 <- dados_filtrados %>%
         filter(especie %in% sp)
     } else if (uti == "nao") {
       R3 <- dados_filtrados %>%
         filter(dor == "não") %>%
         filter(especie %in% sp)
     } else if (pain == "todos") {
       R3 <- dados_filtrados %>%
         filter(utilizado == "sim") %>%
         filter(especie %in% sp)
     } else if (pain == "nao") {
       R3 <- dados_filtrados %>%
         filter(utilizado == "sim" & dor == "não") %>%
         filter(especie %in% sp)
     } else if (uti == "sim" & pain == "sim" & drug == "todos") {
       R3 <- dados_filtrados %>%
         filter(utilizado == "sim" & dor == "sim") %>%
         filter(especie %in% sp)
     } else if (uti == "sim" & pain == "sim" & drug == "nao") {
       R3 <- dados_filtrados %>%
         filter(utilizado == "sim" & dor == "sim" & droga == "não") %>%
         filter(especie %in% sp)
     } else {
       R3 <- dados_filtrados %>%
         filter(utilizado == "sim" & dor == "sim" & droga == "sim") %>%
         filter(especie %in% sp)
     }
     
     print(R3)
     
     FILTRADOSP2$R3 <- R3
   })
  
  observe({
    N1 <- input$n_animais[1]
    N2 <- input$n_animais[2]

    R2 <- FILTRADOSP2$R3%>%
      filter(n_animais > N1 & n_animais < N2)

    # print(R2)
    FILTRADOSN$R2 <- R2
  })
  
  output$PlotEstados <- renderPlot({
    P2 <-  FILTRADOSN$R2 %>%
      mutate(nome = reorder(nome, n_animais),
             especie = reorder(especie, n_animais)) %>%
      ggplot(aes(x = nome, y = n_animais, fill = especie)) +
      geom_col(position = "stack") +
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
           fill = "Espécie",
           title = "Frequência por espécie e por estado")+
      theme_minimal()+
      theme(text = element_text(size = 18, hjust = 0.5, face = "bold"))
    P2
  
  }
  , height = 1000, width = 1200
  )
  
  #cat(file = stderr(), "TÁ entrando no mapa?")
  #browser()
   output$mapa <- renderLeaflet({

     Principal = c("cavia_p" = "#052935",
                   "outras_especies" = "#00525b",
                   "coelhos" = "#007e72",
                   "hamsters" = "#45ab79",
                   "primatas_nao_humanos" = "#98d574",
                   "caes" = "#d0db5e",
                   "porcos" = "#face4b", 
                   "animais_de_fazenda" = "#f7b22d",
                   "gatos" = "#ee7014",
                   "ovelhas" = "#e64a19")
     cores <- c("#052935","#00525b","#007e72","#45ab79","#98d574",
                "#d0db5e","#face4b","#f7b22d","#ee7014","#e64a19")
     especie <- c("cavia_p",
                  "outras_especies" ,
                  "coelhos",
                  "hamsters" ,
                  "primatas_nao_humanos",
                  "caes" ,
                  "porcos" ,
                  "animais_de_fazenda" ,
                  "gatos",
                  "ovelhas")
     FILTRADOSN$R2$especie <- factor(FILTRADOSN$R2$especie, levels = especie)
     paletaFator <- colorFactor(cores,especie)
    leaflet() %>%
       addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(data = FILTRADOSN$R2, lat = ~latitude,
                       lng = ~longitude, radius = ~sqrt(n_animais)/2,
                       color = ~cores[as.integer(FILTRADOSN$R2$especie)],
                       fillOpacity = 0.7)%>%
      addLegend(position = "bottomright",
                colors = cores,
                labels = especie)
    
  })
  
   output$imagemfinal <- renderImage({
     # Imagem cabeçalho  
       list(src = "www/logo.png",
          width = "100%",
          height = 500)
     
   }, deleteFile = F)
   
   output$logoufpr <- renderImage({
     # Imagem cabeçalho  
     list(src = "www/Logo_oficial_da_UFPR_(sem_fundo).png",
          width = "70%",
          height = 250)
     
   }, deleteFile = F)
   
  
}


# 
# output$PlotEstadosMapa <- renderPlot({ 
#   usa <- ne_states(country = "United States of America", returnclass = "sf")
#   
#   convert <- FILTRADOSN$R2%>%
#     group_by(estado,nome,especie)%>%
#     mutate(longitude = longitude + runif(1, -1, 1),  
#            latitude = latitude + runif(1, -1, 1)) %>%
#     ungroup() %>%
#     st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))
#     
#   
#   convert <- convert%>%
#     mutate(especie = str_to_title(especie) %>%
#              str_replace_all("_", " ") %>%
#              str_replace_all("nao", "não") %>%
#              str_replace_all("especies", "espécies") %>%
#              str_replace_all("Caes", "Cães") %>%
#              str_replace_all("Cavia p", "Porquinho-da-índia"))
#   
#   MAPA2 <- convert%>%
#   ggplot() +
#   geom_sf(data = usa, color = "white", fill = "gray") +
#   geom_sf(data = convert, aes(size = Nanimais,
#                               color = especie, 
#                               text = paste0("Estado: ",State_Name,"\nEspécie: ", especie,
#                                             "\n Nº de animais: ", Nanimais)), alpha = 0.7)+
#   scale_size_continuous(range = c(2, 10), name = "") +
#   scale_color_manual(values = Principal_2,
#                      label = rotulos)+
#   labs(color = "Espécie",
#        size = " ")+
#   theme_void()+
#   coord_sf(xlim = c(-125, -66), ylim = c(25, 49))
#  # ggplotly(MAPA2, tooltip = "text")%>%
# #   layout( xaxis = list( linecolor = 'white'), 
# #           yaxis = list( linecolor = 'white'),
# #           legend = list(
# #             bgcolor = "transparent", 
# #             bordercolor = "transparent",  
# #             itemsizing = "constant", 
# #             itemwidth = 30))
# })
