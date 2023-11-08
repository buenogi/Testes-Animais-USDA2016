library(tidyverse)
library(sf)
library(plotly)
library(stringr)
library(magrittr)

#Para esta análise inicial, será avaliado o conj de dados simplificado no qual
# há o numero de animais utilizado independentemente da dor e administração de 
# drogas

totais <- read_csv(file = "Dados/Processados/dados_simplificado.csv")


# Definição da paleta de cores:
# Principal = "#052935"
# #007e72 , #45ab79, #98d574 , 
# #897ba0 , #f7b4cf

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

c("#052935" , "#00c6aa", "#2a8476", "#344b46")


# Análise exploratória - Dados totais

totais$estado <- as.factor(totais$estado)
totais$especie <- as.factor(totais$especie)

# Obs: Neste conjunto de dados estão contabilizados todos os animais utilizados
# em pesquisas em 2016, independentemente do tipo de dor associada ao 
# procedimento realizado

# Variáveis:
# 1 - Estado - Qualitativa nominal
# 2 - Total - Quantitativa discreta - Nº de animais totais 
# 1 - Espécie - Qualitativa nominal
# 1 - n_animais -  Quantitativa discreta - Nº de animais por espécie

summary(totais)

# medidas sumarizadas por espécie

totais_SUM <- totais%>%
  group_by(especie)%>%
  summarise(minimo = min(n_animais),
            "1º Quantil" = quantile(n_animais, 0.25),
            "Mediana" = quantile(n_animais, 0.5),
            "3º Quantil" = quantile(n_animais, 0.75),
            maximo = max(n_animais),
            media = mean(n_animais),
            desvio = sd(n_animais)
            )

# Avaliação de frequencias

frequencias <- totais%>%
  group_by(especie)%>%
  summarise(FreqAbs = sum(n_animais), 
            FreqRel = sum(n_animais)/Nanimais_Total, 
            FreqRelPer =  (sum(n_animais)/Nanimais_Total)*100, 
            x ="." )

# Visualização

frequencias%>%
  mutate(especie = factor(especie,levels = c("ovelhas","gatos",
                                             "animais_de_fazenda", "porcos",
                                             "caes","primatas_nao_humanos",
                                             "hamsters","coelhos",
                                             "outras_especies","cavia_p")))%>%
  ggplot(aes( y = FreqAbs,x = x, fill = especie))+
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
       y = "Nº de animais",
       fill = "Espécie")+
  scale_y_continuous(labels = scales::label_number()) +
                      coord_flip()+
                      theme_minimal()+
  
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))


# Monovariada

# Total - Densidade e BoxPlot

totais %>%
  ggplot(aes(x = total, y = 1, fill = "#052935")) +
  geom_violin(color = "white", size = 1) +
  geom_boxplot(color = "white",width = 0.145) +
  labs(x = "Nº de animais",
       y = "",
       title = "Nº de animais utilizados em pesquisa") +
  scale_fill_identity() +  # Define a cor como fixa
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18, hjust = 0.5, face = "bold"))

# Por espécie - Boxplot

totais%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
  ggplot(aes(n_animais, y = 1))+
  geom_boxplot(color = "#052935")+
  facet_wrap(~especie, nrow = 10, labeller = labeller(especie = c("cavia_p" = "C. porcellus",
                                                     "outras_especies" = "Outras espécies",
                                                     "coelhos" = "Coelhos",
                                                     "hamsters" = "Hamsters",
                                                     "primatas_nao_humanos" = "Primatas não humanos",
                                                     "caes" = "Cães",
                                                     "porcos" = "Porcos",
                                                     "animais_de_fazenda" = "Animais de fazenda",
                                                     "gatos" = "Gatos",
                                                     "ovelhas" = "Ovelhas")))+
    

  labs(x = "Nº de animais", 
       y = "", 
       title = "Nº de animais utilizados em pesquisa por espécie") +
  theme_bw()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))


# Histograma:

totais%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
  ggplot(aes(n_animais))+
  geom_histogram(fill = "#052935", color = "#052935")+
  facet_wrap(~especie, nrow = 2, 
             labeller = labeller(especie = 
                                   c("cavia_p" = "C. porcellus",
                                     "outras_especies" = "Outras espécies",
                                     "coelhos" = "Coelhos",
                                     "hamsters" = "Hamsters",
                                     "primatas_nao_humanos" = "Primatas não humanos",
                                     "caes" = "Cães",
                                     "porcos" = "Porcos",
                                     "animais_de_fazenda" = "Animais de fazenda",
                                     "gatos" = "Gatos",
                                     "ovelhas" = "Ovelhas")))+
  labs(x = "Nº de animais", 
       y = "Nº de observações", 
       title = "Nº de animais utilizados em pesquisa por espécie") +
  theme_bw()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))

# Densidade

totais%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
ggplot() +
  aes(x = n_animais, fill = especie) +
  geom_density(alpha = 0.1, fill = "#052935", color = "#052935") +
  theme_minimal() +
  facet_wrap(~especie, nrow = 2, 
             labeller = labeller(especie = 
                                   c("cavia_p" = "C. porcellus",
                                     "outras_especies" = "Outras espécies",
                                     "coelhos" = "Coelhos",
                                     "hamsters" = "Hamsters",
                                     "primatas_nao_humanos" = "Primatas não humanos",
                                     "caes" = "Cães",
                                     "porcos" = "Porcos",
                                     "animais_de_fazenda" = "Animais de fazenda",
                                     "gatos" = "Gatos",
                                     "ovelhas" = "Ovelhas")))+
  labs(x = "Nº de animais", 
       y = "Densidade", 
       title = "Nº de animais utilizados em pesquisa por espécie") +
  theme_bw()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))

# N de animais x espécie - Densidade

totais %>%
  ggplot(aes(x = n_animais, y = especie)) +
  geom_violin(fill = "#052935",color = "#052935", size = 1) +
  scale_y_discrete(limits = c("ovelhas","gatos",
                              "animais_de_fazenda", "porcos",
                              "caes","primatas_nao_humanos",
                              "coelhos","outras_especies","cavia_p", "hamsters"),
                   labels = c("cavia_p" = "C. porcellus",
                              "outras_especies" = "Outras espécies",
                              "coelhos" = "Coelhos",
                              "hamsters" = "Hamsters",
                              "primatas_nao_humanos" = "Primatas não humanos",
                              "caes" = "Cães",
                              "porcos" = "Porcos",
                              "animais_de_fazenda" = "Animais de fazenda",
                              "gatos" = "Gatos",
                              "ovelhas" = "Ovelhas")) +
  labs(x = "Nº de animais", 
       y = "Espécie", 
       title = "Densidade") +
  theme_minimal() +
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))
  
  
# Bivariada


Principal = c("#052935", "#00525b",
              "#007e72", "#45ab79", 
              "#98d574", "#f9f871",
              "#897ba0", "#f7b4cf")


# Dispersão

totais%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
  ggplot(aes(x = total, y = n_animais, color = especie)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Nº de animais total", 
       y = "Nº de animais total por espécie",
       color = "Espécie") +
  scale_color_manual(values = c(
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
    "ovelhas" = "Ovelhas"
  )) +
  theme_light() +
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))


totais%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
  ggplot(aes(total, n_animais))+
  facet_wrap(~especie, nrow = 2, 
             labeller = labeller(especie = 
                                   c("cavia_p" = "C. porcellus",
                                     "outras_especies" = "Outras espécies",
                                     "coelhos" = "Coelhos",
                                     "hamsters" = "Hamsters",
                                     "primatas_nao_humanos" = "Primatas não humanos",
                                     "caes" = "Cães",
                                     "porcos" = "Porcos",
                                     "animais_de_fazenda" = "Animais de fazenda",
                                     "gatos" = "Gatos",
                                     "ovelhas" = "Ovelhas")))+
  geom_point(fill = "#052935",color = "#052935", size = 2.5, alpha = 0.7)+
  labs(x = "Nº de animais total", 
       y = "Nº de animais total por espécie")+
  theme_bw()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))

# Mapas
US <- read_sf("Dados/Brutos/mapa/States_shapefile.shx")

totais <- full_join(totais, US, by = c("estado"= "State_Code"))
totais$State_Name <- str_to_title(totais$State_Name)

totais$classes <- cut(totais$total,
                      breaks = c(0,500,1000,5000,10000,25000,50000,Inf),
                      labels = c("< 500", 
                                 "500 - 1.000",
                                 "1.000 - 5.000",
                                 "5.000 - 10.000",
                                 "10.000 - 25.000",
                                 "25.000 - 50.000",
                                 ">50.000"))

totais$classes <- as.factor(totais$classes)

totais$classes_N <- cut(totais$n_animais,
                        breaks = c(0,100,500,1000,5000,10000,25000,Inf),
                        labels = c("< 100", 
                                   "100 - 500",
                                   "500 - 1.000",
                                   "1.000 - 5.000",
                                   "5.000 - 10.000",
                                   "10.000 - 25.000",
                                   ">25.000"))

totais$classes <- as.factor(totais$classes)

totais$classes_N <- as.factor(totais$classes_N)

for( i in 1:nrow(totais)){
  if (is.na(totais$classes_N[i])) {
    totais$classes_N[i] <- "< 100"
  }
}

# Remoção Alaska, Hawaii e Porto Rico
HI <- totais%>%
  filter(estado == "HI")

AK <- totais%>%
  filter(estado == "AK")

PR <- totais%>%
  filter(estado == "PR") 

totais <- totais[!(totais$estado %in% c("HI", "AK", "PR")), ]

MAPA1 <- totais%>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = classes), color = "white")+
  scale_fill_manual(values = colorRampPalette(c("white", "#052935"))(7), 
                    name = "Nº de animais utilizados")+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"))
MAPA1

MAPA1 <- ggplotly(MAPA1)

custom_text <- paste("Estado: ", totais$State_Name, "<br>Nº de animais utilizados: ", totais$total)

MAPA1%>%
  plotly::style(text = custom_text, hoverinfo = "text")%>%
  layout( xaxis = list( linecolor = 'white'), 
          yaxis = list( linecolor = 'white'),
    legend = list(
      x = 0.1, 
      y = -0.1, 
      bgcolor = "transparent", 
      bordercolor = "transparent",  
      orientation = "h",
      itemsizing = "constant",  # Define o tamanho da amostra de cor
      itemwidth = 30
    )
  )

# Mapa por espécie

MAPA2 <- totais %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = classes_N), color = "gray") +
  scale_fill_manual(values = colorRampPalette(c("white", "#052935"))(7), 
                    name = "Nº de animais utilizados") +
  facet_wrap(~especie,nrow = 5, labeller = labeller(especie = 
                                             c("cavia_p" = "C. porcellus",
                                               "outras_especies" = "Outras espécies",
                                               "coelhos" = "Coelhos",
                                               "hamsters" = "Hamsters",
                                               "primatas_nao_humanos" = "Primatas não humanos",
                                               "caes" = "Cães",
                                               "porcos" = "Porcos",
                                               "animais_de_fazenda" = "Animais de fazenda",
                                               "gatos" = "Gatos",
                                               "ovelhas" = "Ovelhas")))+
  theme_void()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"))
MAPA2

MAPA2 <- ggplotly(MAPA2)

custom_text <- paste("Estado: ", totais$State_Name, "<br>Nº de animais utilizados: ", totais$n_animais)

MAPA2%>%
  plotly::style(text = custom_text, hoverinfo = "text")%>%
  layout( xaxis = list( linecolor = 'white'), 
          yaxis = list( linecolor = 'white'),
    legend = list(
      x = 0.1,  # Posição horizontal (0.1 da esquerda)
      y = -0.1,  # Posição vertical (-0.1 abaixo do gráfico)
      bgcolor = "transparent",  # Define a cor de fundo da legenda como transparente
      bordercolor = "transparent",  # Define a cor da borda da legenda como transparente
      orientation = "h"  # Orientação horizontal
    )
    )

# Estados que mais utilizam animais em pesquisa - Ranking dos estados

totais %>%
  mutate(State_Name = reorder(State_Name, n_animais),
         especie = reorder(especie, n_animais)) %>%
  ggplot(aes(x = State_Name, y = n_animais, fill = especie)) +
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
