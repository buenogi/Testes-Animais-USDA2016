library(ggplot2)
library(tidyverse)
library(sf)
library(plotly)
library(stringr)

# Análise exploratória  - Dados totais

totais <- read_csv(file = "Dados/Processados/dados_totais.csv")
totais$estado <- as.factor(totais$estado)
totais$especie <- as.factor(totais$especie)

# Obs: Neste conjunto de dados estão contabilizados todos os animais utilizados
# em pesquisas em 2016, independenetemente do tipo de dor associada ao 
# procedimento realizado

# Variáveis:
# 1 - Estado - Qualitativa nominal
# 2 - Total - Quantitativa discreta - Nº de animais totais 
# 1 - Espécie - Qualitativa nominal
# 1 - n_animais -  Quantitativa discreta - Nº de animais por espécie

summary(totais)

# Total - Densidade e BoxPlot
totais%>%
  ggplot(aes(total, y = 1))+
  geom_violin()+
  geom_boxplot(width = 0.13)

# N de animais x espécie - Densidade
totais%>%
  mutate(especie = reorder(especie, n_animais))%>%
  ggplot(aes(arrange(n_animais), especie))+
  geom_violin()+
  geom_boxplot(width = 0.13)
# dispersão
totais%>%
  ggplot(aes(total, n_animais, color = especie))+
  geom_point()
  
totais%>%
  ggplot(aes(estado, especie, size = n_animais, color = especie))+
  geom_point()

# 
ggplot(totais) +
aes(x = n_animais, fill = especie, color = especie) +
  geom_density(alpha = 0.1) +
  theme_minimal() +
  facet_wrap(~especie)+
  theme(legend.position = "bottom")

# Espécie x n_animais - Gŕafico de barras - 
  
totais%>%
  group_by(especie)%>%
  summarise(total_especie = sum(n_animais, na.rm = TRUE)) %>%
  mutate(especie = reorder(especie, total_especie))%>%
  ggplot(aes(x = especie,
             y = total_especie))+
  geom_col()+
  coord_flip()+
  theme_minimal()

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
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"))
MAPA1

MAPA1 <- ggplotly(MAPA1)

custom_text <- paste("Estado: ", totais$State_Name, "<br>Nº de animais utilizados: ", totais$total)

MAPA1%>%
  plotly::style(text = custom_text, hoverinfo = "text")%>%
  layout(
    legend = list(
      x = 0.1,  # Posição horizontal (0.1 da esquerda)
      y = -0.1,  # Posição vertical (-0.1 abaixo do gráfico)
      bgcolor = "transparent",  # Define a cor de fundo da legenda como transparente
      bordercolor = "transparent",  # Define a cor da borda da legenda como transparente
      orientation = "h"  # Orientação horizontal
    )
  )
# Mapa por espécie

MAPA2 <- totais%>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = classes_N), color = "white")+
  scale_fill_manual(values = colorRampPalette(c("white", "#052935"))(7), 
                    name = "Nº de animais utilizados")+
  facet_wrap(~especie)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"))
MAPA2

MAPA2 <- ggplotly(MAPA2)

custom_text <- paste("Estado: ", totais$State_Name, "<br>Nº de animais utilizados: ", totais$n_animais)

MAPA2%>%
  plotly::style(text = custom_text, hoverinfo = "text")%>%
  layout(
    legend = list(
      x = 0.1,  # Posição horizontal (0.1 da esquerda)
      y = -0.1,  # Posição vertical (-0.1 abaixo do gráfico)
      bgcolor = "transparent",  # Define a cor de fundo da legenda como transparente
      bordercolor = "transparent",  # Define a cor da borda da legenda como transparente
      orientation = "h"  # Orientação horizontal
    )
  )
