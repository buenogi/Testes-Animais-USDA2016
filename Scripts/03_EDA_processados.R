# EDA animais mantidos para testagem mas não utilizados

library(tidyverse)
library(sf)
library(plotly)
library(stringr)
library(magrittr)


dadosProcessados <- read_csv(file = "Dados/Processados/dados_processados.csv")

dados <- dadosProcessados%>%
            filter(utilizado!= "sim")

# 1º Total de animais mantidos por espécie

dados%>%
  group_by(especie)%>%
  summarise(total_mantidos = sum(n_animais))

# 2º Nº de animais mantidos em cativeiro por espécie para manutenção e uso

dadosProcessados%>%
  ggplot(aes(x = reorder(especie,n_animais),  y = n_animais, fill = utilizado))+
  geom_col(position="stack")+
  scale_fill_manual(values = c("não" = "#f39530", "sim" = "#052935"))+
  coord_flip()+
  theme_minimal()
