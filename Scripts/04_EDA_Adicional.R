# Análises adicionais
library(tidyverse)
library(magrittr)

dadosProc <- read_csv(file = "Dados/Processados/dados_processados.csv")
dadosAdc <- read_csv(file = "Dados/Brutos/dados_adicionais.csv")

dadosProc%<>%
  group_by(especie)%>%
  summarise(Nanimais = sum(n_animais))

dadosT <- left_join(dadosProc, dadosAdc, by = "especie")

dadosT%>%
  select(Nanimais,exp_vida_anos,maturidade_sexual,tempo_gestacao,peso, custo_man_unidade)%>%
  plot(pch=20 , cex=1.5 , col="#69b3a2")

library(GGally)
  ggpairs(dadosT) 

dadosT%>%
  ggplot(aes(x = exp_vida_anos , y = Nanimais , 
             color = especie, size = peso)) +
  geom_point(alpha = 0.7) +
  labs(y = "Nº de animais", 
       x = "Expectativa de vida",
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
P9_Dispersao
ggsave(filename = "Figuras/P9_Dispersao.png", plot = P9_Dispersao)