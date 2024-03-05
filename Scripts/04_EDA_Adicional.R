# Análises adicionais
library(tidyverse)
library(magrittr)
library(GGally)

dadosT <- read_csv(file = "Dados/Processados/dados_processados_adc1.csv",
                      show_col_types = F)

cores_sp <- c(
  "cavia_p" = "#052935",
  "outras_especies" = "#00525b",
  "coelhos" = "#007e72",
  "hamsters" = "#45ab79",
  "primatas_nao_humanos" = "#98d574",
  "caes" = "#d0db5e",
  "porcos" = "#face4b", 
  "animais_de_fazenda" = "#f7b22d",
  "gatos" = "#ee7014",
  "ovelhas" = "#e64a19")
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



# Coeficientes de correlação
dadosT_NAOM <- na.omit(dadosT)
cor(dadosT_NAOM$Nanimais, dadosT_NAOM$exp_vida_anos)
cor(dadosT_NAOM$Nanimais, dadosT_NAOM$maturidade_sexual)
cor(dadosT_NAOM$Nanimais, dadosT_NAOM$tempo_gestacao)
cor(dadosT_NAOM$Nanimais, dadosT_NAOM$peso)
cor(dadosT_NAOM$Nanimais, dadosT_NAOM$custo_man_unidade)
cor(dadosT_NAOM$Nanimais, dadosT_NAOM$custo_total)

# Correlogramas ----------------------------------------------------------------

dadosT%>%
  select(Nanimais,exp_vida_anos,maturidade_sexual,
         tempo_gestacao,peso, custo_man_unidade)%>%
  plot(pch=20 , cex=1.5 , col="#69b3a2")

ggpairs(dadosT) 

# Heatmap
dadosT_NAOM <- na.omit(dadosT)
titulos <- c("Nº animais", "Expectativa\n de vida", 
             "Maturidade\nsexual","Tempo de\n gestação",
             "Peso", "Custo de manutenção")

dadosHM <- select_if(dadosT_NAOM, is.numeric)
colnames(dadosHM) <- titulos
matriz_correlacao <- cor(dadosHM)

Paleta_3 <- c("#cbffa4","#98d574","#45ab79", "#007e72","#00525b","#052935" )
corrplot::corrplot(matriz_correlacao, method = "color", 
                         type = "upper", tl.cex = 1.0,tl.srt=45, 
                         addCoef.col = "black", 
                         tl.col = "black", addgrid.col = "darkgray", 
                         col = Paleta_3)

# Gráficos separados  ----------------------------------------------------------

variaveis <- c("exp_vida_anos", "maturidade_sexual", "tempo_gestacao",
               "peso", "custo_man_unidade", "custo_total")

source(file = "Funcoes/04_plots_disp.R")


graficos <- lapply(variaveis, plots_disp, dados = dadosT, var1 = "Nanimais", 
                   especie = "especie")



dadosT_NAOM%>%
  ggplot(aes(x = custo_man_unidade, y = Nanimais , 
             color = especie, size = peso/1000)) +
  geom_point() +
  labs(y = "Nº de animais", 
       x = "Custo de manutenção (un)",
       color = "Espécie",
       size = "Peso (Kg)") +
  scale_size_continuous(range = c(2,15))+
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
    "cavia_p" = "Porquinho-da-índia",
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



