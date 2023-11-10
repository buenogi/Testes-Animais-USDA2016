# Análises adicionais
library(tidyverse)
library(magrittr)
library(jsonlite)

dadosProc <- read_csv(file = "Dados/Processados/dados_processados_US.csv",
                      show_col_types = F)
dadosJSON <- fromJSON("Dados/Brutos/us-colleges-and-universities.json")
nome <- dadosJSON$name
estado <- dadosJSON$state
dadosAdc2 <- data.frame(nome, estado)

dadosAdc2%<>%
  group_by(estado)%>%
  summarise(NUniversidades = n())

dadosProc%<>%
  group_by(estado)%>%
  summarise(Nanimais = sum(n_animais))

dadosT <- left_join(dadosProc, dadosAdc2, by = "estado")

coef_correlacao <- cor(dadosT$Nanimais,dadosT$NUniversidades)

P15_CorNuniNani<- dadosT%>%
  ggplot(aes(x = NUniversidades, y = Nanimais))+
  geom_point(size = 5, alpha = 0.7, color = "#052935")+
  geom_smooth(color = "#052935" )+
  geom_text(x = max(dadosT$NUniversidades), y = max(dadosT$Nanimais),
            label = paste("Correlação =", round(coef_correlacao, 2)),
            hjust = 1.7, vjust = 1, color = "#052935") +
  labs(x = "Nº de Universidades por estado",
       y = "Nº de animais utilizados por estado")+
  theme_minimal()+
  theme(text = element_text(size = 12, face = "bold"))

ggsave(filename = "Figuras/P15_CorNuniNani.png", plot = P15_CorNuniNani)


