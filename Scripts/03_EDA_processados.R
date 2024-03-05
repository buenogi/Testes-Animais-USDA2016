# EDA animais mantidos para testagem mas não utilizados

library(tidyverse)
library(sf)
library(plotly)
library(stringr)
library(magrittr)
library(ggraph)


dadosProcessados <- read_csv(file = "Dados/Processados/dados_processados.csv")

dados <- dadosProcessados%>%
            filter(utilizado!= "sim")

US <- read_sf("Dados/Brutos/mapa/States_shapefile.shx")

dadosProcessados <- full_join(dadosProcessados, US, by = c("estado"= "State_Code"))
dadosProcessados$State_Name <- str_to_title(dadosProcessados$State_Name)
dadosProcessados$Program <- NULL
dadosProcessados$FID <- NULL
dadosProcessados$FID_1<- NULL
dadosProcessados$Flowing_St <- NULL
dadosProcessados <- na.omit(dadosProcessados)

# 1º Total de animais mantidos por espécie

dados%>%
  group_by(especie)%>%
  summarise(total_mantidos = sum(n_animais))

# 2º Nº de animais mantidos em cativeiro por espécie para manutenção e uso
# Espécie x n_animais - Gŕafico de barras - Ranking das espécies
dadosProcessados%>% 
  ggplot(aes(x = especie,  y = n_animais, fill = utilizado))+
  geom_col(position="stack")+
  scale_fill_manual(values = c("não" = "#e64a19", "sim" = "#052935"))+
  scale_x_discrete(limits = c("ovelhas","gatos",
                              "animais_de_fazenda", "porcos",
                              "caes","hamsters",
                              "primatas_nao_humanos","coelhos",
                              "outras_especies","cavia_p"),
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
  labs(x = "Espécies",
       y = "Nº de Animais",
       fill = "Utilização")+
  coord_flip()+
  theme_minimal()

# 3º Avaliação das outras variáveis qualitativas

# Utilização - Percentual de animais utilizados e não utilizados

Nanimais_Total <- sum(dadosProcessados$n_animais)
freq_utilizacao <- dadosProcessados%>%
  group_by(utilizado)%>%
  summarise(freqAbs = sum(n_animais),
            freqRel = (sum(n_animais)/Nanimais_Total),
            freqRelPer = (sum(n_animais)/Nanimais_Total)*100, 
            x = ".")

freq_utilizacao%>%
  ggplot(aes(x = x,  y = freqRel, fill = utilizado))+
  geom_col(position="stack", width = 0.15)+
  scale_fill_manual(values = c("não" = "#e64a19", "sim" = "#052935"))+
  labs(x = "Total",
       y = "Nº de Animais",
       fill = "Utilização")+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 18, face = "bold"))
  
# Dor - Percentual de animais que experenciam dor durante a experimentação

freq_dor <- dadosProcessados%>%
  group_by(dor)%>%
  summarise(freqAbs = sum(n_animais),
            freqRel = (sum(n_animais)/Nanimais_Total),
            freqRelPer = (sum(n_animais)/Nanimais_Total)*100, 
            x = ".")


freq_dor%>%
  ggplot(aes(x = x,  y = freqRel, fill = dor))+
  geom_col(position="stack", width = 0.15)+
  scale_fill_manual(values = c("não" = "#e64a19", "sim" = "#052935"))+
  labs(x = "Total",
       y = "Nº de Animais",
       fill = "Dor")+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 18, face = "bold"))

# Analgesia/anestesia - dos que experenciam dor, o percentual dos que recebem 
# tratamento para este fim

NanimaisDor_Total <-dadosProcessados%>%
                     filter(dor == "sim")%>%
                     summarise(NanimaisDor_Total  = sum(n_animais))

NanimaisDor_Total <- NanimaisDor_Total$NanimaisDor_Total

freq_droga <- dadosProcessados%>%
  filter(dor == "sim")%>%
  group_by(droga)%>%
  summarise(freqAbsT = sum(n_animais),
            freqRelT = (sum(n_animais)/Nanimais_Total),
            freqRelPerT = (sum(n_animais)/Nanimais_Total)*100, 
            freqRel = (sum(n_animais)/NanimaisDor_Total),
            freqRelPer = (sum(n_animais)/NanimaisDor_Total)*100, 
            x = ".")

# Com relação aos que experenciam dor
freq_droga%>%
  ggplot(aes(x = x,  y = freqRel, fill = droga))+
  geom_col(position="stack", width = 0.15)+
  scale_fill_manual(values = c("não" = "#e64a19", "sim" = "#052935"))+
  labs(x = "Total",
       y = "Nº de Animais",
       fill = "Droga")+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 18, face = "bold"))


# Avaliação dos animais que experenciam dor

dataDor <- dadosProcessados%>%
  filter(dor == "sim")

totaisDor_SUM <- dataDor%>%
  group_by(especie)%>%
  summarise(minimo = min(n_animais),
            "1º Quantil" = quantile(n_animais, 0.25),
            "Mediana" = quantile(n_animais, 0.5),
            "3º Quantil" = quantile(n_animais, 0.75),
            maximo = max(n_animais),
            media = mean(n_animais),
            desvio = sd(n_animais))

# Ranking das espécies que experenciam dor

dataDor%>% 
  ggplot(aes(x = especie,  y = n_animais, fill = droga))+
  geom_col(position="stack")+
  scale_fill_manual(values = c("não" = "#e64a19", "sim" = "#052935"))+
  scale_x_discrete(limits = c("gatos","ovelhas","animais_de_fazenda",
                              "caes","primatas_nao_humanos","porcos",
                              "outras_especies","hamsters", "cavia_p", 
                              "coelhos"),
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
  labs(x = "Espécies",
       y = "Nº de Animais",
       fill = "Terapia")+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 18, face = "bold"))

# Onde são realizadas as experimentações com dor?

dataDor%>% 
  ggplot(aes(x = reorder(State_Name, n_animais),  y = n_animais, fill = droga))+
  geom_col(position="stack")+
  scale_fill_manual(values = c("não" = "#e64a19", "sim" = "#052935"))+
  labs(x = "Espécies",
       y = "Nº de Animais",
       fill = "Terapia")+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 18, face = "bold"))

# Analise de dados aninhados
library(circlepackeR)
library(data.tree)


for(i in 1:nrow(dadosProcessados)) {
  if(dadosProcessados$utilizado[i] == "sim") {
    dadosProcessados$utilizado[i] <- "Utilizado"
  } else {
    dadosProcessados$utilizado[i] <- "Não utilizado"
  }
}

for(i in 1:nrow(dadosProcessados)) {
  if(dadosProcessados$dor[i] == "sim") {
    dadosProcessados$dor[i] <- "Dor envolvida"
  } else {
    dadosProcessados$dor[i] <- "Sem envolvimento de dor"
  }
}

for(i in 1:nrow(dadosProcessados)) {
  if(dadosProcessados$droga[i] == "sim") {
    dadosProcessados$droga[i] <- "Recebeu anestesia/analgesia"
  } else {
    dadosProcessados$droga[i] <- "Não recebeu anestesia/analgesia"
  }
}

dadosProcessados$especie <- stringr::str_to_title(dadosProcessados$especie)

dadosProcessados$pathString <- paste("world", dadosProcessados$utilizado,
                                     dadosProcessados$dor,
                                     dadosProcessados$droga,
                                     dadosProcessados$especie, sep = "/")

population <- as.Node(dadosProcessados)

circlepackeR(population, size = "n_animais")

circlepackeR(population, size = "n_animais",
                  color_min = "white",
                  color_max = "#052935",width = 600,height = 600)

