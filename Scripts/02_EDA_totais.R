################################################################################
############################# Análise exploratória #############################
################################################################################

# Pacotes-----------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(plotly)
library(patchwork)

# Funções-----------------------------------------------------------------------
source("Funcoes/02_AnimalSum.R")

# Dados-------------------------------------------------------------------------
totais <- read_csv(file = "Dados/Processados/dados_processados.csv")

# Obs: Neste conjunto de dados estão contabilizados todos os animais utilizados
# em pesquisas em 2016, independentemente do tipo de dor associada ao 
# procedimento realizado

# Classificação das variáveis---------------------------------------------------

totais$estado <- as.factor(totais$estado)
totais$especie <- as.factor(totais$especie)

# Variáveis:
# 1 - Estado - Qualitativa nominal
# 2 - Total - Quantitativa discreta - Nº de animais totais 
# 3 - Espécie - Qualitativa nominal
# 4 - n_animais -  Quantitativa discreta - Nº de animais por espécie
# 5 - Utilizado - Qualitativa nominal
# 6 - Dor - Qualitativa nominal
# 7 - Droga - Qualitativa nominal
# 8 - State_Name - Qualitativa nominal
# 9 - Geometry - Poligonos para criação de mapas
# 10 - Classes - Quantitativa discreta - nº de animais no total
# 11 - Classes N - Quantitativa discreta  -  nº de animais por espécie

# 1º - Medidas resumo por espécie----------------------------------------------

Total <- AnimalSum(totais,especie,n_animais)

Utilizados <- totais%>%
  filter(utilizado == "sim")%>%
  AnimalSum(especie = especie, n_animais = n_animais)

Mantidos <- totais%>%
  filter(utilizado != "sim")%>%
  AnimalSum(especie = especie, n_animais = n_animais)

# 2º - Distribuição total - (violino)---------------------------------------------------

P1_Total <- totais %>%
  ggplot(aes(x = total, y = 1, fill = "#052935")) +
  geom_violin(color = "#052935", size = 1) +
  geom_boxplot(color = "gray",width = 0.10) +
  scale_fill_identity() + 
  scale_x_continuous(limits = c(0, 50000),
                     labels = scales::label_number())+
  labs(x = "Nº de animais",
       y = "",
       title = "Total de animais para pesquisa") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.0, face = "bold"),
        text = element_text(size = 12))

P1_Utilizados <- totais %>%
  filter(utilizado == "sim")%>%
  ggplot(aes(x = total, y = 1, fill = "#052935")) +
  geom_violin(color = "#052935", size = 1) +
  geom_boxplot(color = "gray",width = 0.09) +
  scale_fill_identity() + 
  scale_x_continuous(limits = c(0, 50000),
                     labels = scales::label_number())+
  labs(x = "Nº de animais",
       y = "",
       title = "Animais utilizados em pesquisa") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.0, face = "bold"),
        text = element_text(size = 12))

P1_Mantidos <- totais %>%
  filter(utilizado != "sim")%>%
  ggplot(aes(x = total, y = 1, fill = "#052935")) +
  geom_violin(color = "#052935", size = 1) +
  geom_boxplot(color = "gray",width = 0.18) +
  scale_fill_identity() + 
  scale_x_continuous(limits = c(0, 50000),
                     labels = scales::label_number())+
    labs(x = "Nº de animais",
       y = "",
       title = "Animais  mantidos mas não utilizados") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.0, face = "bold"),
        text = element_text(size = 12))

P1_Total
P1_Utilizados
P1_Mantidos

P1 <- (P1_Total/P1_Utilizados/P1_Mantidos )+ plot_annotation(tag_levels = "A")
P1
ggsave(filename = "Figuras/01_Densidade.png", plot = P1)

# 3º - Distribuição por espécie - (boxplot)-------------------------------------

P2_Totais <- totais%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
  ggplot(aes(n_animais, y = 1))+
  geom_boxplot(color = "#052935")+
  facet_wrap(~especie, nrow = 10, 
             labeller = labeller(especie = c("cavia_p" = "C. porcellus",
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
ggsave(filename = "Figuras/02_BoxplotTotais.png", plot = P2_Totais)

P2_Utilizados <- totais%>%
  filter(utilizado == "sim")%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
  ggplot(aes(n_animais, y = 1))+
  geom_boxplot(color = "#052935")+
  facet_wrap(~especie, nrow = 10, 
             labeller = labeller(especie = c("cavia_p" = "C. porcellus",
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

ggsave(filename = "Figuras/02_BoxplotUtilizados.png", plot = P2_Utilizados)

P2_Mantidos <- totais%>%
  filter(utilizado != "sim")%>%
  mutate(especie = factor(especie,levels = c("cavia_p", "outras_especies",
                                             "coelhos","hamsters",
                                             "primatas_nao_humanos","caes",
                                             "porcos","animais_de_fazenda",
                                             "gatos","ovelhas")))%>%
  ggplot(aes(n_animais, y = 1))+
  geom_boxplot(color = "#052935")+
  facet_wrap(~especie, nrow = 10, 
             labeller = labeller(especie = c("cavia_p" = "C. porcellus",
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
P2_Mantidos

ggsave(filename = "Figuras/02_BoxplotMantidos.png", plot = P2_Mantidos)

# 4º - Distribuição por espécie - (histograma)----------------------------------

P3_Totais <- totais%>%
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

P3_Utilizados <- totais%>%
  filter(utilizado == "sim")%>%
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

P3_mantidos <- totais%>%
  filter(utilizado != "sim")%>%
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


P3_Totais
ggsave(filename = "Figuras/03_HistTotais.png", plot = P3_Totais)
P3_Utilizados
ggsave(filename = "Figuras/03_HistUtilizados.png", plot = P3_Utilizados)
P3_mantidos
ggsave(filename = "Figuras/03_HistMantidos.png", plot = P3_mantidos)
# 5º - Distribuição por espécie - (densidade)----------------------------------

P4_Totais <- totais%>%
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


P4_Utilizados <- totais%>%
  filter(utilizado == "sim")%>%
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

P4_Mantidos <- totais%>%
  filter(utilizado == "sim")%>%
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

P4_Totais
ggsave(filename = "Figuras/04_DensTotais.png", plot = P4_Totais)
P4_Utilizados
ggsave(filename = "Figuras/04_DensUtilizados.png", plot = P4_Utilizados)
P4_Mantidos
ggsave(filename = "Figuras/04_DensMantidos.png", plot = P4_Mantidos)
# 6º - Distribuição por espécie - (violino)-------------------------------------

P5_Totais <- totais %>%
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

P5_Utilizados <- totais %>%
  filter(utilizado == "sim")%>%
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

P5_Mantidos <- totais %>%
  filter(utilizado != "sim")%>%
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

P5_Totais
ggsave(filename = "Figuras/05_ViolinoTotais.png", plot = P5_Totais)
P5_Utilizados
ggsave(filename = "Figuras/05_ViolinoUtilizados.png", plot = P5_Utilizados)
P5_Mantidos
ggsave(filename = "Figuras/05_ViolinoMantidos.png", plot = P5_Mantidos)

# 7º - Totais ------------------------------------------------------------------

# Nº total de animais
Nanimais_Total <- sum(totais$n_animais)

#Nº de animais mantidos não utilizados
Nanimais_Mantidos <- totais%>%
  filter(utilizado != "sim")
Nanimais_Mantidos <-sum(Nanimais_Mantidos$n_animais)
#Nº de animais mantidos e utilizados

Nanimais_Utilizados <- totais%>%
  filter(utilizado == "sim")
Nanimais_Utilizados <-sum(Nanimais_Utilizados$n_animais)

#Nº de animais submetidos a dor
NanimaisDor_Total <-dadosProcessados%>%
  filter(dor == "sim")%>%
  summarise(NanimaisDor_Total = sum(n_animais))
NanimaisDor_Total <- NanimaisDor_Total$NanimaisDor_Total

# 8º - Tabelas de Frequencia (Gerais)-------------------------------------------

FreqUtilizacao <- totais%>%
  group_by(utilizado)%>%
  summarise(freqAbs = sum(n_animais),
            freqRel = (sum(n_animais)/Nanimais_Total),
            freqRelPer = (sum(n_animais)/Nanimais_Total)*100, 
            x = ".")

FreqDor <- totais%>%
  group_by(dor)%>%
  summarise(freqAbs = sum(n_animais),
            freqRel = (sum(n_animais)/Nanimais_Total),
            freqRelPer = (sum(n_animais)/Nanimais_Total)*100, 
            x = ".")

FreqDroga <- totais%>%    # Analgesia/anestesia - dos que experenciam dor o
  filter(dor == "sim")%>% # percentual dos que recebem tratamento para este fim
  group_by(droga)%>%
  summarise(freqAbsT = sum(n_animais),
            freqRelT = (sum(n_animais)/Nanimais_Total),
            freqRelPerT = (sum(n_animais)/Nanimais_Total)*100, 
            freqRel = (sum(n_animais)/NanimaisDor_Total),
            freqRelPer = (sum(n_animais)/NanimaisDor_Total)*100, 
            x = ".")
# 9º - Gráficos de frequencia (Gerais) -----------------------------------------

# Utilização - Percentual de animais que foram utilizados para experimentação

P6_Utilizacao <- FreqUtilizacao%>%
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

FreqDor%>%
  ggplot(aes(x = x,  y = freqRel, fill = dor))+
  geom_col(position="stack", width = 0.15)+
  scale_fill_manual(values = c("não" = "#e64a19", "sim" = "#052935"))+
  labs(x = "Total",
       y = "Nº de Animais",
       fill = "Dor")+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 18, face = "bold"))



# Com relação aos que experenciam dor
FreqDroga%>%
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





















#8º - Frequencias por especie -------------------------------------------------------

# Frequencias por especie
Frequencia_Total <- totais%>%
  group_by(especie)%>%
  summarise(FreqAbs = sum(n_animais), 
            FreqRel = sum(n_animais)/Nanimais_Total, 
            FreqRelPer =  (sum(n_animais)/Nanimais_Total)*100,
            x = ".")

Frequencia_Utilizados <- totais%>%
  filter(utilizado == "sim")%>%
  group_by(especie)%>%
  summarise(FreqAbsT = sum(n_animais), 
            FreqRelT = sum(n_animais)/Nanimais_Total, 
            FreqRelPerT =  (sum(n_animais)/Nanimais_Total)*100,
            FreqRelU = sum(n_animais)/Nanimais_Utilizados, 
            FreqRelPerU =  (sum(n_animais)/Nanimais_Utilizados)*100,
            x = ".")

Frequencia_Mantidos <- totais%>%
  filter(utilizado != "sim")%>%
  group_by(especie)%>%
  summarise(FreqAbsT = sum(n_animais), 
            FreqRelT = sum(n_animais)/Nanimais_Total, 
            FreqRelPerT =  (sum(n_animais)/Nanimais_Total)*100,
            FreqRelM = sum(n_animais)/Nanimais_Mantidos, 
            FreqRelPerM =  (sum(n_animais)/Nanimais_Mantidos)*100,
            x = ".")

# Visualização

P6_FreqTotal <- Frequencia_Total %>%
  mutate(especie = factor(especie,levels = c("ovelhas","gatos",
                                             "animais_de_fazenda", "porcos",
                                             "caes","primatas_nao_humanos",
                                             "hamsters","coelhos",
                                             "outras_especies","cavia_p")))%>%
  ggplot(aes( y = FreqRel,x = x, fill = especie))+
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
P6_FreqTotal 
P6_FreqUtilizadoT <- Frequencia_Utilizados %>%
  mutate(especie = factor(especie,levels = c("ovelhas","gatos",
                                             "animais_de_fazenda", "porcos",
                                             "caes","primatas_nao_humanos",
                                             "hamsters","coelhos",
                                             "outras_especies","cavia_p")))%>%
  ggplot(aes( y = FreqRelT,x = x, fill = especie))+
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
       title = "Frequencia de cada espécie no grupo de animais utilizados em pesquisa")+
  scale_y_continuous(labels = scales::label_percent())+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))
  
P6_FreqUtilizadoU <- Frequencia_Utilizados %>%
  mutate(especie = factor(especie,levels = c("ovelhas","gatos",
                                             "animais_de_fazenda", "porcos",
                                             "caes","primatas_nao_humanos",
                                             "hamsters","coelhos",
                                             "outras_especies","cavia_p")))%>%
  ggplot(aes( y = FreqRelU,x = x, fill = especie))+
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
       title = "Frequencia de cada espécie no grupo de animais utilizados em pesquisa")+
  scale_y_continuous(labels = scales::label_percent())+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))
  
P6_FreqMantidoT <- Frequencia_Mantidos %>%
  mutate(especie = factor(especie,levels = c("ovelhas","gatos",
                                             "animais_de_fazenda", "porcos",
                                             "caes","primatas_nao_humanos",
                                             "hamsters","coelhos",
                                             "outras_especies","cavia_p")))%>%
  ggplot(aes( y = FreqRelT,x = x, fill = especie))+
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
       title = "Frequencia de cada espécie no grupo de animais não utilizados 
       em pesquisa com relação ao total")+
  scale_y_continuous(labels = scales::label_percent())+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))

P6_FreqMantidoM <-Frequencia_Mantidos %>%
  mutate(especie = factor(especie,levels = c("ovelhas","gatos",
                                             "animais_de_fazenda", "porcos",
                                             "caes","primatas_nao_humanos",
                                             "hamsters","coelhos",
                                             "outras_especies","cavia_p")))%>%
  ggplot(aes( y = FreqRelM,x = x, fill = especie))+
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
       title = "Frequencia de cada espécie no grupo de animais não utilizados 
       em pesquisa")+
  scale_y_continuous(labels = scales::label_percent())+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 12, hjust = 0.5, face = "bold"))


P6_Frequencias <- (P6_FreqTotal/P6_FreqUtilizadoT/P6_FreqUtilizadoU/
       P6_FreqMantidoT/P6_FreqMantidoM)+plot_annotation(tag_levels = "A")
P6_Frequencias

ggsave(filename = "Figuras/06_Frequencias.png", plot = P6_Frequencias)

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

# Separação do Alaska, Hawaii e Porto Rico
HI <- dados_full%>%
  filter(estado == "HI")

AK <- dados_full%>%
  filter(estado == "AK")

PR <- dados_full%>%
  filter(estado == "PR") 

dados_full <- dados_full[!(dados_full$estado %in% c("HI", "AK", "PR")), ]

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


#Paleta:

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
