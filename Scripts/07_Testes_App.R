library(tidyverse)
source(file = "Documentos/CE302_Teste_em_animais/trabalho-final-buenogi/Funcoes/03_Frequencia.R")
# --------------fREQE--------
DADOS <- read.csv("Documentos/CE302_Teste_em_animais/trabalho-final-buenogi/Dados/Processados/dados_processados.csv")
DADOS_B <- DADOS%>%
  filter(utilizado =="sim")%>%
  filter(especie =="coelhos")

NAnimaistotal <- sum(DADOS$n_animais)
NAnimaisB <- sum(DADOS_B$n_animais)


Frequencia(totais)
NAnimaistotal <- sum(totais$n_animais)
View(NAnimaistotal)

FreqSP <- Frequencia(DADOS,NAnimaistotal,var = "especie")
FreqSP <- FreqSP%>%
  filter(especie =="coelhos")

FreqSP_B <- Frequencia(DADOS_B,NAnimaisB,var = "especie")
#----------------Condicoes--------------
x <- DADOS %>%
  filter(especie == "coelhos")

input <- data_frame(utilizado = "não",  dor = "sim",droga = "sim")


if (input$utilizado == "sim") {
  x <- x%>%
    filter(utilizado == "sim")
  if (input$dor == "sim") {
    x <- x%>%
      filter(dor == "sim", droga == input$droga)
  } else if (input$dor == "não") {
    x <- x %>% filter(dor == "não")
  }
} else {
  x <- x %>% filter(utilizado == "não")
}

print(sum(x$n_animais))




# Plot ------------------
A <- FreqSP%>%
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
plotly::ggplotly(A)
