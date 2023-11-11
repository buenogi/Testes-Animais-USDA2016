AnimalSum <- function(dados, especie,n_animais) {
 resumo <-  dados%>%
    group_by(especie)%>%
    summarise("Média" = mean(n_animais),
              "Desvio padrão" = sd(n_animais),
              "Mínimo" = min(n_animais),
              "1º Quartil" = quantile(n_animais, 0.25),
              "Mediana" = quantile(n_animais, 0.5),
              "3º Quartil" = quantile(n_animais, 0.75),
              "Máximo" = max(n_animais))
              
 resumo <- resumo[order(resumo$Média, decreasing = TRUE), ]
 return(resumo)
}