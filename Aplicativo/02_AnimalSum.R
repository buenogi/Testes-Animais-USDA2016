AnimalSum <- function(dados, especie,n_animais) {
 resumo <-  dados%>%
    group_by(especie)%>%
    summarise(minimo = min(n_animais),
              "1º Quantil" = quantile(n_animais, 0.25),
              "Mediana" = quantile(n_animais, 0.5),
              "3º Quantil" = quantile(n_animais, 0.75),
              maximo = max(n_animais),
              media = mean(n_animais),
              desvio = sd(n_animais))
 return(resumo)
}
