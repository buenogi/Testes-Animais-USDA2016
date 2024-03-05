plots_disp <- function(dados, var1 = "Nanimais", var2, especie){
  ggplot(dados, aes_string(y = var1, x = var2, color = especie)) +
    geom_point(size = 5, alpha = 0.7) +
    labs(title = paste("Gráfico de dispersão para", var2),
         y = "Nº de animais", 
         x = stringr::str_to_title(var2),
         color = "Espécie")+
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
}