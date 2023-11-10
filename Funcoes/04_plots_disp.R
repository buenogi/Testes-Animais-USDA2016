plots_disp <- function(dados, var1 = "Nanimais", var2, especie){
  ggplot(dados, aes_string(x = var1, y = var2, color = especie)) +
    geom_point(size = 5, alpha = 0.7) +
    labs(title = paste("Gráfico de dispersão para", var2))
}