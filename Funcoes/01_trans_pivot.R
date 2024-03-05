trans_pivot <- function(dados){
  dados_T <- pivot_longer(dados,cols = c("outras_especies","gatos","caes",
                                         "cavia_p", "hamsters",
                                         "primatas_nao_humanos",
                                         "animais_de_fazenda", "porcos",
                                         "coelhos", "ovelhas"), 
                          names_to = "especie" , values_to = "n_animais")
  return(dados_T)
}

trans_pivot(dados_E)
lapply(trans_pivot, dados)