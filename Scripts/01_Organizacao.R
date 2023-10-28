library(tidyverse)

# Organização de dados


dados_A <- read.csv("Dados/ALL_PAINTYPES_2016.csv")     # Todos os tipos de dor
dados_B <- read.csv("Dados/COLUMNB_ANYU_ALL_2016.csv")  # Animais mantidos para este fim mas não utilizados
dados_C <- read.csv("Dados/COLUMNC_NPND_ALL_2016.csv")  # Animais utilizados para pesquisa sem exposição a dor e sem tratamento
dados_D <-  read.csv("Dados/COLUMND_WPWD_ALL_2016.csv") # Animais utilizados na pesquisa expostos a dor tratados com  fármacos
dados_E <-  read.csv("Dados/COLUMNE_WPND_ALL_2016.csv") # Animais utilizados na pesquisa expostos a dor sem tratamento

# Abordar na introdução:
# 1. Qual é o posicionamento da população estadunidense com relação ao uso de animais em pesquisa?
# 2. Quais são as legislações a respeito?
# 3. Qual é a discussão do ponto de vista ético?
# 4. Qual é o ponto de vista científico a respeito?
# Pontos a serem avaliados a partir destes dados:
# 5. Há diferença com relação as espécies utilizadas em ensaios com exposição a dor?
# 6. há informações a respeito do motivo pelo qual estes animais são empregados em pesquisa?Qual é a relevancia de cada espécie?
# 7. Qual são as espécies mais utilizadas? 
# 8. Há diferença com relação as espécies utilizadas por tipo de ensaio ?
# 9. Quais são os Estados que mais usam animais em pesquisa?

# ALteração da nomenclatura das variáveis

nomes_var <- c("estado","outras_especies","gatos","caes","caviap", 
      "hamsters", "primatasNH", "AnimaisDeFazenda",
      "porcos", "coelhos", "ovelhas", "total")

colnames(dados_A) <- nomes_var
colnames(dados_B) <- nomes_var
colnames(dados_C) <- nomes_var
colnames(dados_D) <- nomes_var
colnames(dados_E) <- nomes_var


# pivotagem dos dados para criação da variável espécie
dados_B <- pivot_longer(dados_B, cols = c("outras_especies","gatos","caes",
                                          "caviap", "hamsters", "primatasNH",
                                          "AnimaisDeFazenda", "porcos",
                                          "coelhos", "ovelhas"), 
                        names_to = "especie" )




# Criação de variáveis

dados_B$utilizado <- "não"
dados_B$dor <- "não"
dados_B$droga <- "não"

dados_C$utilizado <- "sim"
dados_C$dor <- "não"
dados_C$droga <- "não"

dados_D$utilizado <- "sim"
dados_D$dor <- "sim"
dados_D$droga <- "sim"

dados_E$utilizado <- "sim"
dados_E$dor <- "sim"
dados_E$droga <- "não"





# União dos bancos de dados 
dados_full <- rbind(dados_B,dados_C,dados_D, dados_E)
