library(tidyverse)
library(magrittr)
library(jsonlite)

# Organização de dados originais ----------------------------------------------
# Dados originais são referentes aos disponibilidados no kaggle para realização 
# da análise 
 
dados_A <- read.csv("Dados/Brutos/ALL_PAINTYPES_2016.csv")     # Todos os tipos de dor (C, D e E)
dados_B <- read.csv("Dados/Brutos/COLUMNB_ANYU_ALL_2016.csv")  # Animais mantidos para este fim mas não utilizados
dados_C <- read.csv("Dados/Brutos/COLUMNC_NPND_ALL_2016.csv")  # Animais utilizados para pesquisa sem exposição a dor e sem tratamento
dados_D <-  read.csv("Dados/Brutos/COLUMND_WPWD_ALL_2016.csv") # Animais utilizados na pesquisa expostos a dor tratados com  fármacos
dados_E <-  read.csv("Dados/Brutos/COLUMNE_WPND_ALL_2016.csv") # Animais utilizados na pesquisa expostos a dor sem tratamento

# Alteração da nomenclatura das variáveis

nomes_var <- c("estado","outras_especies","gatos","caes","cavia_p", 
      "hamsters", "primatas_nao_humanos", "animais_de_fazenda",
      "porcos", "coelhos", "ovelhas", "total")

colnames(dados_A) <- nomes_var
colnames(dados_B) <- nomes_var
colnames(dados_C) <- nomes_var
colnames(dados_D) <- nomes_var
colnames(dados_E) <- nomes_var

# Correções das observações

for(i in nomes_var){
  dados_A[[i]] <- as.character(dados_A[[i]])
  dados_B[[i]] <- as.character(dados_B[[i]])
  dados_C[[i]] <- as.character(dados_C[[i]])
  dados_D[[i]] <- as.character(dados_D[[i]])
  dados_E[[i]] <- as.character(dados_E[[i]])
}

# Nestes bancos de dados algumas observações da ordem dos milhares estão 
# separadas por vírgulas e são interpretadas como strings. Para corrigir este
# problema, antes da conversão das variáveis, irei remover as vírgulas.
# Por exemplo: 

sapply(dados_A, class)

#Substituição das vírgulas

dados_A <- as.data.frame(lapply(dados_A, function(coluna) gsub(",", "", coluna)))
dados_B <- as.data.frame(lapply(dados_B, function(coluna) gsub(",", "", coluna)))
dados_C <- as.data.frame(lapply(dados_C, function(coluna) gsub(",", "", coluna)))
dados_D <- as.data.frame(lapply(dados_D, function(coluna) gsub(",", "", coluna)))
dados_E <- as.data.frame(lapply(dados_E, function(coluna) gsub(",", "", coluna)))

# Conversão das variáveis - numericas

variaveis <- list("outras_especies","gatos","caes","cavia_p", 
  "hamsters", "primatas_nao_humanos", "animais_de_fazenda",
  "porcos", "coelhos", "ovelhas", "total")

for(i in variaveis){
  dados_A[[i]] <- as.numeric(dados_A[[i]])
  dados_B[[i]] <- as.numeric(dados_B[[i]])
  dados_C[[i]] <- as.numeric(dados_C[[i]])
  dados_D[[i]] <- as.numeric(dados_D[[i]])
  dados_E[[i]] <- as.numeric(dados_E[[i]])
}

# Pivotagem  para criação da variável espécie

dados <- list(dados_A,dados_B,dados_C,dados_D,dados_E)

for(i in 1: length(dados)){
  dados[[i]] <- pivot_longer(dados[[i]], cols = c("outras_especies","gatos","caes",
                          "cavia_p", "hamsters",
                          "primatas_nao_humanos",
                          "animais_de_fazenda", "porcos",
                          "coelhos", "ovelhas"), 
                 names_to = "especie" , values_to = "n_animais")
}
dados_A <- as.data.frame(dados[[1]])
dados_B <- as.data.frame(dados[[2]])
dados_C <- as.data.frame(dados[[3]])
dados_D <- as.data.frame(dados[[4]])
dados_E <- as.data.frame(dados[[5]])

# Criação de variáveis qualitativas para descrição dos dados

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

# Salvamento dos dados originais
write_csv(dados_full, file = "Dados/Processados/dados_processados.csv")
# Não utilizarei os dados_A uma vez que as informações são redundantes
# e não possuem os dados relativos a utilização, submissão a dor e tratamento

# Dados adicionais - Informações das espécies ----------------------------------

dadosAdc1 <- read_csv(file = "Dados/Brutos/dados_adicionais.csv",
                     show_col_types = F)

dadosT<- dados_full%>%
  group_by(especie)%>%
  summarise(Nanimais = sum(n_animais))

dadosT <- left_join(dados_full, dadosAdc1, by = "especie")

# Salvamento
write_csv(dadosT, file = "Dados/Processados/dados_processados_adc1.csv")

# Dados adicionais - Informações dos estados -----------------------------------

dadosJSON <- fromJSON("Dados/Brutos/us-colleges-and-universities.json")
nome <- dadosJSON$results$name
estado <- dadosJSON$state
dadosAdc2 <- data.frame(nome, estado)

dadosAdc2%<>%
  group_by(estado)%>%
  summarise(NUniversidades = n())

dadosProc <- dados_full%>%
  group_by(estado)%>%
  summarise(Nanimais = sum(n_animais))

dadosT <- left_join(dadosProc, dadosAdc2, by = "estado")

# dadosT <- left_join(dadosT, dados_latlongUS, by = "estado")

# Salvamento

write_csv(dadosT, file = "Dados/Processados/dados_processados_adc2.csv")

# Dados adicionais  - Latitude e longitude dos EUA -----------------------------

dadosJSON <- fromJSON("Dados/Brutos/mapa/mapa.json")
dadosJSON <- dadosJSON$results

nome <- dadosJSON$name
longitude <- dadosJSON$centlon
latitude <- dadosJSON$centlat
estado <- dadosJSON$stusab
dados_latlongUS <- data.frame(estado,latitude,longitude)
write_csv(dados_latlongUS, file = "Dados/Processados/USA_latlong.csv")
