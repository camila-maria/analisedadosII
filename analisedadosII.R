#AULA 01 =======================================================================

#Pacotes
library(readr)

#read_csv("https://raw.githubusercontent.com/williamorim/brasileirao/master/data-raw/csv/matches.csv")

# É melhor baixar o arquivo e não depender do site:

download.file(
  url= "https://raw.githubusercontent.com/williamorim/brasileirao/master/data-raw/csv/matches.csv",
  destfile = "data-raw/csv/brasileirao.csv"
)

brasileirao <- read.csv("data-raw/csv/brasileirao.csv")

# Com o código de baixo o nome do arquivo é salvo com a data da última alteração
#brasileirao <- paste0("data-raw/csv/brasileirao_", Sys.time(), ".csv")

write_rds(brasileirao, "data/brasileirao.rds")

# O data-raw é tipo a primeira fase da faxina
# A gente baixa os dados e deixa tidy e baixa essa nova
# tabela tidy e faz a análise em outra pasta

# AULA 02 ======================================================================

# Substituir NAs das variáveis por algum texto

library(dados)
library(tidyverse)

casas_sem_na <- casas %>%           # no banco casas
  mutate(                           # criar uma variável
    across(                         # aplicar em colunas
      .cols = where(is.character),  # nas quais é charact.
      .fns = tidyr::replace_na,     # com a função subst. NA
      replace = "Sem informação"    # por "Sem informação"
    )
  )


# Criar uma função para soma de Nas:

conta_na <- function(x) {sum(is.na(x))}

conta_na(c(1,2,NA,3,NA,10)) # Quando é na é 1 se é número é 0

casas_sem_na %>%                    #pega a base
  summarise(                        #resume
    across(                         #as colunas
      .cols = where(is.character),  #que são charact.
      .fns = conta_na               # com a função conta_na
    )                               # jeito top: ~sum(is.na(.x)),
  )                                 # cria a função dentro do across

# Confirmei que não tem NAs pq agora td é "Sem informação"
# Eu queria ver o banco de dados longo

casas_sem_na %>%                    #pega a base
  summarise(                        #resume
    across(                         #as colunas
      .cols = where(is.character),  #que são charact.
      .fns = conta_na               # com a função conta_na
    )                               # jeito top: ~sum(is.na(.x)),
  )%>%
  tidyr::pivot_longer(              #aplique pivot_longer()
    cols = everything(),            #em todas as colunas
    names_to = "nome_da_coluna",    #para a nova coluna ser nome_da_coluna
    values_to = "num_nas"           #com valores do número de nas
  )%>%
  filter(num_nas>0)                 #filtre nas>0 => nada, ou seja, a função deu bom!


# Usando função anônima no ex. de cima:

# across(
#.cols = where(is.character),
#.fns = ~sum(is.na(x))
#)

# Quando utilizar a função várias vezes no script criar a função antes,
# porque se quiser mudar algo, muda 1x lá em cima
# Quando for usar só uma vez usar a anônima

# ctr+schift+r cria seções

# Nova análise: filmes

# Gráfico de dispersão do lucro por variável numérica

imdb <- readr::read_rds("data/imdb.rds")

library(ggplot2)

imdb %>%
  mutate(lucro= receita - orcamento) %>%
  select(where(is.numeric))%>%
  tidyr::pivot_longer(
    cols = - lucro,                         # todas menos o lucro
    names_to = "variavel",
    values_to = "valores"
  )%>%
  ggplot()+
  geom_point(aes(x=valores, y=lucro))+
  facet_wrap(vars(variavel), scales="free")

# Voltando para o brasileirao

# não sei pq não rodou, mas era para criar uma nova pasta com rascunhos
# nesta pasta seria para ter só arquivos temporários para conhecer os dados
# usethis::use_data(temp_grafico_mandantes, overwrite=TRUE)

# Queremos comparar o número de gols
# vamos ver se todas as linhas tem número x número

brasileirao%>%
  mutate(
    tem_x = stringr::str_detect(score, "x")
  )%>% filter(tem_x ==FALSE)

# Ok, todos tem resultado
# Vamos separar

brasileirao%>%
  separate(
    col = "score",
    into = c("gols_mandante", "gols_visitante"),
    sep = "x",
    remove=FALSE,
    convert=TRUE
  )%>%    # poderia colocar no lugar de convert:
  #mutate(
   # across(
    #  .cols=starts_with("gols"),
     # .fns = as.numeric
    #)
  #)
 mutate(
     vitoria_mandante = gols_mandante > gols_visitante
   )%>%view()

# Pronto, agora eu sei se o mandandante ganhou ou não,
# mandante é quem joga na casa
# Ctr+schift+C => atalho para comentário e código(?)
# Para tirar acento:
# stringi:: stri_trans_general()



