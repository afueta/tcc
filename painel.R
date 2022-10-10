rm(list = ls())

#fazer todas as análises para os anos de Censo
options(scipen = 999)
basedosdados::set_billing_id("tccusp")
library(tidyverse)
library(readxl)
library(fst)
library(basedosdados)
theme_set(theme_void()) 


df <- read_fst("base.fst")

variaveis <- colnames(df)

query <- paste0('SELECT id_municipio, v0301, v3072, v0328, v0349, v3561 ',
                'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1991` ')
dados <- basedosdados::read_sql(query)
#fazer o primeiro antes
#novodf <- vetor_municipio
# para 
a <- df$id_municipio[1]

for (i in a) {
  
  
  # query <- paste0('SELECT v0301, v3072, v0328, v0349, v3561 ',
  #                 'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1991` ',
  #                 'WHERE id_municipio in ("',i,'")')
  # df_municipio <- basedosdados::read_sql(query)
  
  df_municipio <- subset(dados, dados$id_municipio == i)
  
  
  
  
  #fazendo a primeira entrada
  
  ## percentual de mulheres no municipio e gerando o vetor com o id_municipio
  ## mundando o 2 de mulher para 0
  df_municipio$v0301[df_municipio$v0301 == 2] = 0
  vetor_municipio <- df_municipio |> summarise(ano = 1991,id_municipio = i ,
                                               mulher = 1 - mean(as.numeric(v0301)))
  
  
  # percentual de populacao por idade de 5 em 5
  b <- df_municipio$v3072 |> as.numeric() |> hist(breaks = seq(0,140,5))
  
  c <- paste0("i",b$breaks[-1]) |> as_tibble()
  c$valor <- b$counts/sum(b$counts)
  ## colando com o vetor do municipio
  ## "mutate" é pra colocar em ordem
  ## "spread" pra transpor em vetor com colnames certos
  vetor_municipio <- c |>
    mutate(value = factor(value, levels = unique(value))) |>
    spread(value,valor) %>% 
    cbind(vetor_municipio,.)
  
  
  # Escolaridade
  b <- df_municipio$v0328 |> as.integer() |> hist(seq(0,9), right = F)
  
  vetor_municipio <- b$density |> as_tibble()  %>% summarise(nenhum = value[1],
                                                             fundamental = value[3]+value[4]+value[5],
                                                             medio = value[5]+value[6],
                                                             superior = value[8]+value[9])  %>% cbind(vetor_municipio,.)
  # *** Categorias ***
  #   Código  Descrição
  # 0   Nenhum
  # 1   Curso de alfabetização de adultos
  # 2   Primário ou elementar
  # 3   Ginasial ou médio 1o ciclo
  # 4   1o grau
  # 5   2o grau
  # 6   Colegial ou médio 2o ciclo
  # 7   Superior
  # 8   Mestrado ou Doutorado
  
  #posicao na ocupacao
  
  
  b <- df_municipio$v0349 |> na.omit() |> as.numeric()
  b <- b[b!= 11] |> hist(seq(0,11), right = F)
  
  # *** Categorias ***
  #   Código  Descrição
  # 01  Trabalhador agrícola volante
  # 02  Parceiro ou Meeiro - Empregado
  # 03  Parceiro ou Meeiro - Autônomo ou Conta própria
  # 04  Trabalhador Doméstico - Empregado
  # 05  Trabalhador Doméstico - Autônomo ou Conta própria
  # 06  Empregado do Setor Privado
  # 07  Empregado do Setor Público - Servidor Público
  # 08  Empregado do Setor Público - de Empresa Estatal
  # 09  Autônomo ou conta-própria
  # 10  Empregador
  # 11  Sem remuneração
  # branco   Pessoas com menos de 10 anos
  
  vetor_municipio <- b$density |> as_tibble()  %>% 
    summarise(informal = value[1+3]+value[1+5]+value[1+9]) %>% 
    cbind(vetor_municipio,.)
  
  
  
  #rendimento nominal total
  b <- df_municipio$v3561 |> na.omit() |> as.numeric() |> as_tibble()
  b <- b[b != 0,]
  b <- b[b != 99999999,]
  
  vetor_municipio <- b |> summarise(wage = mean(value),lwage = log(wage))  %>% 
    cbind(vetor_municipio,.)
  
  #para o primeiro
  novodf <- vetor_municipio
  print(i)
}

################################################################################
a <- df$id_municipio[2:length(df$id_municipio)]

for (i in a) {
  

# query <- paste0('SELECT v0301, v3072, v0328, v0349, v3561 ',
#                 'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1991` ',
#                 'WHERE id_municipio in ("',i,'")')
# df_municipio <- basedosdados::read_sql(query)
  
df_municipio <- subset(dados, dados$id_municipio == i)




#fazendo a primeira entrada

## percentual de mulheres no municipio e gerando o vetor com o id_municipio
## mundando o 2 de mulher para 0
df_municipio$v0301[df_municipio$v0301 == 2] = 0
vetor_municipio <- df_municipio |> summarise(ano = 1991,id_municipio = i ,
                          mulher = 1 - mean(as.numeric(v0301)))


# percentual de populacao por idade de 5 em 5
b <- df_municipio$v3072 |> as.numeric() |> hist(breaks = seq(0,140,5))

c <- paste0("i",b$breaks[-1]) |> as_tibble()
c$valor <- b$counts/sum(b$counts)
## colando com o vetor do municipio
## "mutate" é pra colocar em ordem
## "spread" pra transpor em vetor com colnames certos
vetor_municipio <- c |>
  mutate(value = factor(value, levels = unique(value))) |>
  spread(value,valor) %>% 
  cbind(vetor_municipio,.)


# Escolaridade
b <- df_municipio$v0328 |> as.integer() |> hist(seq(0,9), right = F)

vetor_municipio <- b$density |> as_tibble()  %>% summarise(nenhum = value[1],
                                        fundamental = value[3]+value[4]+value[5],
                                        medio = value[5]+value[6],
                                        superior = value[8]+value[9])  %>% cbind(vetor_municipio,.)
# *** Categorias ***
#   Código  Descrição
# 0   Nenhum
# 1   Curso de alfabetização de adultos
# 2   Primário ou elementar
# 3   Ginasial ou médio 1o ciclo
# 4   1o grau
# 5   2o grau
# 6   Colegial ou médio 2o ciclo
# 7   Superior
# 8   Mestrado ou Doutorado

#posicao na ocupacao


b <- df_municipio$v0349 |> na.omit() |> as.numeric()
b <- b[b!= 11] |> hist(seq(0,11), right = F)

# *** Categorias ***
#   Código  Descrição
# 01  Trabalhador agrícola volante
# 02  Parceiro ou Meeiro - Empregado
# 03  Parceiro ou Meeiro - Autônomo ou Conta própria
# 04  Trabalhador Doméstico - Empregado
# 05  Trabalhador Doméstico - Autônomo ou Conta própria
# 06  Empregado do Setor Privado
# 07  Empregado do Setor Público - Servidor Público
# 08  Empregado do Setor Público - de Empresa Estatal
# 09  Autônomo ou conta-própria
# 10  Empregador
# 11  Sem remuneração
# branco   Pessoas com menos de 10 anos

vetor_municipio <- b$density |> as_tibble()  %>% 
  summarise(informal = value[1+3]+value[1+5]+value[1+9]) %>% 
  cbind(vetor_municipio,.)



#rendimento nominal total
b <- df_municipio$v3561 |> na.omit() |> as.numeric() |> as_tibble()
b <- b[b != 0,]
b <- b[b != 99999999,]
 
vetor_municipio <- b |> summarise(wage = mean(value),lwage = log(wage))  %>% 
  cbind(vetor_municipio,.)

#para o primeiro
#novodf <- vetor_municipio
novodf <- vetor_municipio |> rbind(novodf)
print(i)
}


################################################################################
#################################CENSO 2000#####################################
################################################################################



a <- df$id_municipio[1:length(df$id_municipio)]

query <- paste0('SELECT id_municipio, v0401, v4752, v0432, v0447, v4512 ',
                'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_2000` ')
dados <- basedosdados::read_sql(query)

for (i in a) {
  
  
  # query <- paste0('SELECT v0301, v3072, v0328, v0349, v3561 ',
  #                 'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1991` ',
  #                 'WHERE id_municipio in ("',i,'")')
  # df_municipio <- basedosdados::read_sql(query)
  df_municipio <- subset(dados, dados$id_municipio == i)
  
  
  
  
  #fazendo a primeira entrada
  
  ## percentual de mulheres no municipio e gerando o vetor com o id_municipio
  ## mundando o 2 de mulher para 0
  df_municipio$v0401[df_municipio$v0401 == 2] = 0
  vetor_municipio <- df_municipio |> summarise(ano = 2000,id_municipio = i ,
                                               mulher = 1 - mean(as.numeric(v0401)))
  
  
  # percentual de populacao por idade de 5 em 5
  b <- df_municipio$v4752 |> as.numeric() |> hist(breaks = seq(0,140,5))
  
  c <- paste0("i",b$breaks[-1]) |> as_tibble()
  c$valor <- b$counts/sum(b$counts)
  ## colando com o vetor do municipio
  ## "mutate" é pra colocar em ordem
  ## "spread" pra transpor em vetor com colnames certos
  vetor_municipio <- c |>
    mutate(value = factor(value, levels = unique(value))) |>
    spread(value,valor) %>% 
    cbind(vetor_municipio,.)
  
  
  # Escolaridade
  b <- df_municipio$v0432 |> as.integer() |> hist(seq(0,10), right = F)

  vetor_municipio <- b$density |> as_tibble()  %>% summarise(nenhum = value[10],
                                                             fundamental = value[6]+value[4]+value[3],
                                                             medio = value[7]+value[5],
                                                             superior = value[8]+value[9])  %>% cbind(vetor_municipio,.)
  # Classificação da Informação:
  # 1 – Alfabetização de adultos
  # 2 – Antigo primário
  # 3 – Antigo ginásio
  # 4 – Antigo clássico, científico, etc.
  # 5 – Ensino fundamental ou 1o grau
  # 6 – Ensino médio ou 2o Grau
  # 7 –Superior – graduação
  # 8 – Mestrado ou doutorado
  # 9 – Nenhum
  # Branco – para os estudantes
  
  #posicao na ocupacao
  
  
  b <- df_municipio$v0447 |> as.integer() |> hist(seq(0,11), right = F)
  b$counts
  df_municipio$v0447 |> table()
  # Classificação da Informação:
  # 1 - Trabalhador doméstico com carteira de trabalho assinada
  # 2 - Trabalhador doméstico sem carteira de trabalho assinada
  # 3 - Empregado com carteira de trabalho assinada
  # 4 - Empregado sem carteira de trabalho assinada
  # 5 - Empregador
  # 6 - Conta-própria
  # 7 - Aprendiz ou estagiário sem remuneração
  # 8 - Não remunerado em ajuda a membro do domicílio
  # 9 - Trabalhador na produção para o próprio consumo
  # Branco - para as pessoas com menos de 10 anos de idade e pessoas com 10 anos ou mais de idade que não tinham trabalho na semana de referência.
  
  vetor_municipio <- b$density |> as_tibble()  %>% 
    summarise(informal = value[3]+value[5]+value[7]) %>% 
    cbind(vetor_municipio,.)
  
  
  
  #rendimento nominal total
  b <- df_municipio$v4512 |> na.omit() |> as.numeric() |> as_tibble()
  b <- b[b != 0,]
  b <- b[b != 999000,]
  
  vetor_municipio <- b |> summarise(wage = mean(value),lwage = log(wage))  %>% 
    cbind(vetor_municipio,.)
  
  #para o primeiro
  #novodf <- vetor_municipio
  novodf <- vetor_municipio |> rbind(novodf)
  print(i)
}



################################################################################
#################################CENSO 2010#####################################
################################################################################



a <- df$id_municipio[1:length(df$id_municipio)]

query <- paste0('SELECT id_municipio, v0601, v6036, v0633, v0648, v6513 ',
                'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_2010` ')
dados <- basedosdados::read_sql(query)

for (i in a) {
  
  
  # query <- paste0('SELECT v0301, v3072, v0328, v0349, v3561 ',
  #                 'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1991` ',
  #                 'WHERE id_municipio in ("',i,'")')
  # df_municipio <- basedosdados::read_sql(query)
  df_municipio <- subset(dados, dados$id_municipio == i)
  

  
  
  #fazendo a primeira entrada
  
  ## percentual de mulheres no municipio e gerando o vetor com o id_municipio
  ## mundando o 2 de mulher para 0
  df_municipio$v0601[df_municipio$v0601 == 2] = 0
  vetor_municipio <- df_municipio |> summarise(ano = 2010,id_municipio = i ,
                                               mulher = 1 - mean(as.numeric(v0601)))
  
  
  # percentual de populacao por idade de 5 em 5
  b <- df_municipio$v6036 |> as.numeric() |> hist(breaks = seq(0,140,5))
  
  c <- paste0("i",b$breaks[-1]) |> as_tibble()
  c$valor <- b$counts/sum(b$counts)
  ## colando com o vetor do municipio
  ## "mutate" é pra colocar em ordem
  ## "spread" pra transpor em vetor com colnames certos
  vetor_municipio <- c |>
    mutate(value = factor(value, levels = unique(value))) |>
    spread(value,valor) %>% 
    cbind(vetor_municipio,.)
  
  
  # Escolaridade
  b <- df_municipio$v0633 |> as.integer() |> hist(seq(0,15), right = F)
  b$counts
  df_municipio$v0633 |> table()
  vetor_municipio <- b$density |> as_tibble()  %>% summarise(nenhum = value[1],
                                                             fundamental = value[5]+value[6]+value[7]+value[8]+value[9],
                                                             medio = value[10]+value[11],
                                                             superior = value[12]+value[13]+value[14]+value[15])  %>% cbind(vetor_municipio,.)
  # Classificação da Informação:
  # 01 – Creche, Pré-escolar (Maternal e Jardim de Infância), Classe de alfabetização - CA
  # 02 - Alfabetização de Jovens e Adultos 
  # 03 – Antigo Primário (Elementar)
  # 04 – Antigo Ginásio (Médio 1o Ciclo)
  # 05 – Regular do Ensino Fundamental ou 1o Grau (da 1aa 3a série/do 1o ao 4o ano)
  # 06 – Regular do Ensino Fundamental ou 1o Grau (da 4a série/5o ano)
  # 07 - Regular do Ensino Fundamental ou 1oGrau (da 5aa 8asérie/ do 6o ao 9o ano)
  # 08 - Supletivo do Ensino Fundamental ou do 1oGrau 
  # 09 - Antigo Científico, Clássico, etc. (Médio 2o ciclo)
  # 10 - Regular ou Supletivo do Ensino Médio ou do 2o Grau 
  # 11 - Superior de Graduação
  # 12 - Especialização de Nível Superior (mínimo de 360 horas) 
  # 13 – Mestrado
  # 14 - Doutorado
  
  #posicao na ocupacao
  
  
  b <- df_municipio$v0648 |> as.integer() |> hist(seq(0,11), right = F)

  # Classificação da Informação:
  # 1 – Empregado com carteira de trabalho assinada: pessoa
  # empregada contratada com carteira de trabalho assinada.
  # 2 – Militar do Exército, Marinha, Aeronáutica, Polícia Militar ou Corpo de Bombeiros: pessoa que era militar do Exército, Marinha, Aeronáutica ou das Forças Auxiliares, como Polícia Militar ou Corpo de Bombeiros, inclusive a pessoa que prestava o serviço militar obrigatório.
  # 3 – Empregado pelo Regime Jurídico dos Funcionários Públicos:
  #   pessoa que era empregada de instituição, fundação, autarquia, etc., no poder público(Executivo, Legislativo, Judiciário), desde que regido pelo Regime Jurídico dos Funcionários Públicos de qualquer instância (federal, estadual ou municipal).
  # 4 – Empregado sem carteira de trabalho assinada: pessoa empregada que não tinha carteira de trabalho assinada, não era militar das Forças Armadas ou Auxiliares e não era regida pelo Regime Jurídico dos Funcionários Públicos.
  # 5 – Conta própria: pessoa que trabalhava explorando seu próprio empreendimento, sozinha ou com sócio, sem ter empregado, ainda que contando com ajuda de trabalhador não remunerado.
  # 6 – Empregador: pessoa que trabalhava explorando o seu próprio empreendimento com, pelo menos, um empregado.
  # 7 – Não remunerado: pessoa que, na semana de referência, trabalhou sem remuner
  vetor_municipio <- b$density |> as_tibble()  %>% 
    summarise(informal = value[5]+value[6]+value[8]) %>% 
    cbind(vetor_municipio,.)
  
  
  
  #rendimento nominal total

  b <- df_municipio$v6513 |> na.omit() |> as.numeric() |> as_tibble()
  b <- b[b != 0,]
  
  vetor_municipio <- b |> summarise(wage = mean(value),lwage = log(wage))  %>% 
    cbind(vetor_municipio,.)
  
  #para o primeiro
  #novodf <- vetor_municipio
  novodf <- vetor_municipio |> rbind(novodf)
  print(i)
}


write.fst(novodf,"censo.fst")



