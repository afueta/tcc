rm(list = ls())

#fazer todas as análises para os anos de Censo
options(scipen = 999)
basedosdados::set_billing_id("tccusp")
library(tidyverse)
library(readxl)
library(fst)
theme_set(theme_void()) 


df <- read_fst("base.fst")

variaveis <- colnames(df)

a <- df$id_municipio[1]


query <- paste0('SELECT * ',
                'FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1991` ',
                'WHERE id_municipio in ("',a,'")')
df_municipio <- basedosdados::read_sql(query)





#fazendo a primeira entrada

## percentual de mulheres no municipio e gerando o vetor com o id_municipio
## mundando o 2 de mulher para 0
df_municipio$v0301[df_municipio$v0301 == 2] = 0
vetor_municipio <- df_municipio |> summarise(id_municipio = a ,
                          mulher = 1 - mean(as.numeric(v0301)))


# percentual de populacao por idade de 5 em 5
b <- df_municipio$v3072 |> as.numeric() |> hist(breaks = seq(0,90,5))

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
b <- df_municipio$v0328 |> table() |> as.data.frame()
c <- cbind(paste0("e",b$Var1)) |> as_tibble()
c$V2 <- b$Freq |> as.numeric()
c$V2 <- c$V2/sum(c$V2)
c<-c |> as_tibble() |> spread(V1,V2)
c |> sum() 
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

vetor_municipio <- c |> summarise(nenhum = e0,
               fundamental = e2+e4,
               medio = e3+e5+e6,
               superior = e7)  %>% 
  cbind(vetor_municipio,.)


#posicao na ocupacao


b <- df_municipio$v0349 |> na.omit() |> as.numeric() |> as.data.frame() %>% filter(.!= 11) |> table() |> as.data.frame()
b
c <- cbind(paste0("e",b$Var1)) |> as_tibble()
c$V2 <- b$Freq |> as.numeric()
c$V2 <- c$V2/sum(c$V2)
c<-c |> as_tibble() |> spread(V1,V2)
c |> sum() 
c
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

#rendimento nominal total
v3561


