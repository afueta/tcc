rm(list = ls())
options(scipen = 999)
basedosdados::set_billing_id("tccusp")
library(tidyverse)
library(readxl)
library(jtools)



query <- "SELECT * FROM `basedosdados.br_me_rais.dicionario`"
dicionario <- basedosdados::read_sql(query)

query <- "SELECT ano, id_municipio, quantidade_vinculos_ativos FROM `basedosdados.br_me_rais.microdados_estabelecimentos` WHERE ano = 2010"
dados <- basedosdados::read_sql(query)

query <- "SELECT * FROM `basedosdados.br_bd_diretorios_brasil.municipio`"
dic_muni <- basedosdados::read_sql(query)

total <- read_excel("censo/total.xlsx")
total$cod <- total$cod |> as.character()
wage <- total |>
  select(id_municipio=cod,Total,wageCLT = `Com carteira de trabalho assinada`) |>
  mutate(logwage=log(Total),logwageCLT=log(wageCLT))

# HHI para vetor
hhi_value <- function(x){
  x <- x/sum(x) 
  for (i in 1:length(x)) {x[i] <- (x[i]*(100))^2}
  sum(x)
}
# HHI para subset do dataframe
hhi <- function(x){
  soma <- x$quantidade_vinculos_ativos %>% sum()
  x %>%
    mutate(share = quantidade_vinculos_ativos/soma) %>%
    select(share) %>%
    "*"(100) %>%
    .^2 %>%
    sum()
}

df <- dados |>
  inner_join(dic_muni, by="id_municipio")


tb_muni <- df |>
  group_by(id_municipio) |>
  summarise(HHI = hhi_value(quantidade_vinculos_ativos)) |>
  mutate(loghhi = log(HHI))
tb <- tb_muni |> inner_join(wage, by="id_municipio")
model1 <- lm(logwageCLT ~ loghhi , tb)


tb_microrregiao <- df |>
  group_by(id_microrregiao) |>
  summarise(HHI = hhi_value(quantidade_vinculos_ativos)) |>
  mutate(loghhi = log(HHI))
tb <- inner_join(tb_microrregiao, wage, by=c("id_microrregiao"="id_municipio"))
model2 <- lm(logwageCLT ~ loghhi , tb)


tb_mesoregiao <- df |>
  group_by(id_mesorregiao) |>
  summarise(HHI = hhi_value(quantidade_vinculos_ativos)) |>
  mutate(loghhi = log(HHI))
tb <- tb_mesoregiao |> inner_join(wage, by=c("id_mesorregiao"="id_municipio"))
model3 <- lm(logwageCLT ~ loghhi , tb)

plot_summs(model1, model2, model3, model.names = c("Município", "Microrregião", "Mesoregião"))

