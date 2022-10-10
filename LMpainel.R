
rm(list = ls())

#fazer todas as análises para os anos de Censo
options(scipen = 999)
basedosdados::set_billing_id("tccusp")
library(tidyverse)
library(readxl)
library(fst)
# HHI para vetor
hhi_value <- function(x){
  x <- x/sum(x) 
  for (i in 1:length(x)) {x[i] <- (x[i]*(100))^2}
  sum(x)
}

theme_set(theme_void()) 

df <- read_fst("base.fst")
variaveis <- colnames(df)

df <-read_fst("censo.fst")

query <- paste0('SELECT id_municipio, ano, quantidade_vinculos_ativos ',
                'FROM `basedosdados.br_me_rais.microdados_estabelecimentos` ',
                'WHERE ano in (2010,2000,1991)')
dados <- basedosdados::read_sql(query)

query <- paste0('SELECT id_municipio, ano, AVG(valor_remuneracao_media) ',
                'FROM `basedosdados.br_me_rais.microdados_vinculos` ',
                'WHERE ano in (2010,2000,1991)',
                'GROUP BY id_municipio, ano')
wage <- basedosdados::read_sql(query)

query <- paste0('SELECT id_municipio, ano, valor_remuneracao_media ',
                'FROM `basedosdados.br_me_rais.microdados_vinculos` ',
                'WHERE ano in (1991)')
wage <- basedosdados::read_sql(query)

summary(wage)

HHI_ano <- dados |> 
  group_by(ano,id_municipio) |> 
  summarise(HHI = hhi_value(quantidade_vinculos_ativos)) |>
  mutate(idjoin = paste0(ano,id_municipio))

df <- df |> select(id_municipio,ano,lwage,informal,
                   mulher,
                   i5,i10,i15,i20,i25,i30,i35,i40,i45,i50,i55,i60,i65,i70,
                   fundamental,medio,superior) |>
  mutate(idjoin = paste0(ano,id_municipio))
  
df <- inner_join(df,HHI_ano,by="id_municipio")
