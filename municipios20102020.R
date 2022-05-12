# arquivo para serie historica

##############################
# parte inicial igual
rm(list = ls())
options(scipen = 999)
library(kableExtra)
library(tidyverse)
library(readxl)
library(fst)
library(vtable)
library(sf)
library(geobr)
library(ggpubr)
theme_set(theme_void()) 





#carregar base
#nome dos municipios
municipio <- read_excel("base/RAIS_estabelecimento_layout2018e2019.xlsx",
                        sheet = "municipio", 
                        col_types = c("numeric","text", "text", "skip", "skip", "skip","skip", "skip", "skip", "skip", "skip","skip", "skip", "skip"))
# nome das subclasses 2.0 CNAE
subclasse <- read_excel("base/RAIS_estabelecimento_layout2018e2019.xlsx",
                        sheet = "subclasse 2.0", 
                        col_types = c("text","numeric", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip"))
df <- read.table(archive::archive_read("base/2020.7z") , sep = ";", header = TRUE, dec = ",", encoding = "latin1")


# HHI para vetor
hhi_value <- function(x){
  x <- x/sum(x) 
  for (i in 1:length(x)) {x[i] <- (x[i]*(100))^2}
  sum(x)
}
# teste da funçao hhi para vetor
x <- c(1,1,1,1)
hhi_value(x)

# HHI para subset do dataframe
hhi <- function(x){
  soma <- x$Qtd.Vínculos.Ativos %>% sum()
  x %>%
    mutate(share = Qtd.Vínculos.Ativos/soma) %>%
    select(share) %>%
    "*"(100) %>%
    .^2 %>%
    sum()
}

##############################

# fazendo para municipio e subclasse
tb <- df |>
  group_by(Município, CNAE.2.0.Subclasse) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos), .groups = 'drop')
tb <- tb[!is.na(tb$HHI),]
tb |> summary()
# considerando o brasil como único mercado de trabalho e livre movimentação entre as profissões temos de HHI:
hhi(df)
tb <- "Brasil" |> as_tibble()
tb$HHI <- hhi(df)

# juntando o hhi pra cada municipio em uma unica tabela
tb_muni <- df |>
  group_by(Município) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos)) |>
  inner_join(municipio, by = c("Município" = "cod"))
muni_semHHI <- anti_join(municipio,tb_muni, by = c("cod"="Município"))
tb_muni <- tb_muni %>% select( "cod" = Município, UF, municipio, "2020" = HHI)


#fazendo a seríe histórica
#feito na mão por ano
df <- read.table(archive::archive_read(paste0("base/",2010,".7z")) , sep = ";", header = TRUE, dec = ",", encoding = "latin1")
tb <- df |>
  group_by(Município) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos)) |>
  inner_join(municipio, by = c("Município" = "cod")) |>
  select( "cod" = Município, "2010" = HHI)
tb_muni <- inner_join(tb_muni,tb, by="cod")
write_fst(tb_muni,"base/HHImunicipio.fst")

