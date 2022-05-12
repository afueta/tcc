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

# teste para hhi como subset do dataframe
a <- df[sample(1:nrow(df), size = 100),]
hhi(a)

###################################
#calculo do HHI para cada subset
###################################
#fazendo para subclasse 2.0
tb_subclasse <- df |>
  group_by(CNAE.2.0.Subclasse) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos)) |>
  inner_join(subclasse, by = c("CNAE.2.0.Subclasse" = "cod"))
tb_subclasse <- tb_subclasse %>% select( "cod" = CNAE.2.0.Subclasse, subclasse, "2020" = HHI)
tb_subclasse <- tb_subclasse[!is.na(tb_subclasse$`2020`),]
hist(tb_subclasse$`2020`)


# fazendo hhi para cada subset e salvando em um csv pra excel
tb <- df %>%
  group_by(UF) %>%
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos))
summary(tb)
tb |> subset(tb$HHI>1500 & tb$HHI<2500) |> summary()
tb |> subset(tb$HHI>2500) |> summary()

tb <- df |>
  group_by(CNAE.2.0.Classe) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos))
summary(tb)
tb |> subset(tb$HHI>1500 & tb$HHI<2500) |> summary()
tb |> subset(tb$HHI>2500) |> summary()

tb <- df |>
  group_by(CNAE.95.Classe) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos))
tb[2] <- tb[2] %>% round()
write_excel_csv(tb,"cna95.csv")

tb <- df |>
  group_by(Município) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos))
tb[2] <- tb[2] %>% round()
write_excel_csv(tb,"muni.csv")

tb <- df |>
  group_by(CNAE.2.0.Subclasse) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos))
summary(tb)
tb |> subset(tb$HHI>1500 & tb$HHI<2500)
tb |> subset(tb$HHI>2500)

tb <- df |>
  group_by(IBGE.Subsetor) |>
  summarise(HHI = hhi_value(Qtd.Vínculos.Ativos))
tb[2] <- tb[2] %>% round()
write_excel_csv(tb,"ibgesub.csv")


# 
# b <- subset(df,df$Escolaridade.após.2005 == 7) #MEDIO COMPL
# c <- subset(b,b$IBGE.Subsetor == 24) #Administraçao pública direta e autárquica
# d <- subset(c,c$Vl.Remun.Média.Nom > 10000) #salario mais q 10000
# 
# summary(b$Vl.Remun.Média.Nom)
# summary(df$Vl.Remun.Média.Nom)
# summary(c$Vl.Remun.Média.Nom)
# 
# hist(c$Vl.Remun.Média.Nom)
# hist(d$Vl.Remun.Média.Nom)
# boxplot(c$Vl.Remun.Média.Nom)
# 
# boxplot(df$Vl.Remun.Média.Nom ~ df$Escolaridade.após.2005)
# boxplot(b$Vl.Remun.Média.Nom ~ b$Tamanho.Estabelecimento)
# boxplot(b$Vl.Remun.Média.Nom ~ b$IBGE.Subsetor)


##################################
# No final, nao precisou calcular pois os municipios não encontrados não existem mais no ano de 2020 (podem ter sido extinguidos durante a serie historia). Para fim de comparação, somente manter municipios em que se mantém no nome em todo o período analisado.
###################################

# # calculando o hhi para o Grande São Paulo
# hhi_sp <- df |>
#   subset(df$Distritos.SP != "{ñ clas" #hhi = 9.63
#          #         & df$Distritos.SP != "   9999" #hhi = 73.83
#   ) 
# a <- hhi_sp |> count(hhi_sp$Distritos.SP) |> as_tibble()
# a[97,2]/sum(a[2]) #maior que 60%
# 
# 
# hhi_sp2 <- df |>
#   subset(df$Bairros.SP != "           {ñ class}" #hhi = 1.37
#          #         & df$Bairros.SP != "                9999" #hhi = 9.03
#   )
# a <- hhi_sp2 |> count(hhi_sp2$Bairros.SP) |> as_tibble()
# a[1652,2]/sum(a[2]) #maior que 60%
# 
# #nesse caso, usarei o hhi = 9.63
# hhi_sp <- hhi(hhi_sp)
# 
# # calculando o hhi para o Grande Rio de Janeiro
# hhi_rio <- df |>
#   subset(df$Bairros.RJ != "           {ñ class}" #hhi = 13.12
#          # & df$Bairros.RJ != "                9999" #hhi = 40.53
#   )
# a <- hhi_rio |> count(hhi_rio$Bairros.RJ) |> as_tibble()
# a[166,2]/sum(a[2]) #maior que 50%
# 
# #nesse caso, usarei o hhi=13.12
# hhi_rio <- hhi(hhi_rio)
# 330455
# b <- df |> subset(df$Município == 330455) |>
#   count(Bairros.RJ)
# a == b


