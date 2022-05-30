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
library(stargazer)
theme_set(theme_void()) 

#carregar base
#nome dos municipios
municipio <- read_excel("base/RAIS_estabelecimento_layout2018e2019.xlsx",
                        sheet = "municipio", 
                        col_types = c("numeric","text", "text", "skip", "skip", "skip","skip", "skip", "skip", "skip", "skip","skip", "skip", "skip"))

mun <- read_excel("base/municipios.xlsx", 
                                col_types = c("numeric", "text", "numeric", 
                                              "text", "numeric", "text", "text", 
                                              "text", "text", "text", "text", "numeric", 
                                              "text"))
mun$id <- substr(mun$`Código Município Completo`,0,6) |> as.numeric()
municipio <- inner_join(mun,municipio,by=c("id"="cod"))


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

#agora vou buscar uma tablea
tb_muni <- read_fst("base/HHImunicipio.fst")
micro <- read_excel("base/RELATORIO_DTB_BRASIL_MUNICIPIO.xls", 
                                             sheet = "DTB_2020_Municipio")
wage <-read_excel("censo/total.xlsx", col_types = c("text","numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric"))
wage$id <- substr(wage$cod,0,6) |> as.numeric()

pop <- read_excel("base/tabela.xlsx", 
                  col_types = c("numeric", "numeric", "text", 
                                "numeric", "numeric", "numeric", 
                                "text", "numeric", "text", "numeric", 
                                "text", "numeric", "numeric", "numeric"))

sexo <- read_excel("censo/sexo.xlsx", sheet = "População residente - percen...", 
                   col_types = c("numeric", "text", "text", 
                                 "skip", "numeric", "numeric", "numeric"), 
                   skip = 5)
colnames(sexo) <- c("id","municipio","idade","total","homem","mulher")

a <- sexo |>
  select(id,idade,total)
a <- spread(a,idade,total, fill= NA)

89040 / 16
df <- sexo[1,]
for (i in 1:5564) {
  df<-rbind(df,sexo[1+i*16,])
}
sex <- df
sex$id <- substr(sex$id,0,6) |> as.numeric()

idade <- read_excel("base/idade.xlsx", col_types = c("text", 
                                                     "skip", "skip", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric"))

colnames(idade) <- c("id","i0","i5","i10","i15","i18","i20","i25","i30","i35","i40","i45","i50","i55","i60","i70")
idade$id <- substr(idade$id,0,6) |> as.numeric()

educacao <- read_excel("base/educacao.xlsx", 
                       col_types = c("numeric", "skip", "skip", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "skip"), skip = 4)
colnames(educacao) <- c("id","fundamental","medio","superior")
educacao$id <- substr(educacao$id,0,6) |> as.numeric()

df <- tb_muni |> select(id=cod,hhi=`2010`)
df$loghhi <- df$hhi |> log()

df <- wage |> 
  select(Total,id,wageCLT=`Com carteira de trabalho assinada`) |>
  mutate(logwage=log(Total),logwageCLT=log(wageCLT)) |>
  inner_join(df,by="id") |>
  select(id,wage=Total,wageCLT,hhi,loghhi,logwage,logwageCLT)

df <- pop |> 
  select(id,`conta propria/total`,`CP+sCLT/total`, Total) |>
  mutate(logpop=log(Total)) |>
  inner_join(df,by="id") |>
  select(id,wage,logwage,logwageCLT,hhi,loghhi,logpop,"Conta Própria" =`conta propria/total`,"Conta Própria + sem CLT" = `CP+sCLT/total`)

df <- sex |>
  select(id,mulher) |>
  inner_join(df,by="id") |>
  select(id,wage,logwage,logwageCLT,hhi,loghhi,logpop,'Conta Própria','Conta Própria + sem CLT', mulher)


df <-inner_join(df,idade,by="id")
df <-inner_join(df,educacao,by="id")


write_fst(df,"base.fst")
