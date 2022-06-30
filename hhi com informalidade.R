rm(list = ls())
options(scipen = 999)
basedosdados::set_billing_id("tccusp")
library(tidyverse)
library(readxl)
library(jtools)


query <- "SELECT ano, id_municipio, quantidade_vinculos_ativos FROM `basedosdados.br_me_rais.microdados_estabelecimentos` WHERE ano = 2010"
dados <- basedosdados::read_sql(query)

tabela <- read_excel("base/tabela.xlsx")

wage <-read_excel("censo/total.xlsx", col_types = c("text","numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric"))
wage$id <- substr(wage$cod,0,7) #|> as.numeric()


# HHI para vetor
hhi_value_informal <- function(x,y){
  a <- tabela[tabela$Cód. == y[1],14]
  a <- a[[1]][1] |> as.numeric()
  
  informal <- sum(x)*(a)
  total <- sum(x)*(1+a)
  hhinformal <- (100/total)^2
  
  x <- x/(total)
  for (i in 1:length(x)) {x[i] <- (x[i]*(100))^2}
  sum(x) + informal*hhinformal
}

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


tb <- dados |>
  group_by(id_municipio) |>
  summarise(HHI = hhi_value(quantidade_vinculos_ativos),
            HHI_i = hhi_value_informal(quantidade_vinculos_ativos,id_municipio)) |>
  mutate(logHHI = log(HHI),logHHIi = log(HHI_i))

df <- wage |> 
  select(Total,id_municipio = id,
         wageCLT = `Com carteira de trabalho assinada`,
         wageSclt = `Sem carteira de trabalho assinada`,
         wageCP = `Conta própria`) |>
  mutate(logwage=log(Total),logwageCLT=log(wageCLT),logwageSclt= log(wageSclt),logwageCP= log(wageCP)) |>
  inner_join(tb,by="id_municipio") |>
  select(logwage,logwageCLT,logwageSclt,logwageCP,logHHI,logHHIi)

model1 <- lm(logwage ~ logHHI , df)
model2 <- lm(logwage ~ logHHIi , df)
model3 <- lm(logwageCLT ~ logHHI , df)
model4 <- lm(logwageCLT ~ logHHIi , df)
model5 <- lm(logwageSclt ~ logHHI , df)
model6 <- lm(logwageSclt ~ logHHIi , df)
model7 <- lm(logwageCP ~ logHHI , df)
model8 <- lm(logwageCP ~ logHHIi , df)

stargazer::stargazer(model1,model2,model3,model4,model5,model6,model7,model8)


summary(df)
var(tb)
summary(tb)

plot(df$logHHIi,df$logHHI)
abline(0,1)
plot(tb$HHI_i,tb$HHI)

