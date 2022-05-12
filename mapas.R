# arquivo para mapas

# parte inicial igual
##############################

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
# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_line(colour = 'transparent'))

################################################################################
# tabela por ano
################################################################################
tb_muni <- read_fst("base/HHImunicipio.fst")
a <- tb_muni |> group_by(UF) |>
  summarise(media2020 = mean(`2020`))
summary(a)
a |> subset(a$media2020>1500 & a$media2020<2500) |> summary()
a |> subset(a$media2020>2500) |> summary()

a$UF <- a$UF |> toupper()
states <- read_state(year=2020)
states <- left_join(states, a, by = c("abbrev_state" = "UF"))

dados <- tb_muni$cod |> as_tibble()
dados$a2020 <- tb_muni$`2020`

muni <- read_municipality(code_muni="all", year=2020)
muni$code_muni<- substring(muni$code_muni,1,6) |> as.numeric()
muni <- inner_join(muni,dados,by = c("code_muni" = "value"))


# Plot all Brazilian states
ggplot() +
  geom_sf(data=states, aes(fill=media2020), color = "white", size=0.01) +
  # labs(subtitle="Concentration on labor market by micro region, 2020", size=8) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, name="HHI", limits = c(0,10000)) +
  theme_minimal() +
  no_axis

# plots pra serie histórica
#2010
dados <- tb_muni$cod |> as_tibble()
dados$a2010 <- tb_muni$`2010`

muni <- read_municipality(code_muni="all", year=2020)
muni$code_muni<- substring(muni$code_muni,1,6) |> as.numeric()
muni <- inner_join(muni,dados,by = c("code_muni" = "value"))

a2010<-ggplot() +
  geom_sf(data=muni, aes(fill=a2010), color = "white", size=0.01) +
  # labs(subtitle="Concentration on labor market by micro region, 2020", size=8) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, name="HHI", limits = c(0,10000)) +
  theme_minimal() +
  no_axis

#2013
dados <- tb_muni$cod |> as_tibble()
dados$a2013 <- tb_muni$`2013`

muni <- read_municipality(code_muni="all", year=2020)
muni$code_muni<- substring(muni$code_muni,1,6) |> as.numeric()
muni <- inner_join(muni,dados,by = c("code_muni" = "value"))

a2013<-ggplot() +
  geom_sf(data=muni, aes(fill=a2013), color = "white", size=0.01) +
  # labs(subtitle="Concentration on labor market by micro region, 2020", size=8) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, name="HHI", limits = c(0,10000)) +
  theme_minimal() +
  no_axis

#2016
dados <- tb_muni$cod |> as_tibble()
dados$a2016 <- tb_muni$`2016`

muni <- read_municipality(code_muni="all", year=2020)
muni$code_muni<- substring(muni$code_muni,1,6) |> as.numeric()
muni <- inner_join(muni,dados,by = c("code_muni" = "value"))

a2016<-ggplot() +
  geom_sf(data=muni, aes(fill=a2016), color = "white", size=0.01) +
  # labs(subtitle="Concentration on labor market by micro region, 2020", size=8) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, name="HHI", limits = c(0,10000)) +
  theme_minimal() +
  no_axis

ggarrange(a2010+ theme(legend.position = "none"), a2013+ theme(legend.position = "none"), a2016+ theme(legend.position = "none"),a2020, 
          labels = c("2010", "2013", "2016","2020"),
          ncol = 2, nrow = 2)
