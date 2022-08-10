rm(list = ls())

#fazer todas as análises para os anos de Censo
options(scipen = 999)
basedosdados::set_billing_id("tccusp")
library(kableExtra)
library(tidyverse)
library(readxl)
library(fst)
library(vtable)
library(sf)
library(geobr)
library(ggpubr)
theme_set(theme_void()) 


df <- read_fst("base.fst")

df$id_municipio[1]

