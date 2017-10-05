rm(list=ls())

library(tidyverse)
library(readr)
library(plm)

setwd("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output")
dir()
txt <- read_rds("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output\\tab_FINAL")

dados <- tibble(ano          = as.integer(txt$Q1),
                distrito     = as.factor(txt$Q13),
                dpol         = as.integer(txt$Q12),
                seccional    = as.factor(txt$Seccional),
                homic        = as.numeric((txt$Q22/txt$P)*100000),
                roubovcl     = as.numeric((txt$Q40/txt$P)*100000),
                furtovcl     = as.numeric((txt$Q43/txt$P)*100000),
                jov1524      = as.numeric(txt$PMJ_ONU/txt$P),
                baixopadrao  = as.numeric(txt$AC_ResBP/1000),
                rendamedia   = as.numeric(txt$RM_DOM),
                dprendamedia = as.numeric(txt$S_RM_DOM),
                favela       = as.numeric((txt$fvl/txt$P)*100000),
                eformais     = as.numeric(txt$EF_P),
                mandato      = as.numeric((txt$Q76/txt$P)*100000),
                flagrante    = as.numeric((txt$Q74/txt$P)*100000),
                densidade    = as.numeric(txt$P/txt$Km2)
                )

pdados <- pdata.frame(dados, c("dpol", "ano"))


# [barras] crime x dpol
barra <- ggplot(data = filter(pdados, seccional == "1 CENTRO"),
                aes(x=distrito, y=homic, fill=ano)) +
  geom_bar(stat="identity", position="dodge") +
  theme_classic()

# [pontos] (x, y, z)
xyz <- ggplot(data  = filter(dados, ano==2003),
              aes(x = jov1524,
                  y = homic)) +
              geom_smooth(method = "lm") +
              geom_point(aes(color = as.factor(seccional),
                             size  = eformais)) +
              theme_classic()

# [boxplot] 2003\2013
ggbox <- ggplot(data = pdados,
               aes(x = ano,
                   y = homic)) +
          geom_boxplot() +
          theme_classic()
