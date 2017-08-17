rm(list=ls())
library(systemfit)

## Busca no diret?rio
setwd("C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts")
dir()
txt <- read.table("txt_FINAL.txt", h=T)
is.data.frame(txt) # TRUE

# rodar o arquivo "1_?NDICE.R
#                            >normalizando vari?veis"
# Y
txt$homic
txt$roubovlc
txt$furtovlc

# X
txt$jov1524
txt$baixopadrao
txt$rendamedia
txt$dprendamedia
txt$favela
txt$eformais
txt$mandato

# outras
txt$prisoes
txt$flagrante
txt$DENS

bye()

# subset do per?odo (Q1) desejado
txt2013 <- subset(txt, Q1>2003)
txt2003 <- subset(txt, Q1<2013)

# criando sistema do tipo (Y ~ X1 + X2)
library(MASS)
eq1 <- lm(txt2003$homic ~ txt2003$jov1524 + txt2003$baixopadrao + txt2003$rendamedia +
  txt2003$dprendamedia + txt2003$favela + txt2003$eformais + txt2003$mandato)

eq2 <- lm(txt2013$homic ~ txt2013$jov1524 + txt2013$baixopadrao + txt2013$rendamedia +
  txt2013$dprendamedia + txt2013$favela + txt2013$eformais + txt2013$mandato)

system <- list(eq1 = eq1, eq2 = eq2)

# Modelo SUR
sur <- systemfit(system, method = "SUR", data = txt2003)
summary(sur) # exibe resultados
