rm(list=ls())
library(tidyverse)
library(spdep)


# diretÓrio #########
setwd("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output")
dir()
dados <- read_rds("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output\\tab_FINAL")

# Tabela mãe #########
dados <- tibble(ano          = as.integer(dados$Q1),
                distrito     = as.character(gsub(
                  dados$distrito, pattern="_", replacement=" ")),
                dpol         = as.integer(dados$Q12),
                seccional    = as.character(dados$Seccional),
                homic        = as.numeric((dados$Q22/dados$P)*100000),
                roubovcl     = as.numeric((dados$Q40/dados$P)*100000),
                furtovcl     = as.numeric((dados$Q43/dados$P)*100000),
                jov1524      = as.numeric(dados$PMJ_ONU/dados$P),
                baixopadrao  = as.numeric(dados$AC_ResBP/1000),
                rendamedia   = as.numeric(dados$RM_DOM),
                dprendamedia = as.numeric(dados$S_RM_DOM),
                favela       = as.numeric((dados$fvl/dados$P)*100000),
                eformais     = as.numeric(dados$EF_P),
                mandato      = as.numeric((dados$Q76/dados$P)*100000),
                flagrante    = as.numeric((dados$Q74/dados$P)*100000),
                densidade    = as.numeric(dados$P/dados$Km2)
                )
dados2003 <- dados %>% filter(ano=="2003") %>% arrange(dpol)

# Matriz W --------------------------------

# diretório
dir()
queen<-read.table("plan_queen.txt", h=T)
dim(queen) # 80x80

# matriz esparsa
queen<-as.matrix(queen)
is.matrix(queen)

# Nomeando colunas e linhas com o num do DPol correspondente.
DPol<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","21","22","23","24","25","27","28","29","31","33","34","36","37","38","40","41","42","44","46","47","48","50","51","53","54","55","56","58","59","62","63","65","66","67","68","70","74","75","77","78","81","87","89","91","92","93","96","98","99","100","102","103","1857","2073","3052","3264","3597","4380","4572","4969","85101","193990","268395")
colnames(queen)<-DPol
rownames(queen)<-DPol
isSymmetric(queen) # pergunta se queen ? simetrica
dim(queen)

# objeto 'listw', style="W" (padronizada na linha)
w <- mat2listw(queen, row.names = NULL, style="M")
listw <- nb2listw(w$neighbours, glist=NULL, style="W", zero.policy=NULL) 

# centra na média
for(i in 1:160) {
  dados$homic[i] <- print(dados$homic[i])+0.000001
    }

dados2003$homicz <- scale(dados2003$homic, center = TRUE, scale = TRUE)
moran.plot(as.vector(dados2003$homic),
           w,
           zero.policy=T,
           spChk=NULL,
           main="I de Moran - Taxa de Homicídios 2003",
           xlab="Taxa de Homicídios",
           ylab="lag Taxa de homicídios",
           quiet=NULL)
