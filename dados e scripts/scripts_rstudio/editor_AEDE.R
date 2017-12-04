rm(list=ls())

library(maptools)
library(rgdal)
library(spdep)
library(classInt)
library(RColorBrewer)
library(tidyverse)
library(ggrepel)

# diretÓrio
setwd("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output")
dir() # carregando a tabela principal
dados <- read_rds("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output\\tab_FINAL")


# Tabela (subset dados) :::::::::::::::::::::::::::::::::::::::::::::::::::
dados <- data_frame(ano          = as.integer(dados$Q1),
                distrito     = as.character(gsub(
                  dados$distrito, pattern="_", replacement=" ")),
                dpol         = as.integer(dados$Q12),
                seccional    = as.character(dados$Seccional),
                homic        = as.numeric((dados$Q22/dados$P)*100000),
                i_moran      = rep(NA, nrow(dados)),
                local_moran  = rep(NA, nrow(dados)),
                pval_lcmoran = rep(NA, nrow(dados))
                )


# um obj tibble para cada ano
dados2003 <- dados %>% filter(ano=="2003") %>% arrange(dpol)
dados2013 <- dados %>% filter(ano=="2013") %>% arrange(dpol)


# Matriz W ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# diretório
queen <- read.table("plan_queen.txt", h=T)
dim(queen) # 80x80

# matriz esparsa
queen <- as.matrix(queen) # matriz feita à mão
is.matrix(queen)

# Nomeando colunas e linhas com o num. do istrito_pol correspondente
distrito_pol <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
        "16","17","21","22","23","24","25","27","28","29","31","33","34",
        "36","37","38","40","41","42","44","46","47","48","50","51","53",
        "54","55","56","58","59","62","63","65","66","67","68","70","74",
        "75","77","78","81","87","89","91","92","93","96","98","99","100",
        "102","103","1857","2073","3052","3264","3597","4380","4572","4969",
        "85101","193990","268395")
colnames(queen) <- distrito_pol
rownames(queen) <- distrito_pol
isSymmetric(queen) # pergunta se queen é simetrica

# objeto 'listw', style="W" (padronizada na linha)
w     <- mat2listw(queen, row.names = NULL, style="M")
listw <- nb2listw(w$neighbours, glist=NULL, style="W", zero.policy=NULL) 

# guardando lag_homic na tabela :::::::::::::::::::::::::::::::::::::::::::::::
dados2003$lag_homic <- lag.listw(listw, dados2003$homic)
dados2013$lag_homic <- lag.listw(listw, dados2013$homic)

# valores padronizados
dados2003$homic_z <- scale(dados2003$homic, center = TRUE, scale = TRUE)
dados2013$homic_z <- scale(dados2013$homic, center = TRUE, scale = TRUE)

dados2003$lag_homic_z <- lag.listw(listw, dados2003$homic_z)
dados2013$lag_homic_z <- lag.listw(listw, dados2013$homic_z)

dados <- dados %>% arrange(ano, dpol) %>% 
  mutate(homic_z=combine(dados2003$homic_z,
                         dados2013$homic_z),
         lag_homic_z=combine(dados2003$lag_homic_z,
                             dados2013$lag_homic_z),
         lag_homic=combine(dados2003$lag_homic,
                             dados2013$lag_homic))

# Moran test ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
moran_03 <- moran.mc(dados2003$homic, listw=listw, nsim=999) # simulação MC
moran_13 <- moran.mc(dados2013$homic, listw=listw, nsim=999)

# guarda resultados nas tabelas

dados2003$i_moran <- rep(moran_03$statistic, nrow(dados2003))
dados2013$i_moran <- rep(moran_13$statistic, nrow(dados2013))

dados <- dados %>% arrange(ano) %>%
  mutate(i_moran=combine(dados2003$i_moran,
                         dados2013$i_moran)
         )

hist(moran_03$res, breaks = 50)
hist(moran_13$res, breaks = 50)

# Moran.plot modo convencional ::::::::::::::::::::::::::::::::::::::::::::::
# função moran.plot
moran.plot(as.vector(dados2003$homic_z),
           w,
           zero.policy=T,
           spChk = NULL,
           main  = "I de Moran - Taxa de Homicídios 2003",
           xlab  = "Taxa de Homicídios",
           ylab  = "Taxa de homicídios defasada",
           labels = as.character(dados2003$distrito),
           quiet = NULL)


moran.plot(as.vector(dados2013$homic_z),
           w,
           zero.policy=T,
           spChk = NULL,
           main  = "I de Moran - Taxa de Homicídios 2003",
           xlab  = "Taxa de Homicídios",
           ylab  = "Taxa de homicídios defasada",
           labels = as.character(dados2003$distrito),
           quiet = NULL)





# Moran Local :::::::::::::::::::::::::::::::::::::::::::::::::::::
# tabelas de valores
lcm <- localmoran(dados2003$homic, listw = listw)
lcm <- as_data_frame(lcm)

# guardando na tabela
dados2003 <- dados2003 %>% arrange(dpol) %>% 
  mutate(local_moran=round(lcm$Ii,3),
         pval_lcmoran=round(lcm$`Pr(z > 0)`,3))

dados2013 <- dados2013 %>% arrange(dpol) %>% 
  mutate(local_moran=round(lcm$Ii,3),
         pval_lcmoran=round(lcm$`Pr(z > 0)`,3))

dados <- dados %>% arrange(ano, dpol) %>%
  mutate(local_moran  = as.numeric(combine(dados2003$local_moran,
                             dados2013$local_moran)),
         pval_lcmoran = as.numeric(combine(dados2003$pval_lcmoran,
                              dados2013$pval_lcmoran))
         )

# identify the Moran plot quadrant for each observation this is some
# serious slicing and illustrate the power of the bracket
dados$quad_sig <- NA
dados[(dados$homic_z >= 0 & dados$lag_homic_z >= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Alto-alto"
dados[(dados$homic_z <= 0 & dados$lag_homic_z <= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Baixo-baixo"
dados[(dados$homic_z >= 0 & dados$lag_homic_z <= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Alto-baixo"
dados[(dados$homic_z <= 0 & dados$lag_homic_z >= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Baixo-alto"
dados[(dados$pval_lcmoran > 0.05), "quad_sig"] <- "Não sig."



# Moran ggplot2 ::::::::::::::::::::::::::::::::::::::::::::::::::::::
ggplot(filter(dados, ano==2003), aes(x=homic_z, y=lag_homic_z)) + 
  geom_point(aes(color=as.factor(quad_sig))) +
  geom_smooth(method=lm, se=FALSE)+ geom_rug(aes(color=as.factor(quad_sig))) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) 

  
