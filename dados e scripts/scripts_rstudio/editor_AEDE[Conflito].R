rm(list=ls())

library(spdep)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(wesanderson)
library(ggrepel)

# diretÓrio - Dell: rauld; HP: user
setwd("C:\\Users\\rauld\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output")
dir() # carregando a tabela principal
dados <- read_rds("C:\\Users\\rauld\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output\\tab_FINAL.rds")
glimpse(dados)

# Tabela (subset dados) :::::::::::::::::::::::::::::::::::::::::::::::::::
dados <- dados %>%
  transmute(ano          = ano,
            distrito     = distrito,
            dpol         = distrito_num,
            seccional    = seccional,
            homicidio    = homicidio,
            populacao    = populacao,
            i_moran      = rep(NA, nrow(dados)),
            local_moran  = rep(NA, nrow(dados)),
            pval_lcmoran = rep(NA, nrow(dados))
            )

# taxa de homicídio
dados$tx_homicidio <- (dados$homicidio/dados$populacao)*100000

# um obj tibble para cada ano
dados2003 <- dados %>% filter(ano=="2003") %>% arrange(dpol)
dados2013 <- dados %>% filter(ano=="2013") %>% arrange(dpol)


# Matriz W ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# diretório
queen <- read.table("plan_queen.txt", h=T)
glimpse(queen) # 80x80

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
summary(listw)

# guardando lag_tx_homicidio na tabela :::::::::::::::::::::::::::::::::::::::::::::::
dados2003$lag_tx_homicidio <- lag.listw(listw, dados2003$tx_homicidio)
dados2013$lag_tx_homicidio <- lag.listw(listw, dados2013$tx_homicidio)

# valores padronizados
dados2003$tx_homicidio_z <- scale(dados2003$tx_homicidio, center = TRUE, scale = TRUE)
dados2013$tx_homicidio_z <- scale(dados2013$tx_homicidio, center = TRUE, scale = TRUE)

dados2003$lag_tx_homicidio_z <- lag.listw(listw, dados2003$tx_homicidio_z)
dados2013$lag_tx_homicidio_z <- lag.listw(listw, dados2013$tx_homicidio_z)

dados <- dados %>% arrange(ano, dpol) %>% 
  mutate(tx_homicidio_z=combine(dados2003$tx_homicidio_z,
                         dados2013$tx_homicidio_z),
         lag_tx_homicidio_z=combine(dados2003$lag_tx_homicidio_z,
                             dados2013$lag_tx_homicidio_z),
         lag_tx_homicidio=combine(dados2003$lag_tx_homicidio,
                             dados2013$lag_tx_homicidio))
glimpse(dados)

# Moran test ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
moran_03 <- moran.mc(dados2003$tx_homicidio, listw=listw, nsim=999)
moran_13 <- moran.mc(dados2013$tx_homicidio, listw=listw, nsim=999)

# simulação MC, veja a distr. dos resíduos
hist(moran_03$res, breaks = 50)
hist(moran_13$res, breaks = 50)

# guarda resultados nas tabelas
dados2003$i_moran <- rep(moran_03$statistic, nrow(dados2003))
dados2013$i_moran <- rep(moran_13$statistic, nrow(dados2013))

dados <- dados %>% arrange(ano) %>%
  mutate(i_moran=combine(dados2003$i_moran,
                         dados2013$i_moran)
         )


# Moran.plot modo convencional ::::::::::::::::::::::::::::::::::::::::::::::
# função spdep::moran.plot
moran.plot(as.vector(dados2003$tx_homicidio_z),
           listw,
           zero.policy=T,
           spChk = NULL,
           main  = "I de Moran - Taxa de tx_homicidioídios 2003",
           xlab  = "Taxa de tx_homicidioídios",
           ylab  = "Taxa de tx_homicidioídios defasada",
           labels = as.character(dados2003$distrito),
           quiet = NULL)


moran.plot(as.vector(dados2013$tx_homicidio_z),
           listw,
           zero.policy=T,
           spChk = NULL,
           main  = "I de Moran - Taxa de tx_homicidioídios 2013",
           xlab  = "Taxa de tx_homicidioídios",
           ylab  = "Taxa de tx_homicidioídios defasada",
           labels = as.character(dados2003$distrito),
           quiet = NULL)

# Moran Local :::::::::::::::::::::::::::::::::::::::::::::::::::::
# tabelas de valores
lcm03 <- localmoran(dados2003$tx_homicidio, listw = listw)
lcm03 <- as_data_frame(lcm03)
lcm13 <- localmoran(dados2013$tx_homicidio, listw = listw)
lcm13 <- as_data_frame(lcm13)

# guardando na tabela
dados2003 <- dados2003 %>% arrange(dpol) %>% 
  mutate(local_moran=round(lcm03$Ii,3),
         pval_lcmoran=round(lcm03$`Pr(z > 0)`,3))

dados2013 <- dados2013 %>% arrange(dpol) %>% 
  mutate(local_moran=round(lcm13$Ii,3),
         pval_lcmoran=round(lcm13$`Pr(z > 0)`,3))

dados <- dados %>% arrange(ano, dpol) %>%
  mutate(local_moran  = as.numeric(combine(dados2003$local_moran,
                             dados2013$local_moran)),
         pval_lcmoran = as.numeric(combine(dados2003$pval_lcmoran,
                              dados2013$pval_lcmoran))
         )
glimpse(dados)

# identify the Local Moran plot quadrant for each observation this is some
# serious slicing and illustrate the power of the bracket
dados$quad_sig <- NA
dados[(dados$tx_homicidio_z >= 0 & dados$lag_tx_homicidio_z >= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Alto-alto"
dados[(dados$tx_homicidio_z <= 0 & dados$lag_tx_homicidio_z <= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Baixo-baixo"
dados[(dados$tx_homicidio_z >= 0 & dados$lag_tx_homicidio_z <= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Alto-baixo"
dados[(dados$tx_homicidio_z <= 0 & dados$lag_tx_homicidio_z >= 0) & (dados$pval_lcmoran <= 0.05), "quad_sig"] <- "Baixo-alto"
dados[(dados$pval_lcmoran > 0.05), "quad_sig"] <- "Não sig."


# Moran ggplot2 ::::::::::::::::::::::::::::::::::::::::::::::::::::::
#2003 #filter(dados, ano==2003)
moran2003 <-
ggplot(filter(dados, ano==2003), aes(x=tx_homicidio_z, y=lag_tx_homicidio_z)) + 
  geom_point(aes(color=as.factor(quad_sig)),
             shape=21,
             fill = "white",
             size = 2,
             stroke = 1.5) +
  geom_rug(aes(color=as.factor(quad_sig))) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "2003",
       x = "Taxa de tx_homicidioídios",
       y = "Lag - taxa de tx_homicidioídio",
       color = "I de Moran Local (p-valor<0,05)") +
  scale_y_continuous(limits = c(-2,2), breaks=seq(-2,2, by=.5)) +
  scale_x_continuous(limits = c(-8,8), breaks=seq(-8,8, by=2)) +
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling")) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope=0.1944380, #coef(lm(dados2003$lag_tx_homicidio_z~dados2003$tx_homicidio_z))
              intercept=.0124981) +
  geom_text_repel(data=subset(dados, ano == "2003" & quad_sig == "Alto-alto" | ano == "2003" & quad_sig == "Baixo-baixo"),
                  aes(label=distrito),
                  size = 3)+
  geom_label( label = "Alto-alto", x = 7, y = 2, size = 3, colour = "black") +
  geom_label( label = "Alto-baixo", x = 7, y = -2, size = 3, colour = "black") +
  geom_label( label = "Baixo-baixo", x = -7, y = -2, size = 3, colour = "black") +
  geom_label( label = "Baixo-alto", x = -7, y = 2, size = 3, colour = "black")


#2013
moran2013 <-
ggplot(filter(dados, ano==2013), aes(x=tx_homicidio_z, y=lag_tx_homicidio_z)) + 
  geom_point(aes(color=as.factor(quad_sig)),
             shape=21,
             fill = "white",
             size = 2,
             stroke = 1.5) +
  geom_rug(aes(color=as.factor(quad_sig))) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = .5),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "2013",
       x = "Taxa de tx_homicidioídios",
       color = "I de Moran Local (p-valor<0,05)") +
  scale_y_continuous(limits = c(-2,2), breaks=seq(-2,2, by=.5)) +
  scale_x_continuous(limits = c(-8,8), breaks=seq(-8,8, by=2)) +
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling")) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope=0.15693795, #coef(lm(dados2003$lag_tx_homicidio_z~dados2003$tx_homicidio_z))
              intercept=0.02133708) +
  geom_text_repel(data=subset(dados, ano == "2013" & quad_sig == "Alto-alto"),
                  aes(label=distrito),
                  size = 3)+
  geom_label(label = "Alto-alto", x = 7, y = 2, size = 3, colour = "black") +
  geom_label(label = "Alto-baixo", x = 7, y = -2, size = 3, colour = "black") +
  geom_label(label = "Baixo-baixo", x = -7, y = -2, size = 3, colour = "black") +
  geom_label(label = "Baixo-alto", x = -7, y = 2, size = 3, colour = "black")

# enquadrando gráficos
fig <- ggarrange(moran2003,moran2013,
                 ncol=2, nrow=1, # align="hv",
                 common.legend = TRUE, legend = "top")

fig_completa <- annotate_figure(fig, ######
              top    = text_grob("Figura: Diagramas de dispersão de Moran (2003/2013) \n Índice global e local",
                          color  = "black",
                          face   = "bold",
                          size   = 14),
              bottom = text_grob("Fonte: Elaboração própria a partir de dados da SSP/SP",
                          color  = "black",
                          face  = "italic",
                          size  = 10),
                          left    = NA,
                          right   = NA,
                          fig.lab = NA, fig.lab.face = NA
                       )
fig_completa


