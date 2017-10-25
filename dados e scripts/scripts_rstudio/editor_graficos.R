rm(list=ls())

library(tidyverse)
library(readr)
library(plm)

# diretório #########
setwd("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output")
dir()
txt <- read_rds("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output\\tab_FINAL")

# Tabela mãe #########
dados <- tibble(ano          = as.integer(txt$Q1),
                distrito     = as.character(gsub(
                               txt$distrito, pattern="_", replacement=" ")),
                dpol         = as.integer(txt$Q12),
                seccional    = as.character(txt$Seccional),
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

# subsets ################
limits2003 <- dados %>% filter(ano == 2003) %>%
  select(distrito, homic) %>% arrange(desc(homic)) %>% select(distrito)

limits2013 <- dados %>% filter(ano == 2013) %>%
  select(distrito, homic) %>% arrange(desc(homic)) %>% select(distrito)

seccional <- dados %>% select(seccional) %>% distinct(seccional) %>% arrange(seccional)

limits_1 <- dados %>% filter(ano == 2003 & seccional == "1 CENTRO") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)

limits_2 <- dados %>% filter(ano == 2003 & seccional == "2 SUL") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)

limits_3 <- dados %>% filter(ano == 2003 & seccional == "3 OESTE") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)

limits_4 <- dados %>% filter(ano == 2003 & seccional == "4 OESTE") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)

limits_5 <- dados %>% filter(ano == 2003 & seccional == "5 LESTE") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)

limits_6 <- dados %>% filter(ano == 2003 & seccional == "6 SANTO AMARO") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)

limits_7 <- dados %>% filter(ano == 2003 & seccional == "7 ITAQUERA") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)

limits_8 <- dados %>% filter(ano == 2003 & seccional == "8 SÃO MATEUS") %>% 
  select(distrito, homic) %>% arrange(desc(homic))  %>% select(distrito)


##################################################################
#'@Gráficos                                              =
##################################################### objetivo ######
# Colocar em gráficos os dados
#----
#----
# [boxplot] 2003\2013 ####
ggbox <- ggplot(data = pdados,
                aes(x = ano,
                    y = homic)) +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black", size=10),
        axis.line             = element_line(size=1, colour = "black"),
        panel.background      = element_rect(fill="white")) +
  geom_boxplot() +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Município de São Paulo",
       y        = "Taxa de homicídio",
       color    = "Ano")
# {barra03} ordem:2003 ##############
barra03 <- ggplot(data = pdados,
      aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits2003)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 1 (Centro)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("orange", "black"))

# {barra13} ordem:2013 ##########
barra13 <- ggplot(data = pdados,
       aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits2013)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 1 (Centro)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("orange", "black"))

#----
#----
# {barra1} Centro, ordem: 2013 ###########
barra1 <- ggplot(data = filter(pdados, seccional == "1 CENTRO"),
       aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_1)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 1 (Centro)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))




# {barra2} Sul, ordem: 2013 ######
barra2 <- ggplot(data = filter(pdados, seccional == "2 SUL"),
                 aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_2)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 2 (Sul)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))




# {barra3} Oeste, ordem: 2013 ######
barra3 <- ggplot(data = filter(pdados, seccional == "3 OESTE"),
                 aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_3)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 3 (Oeste)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))
# {barra4} Oeste, ordem: 2013 ######
barra4 <- ggplot(data = filter(pdados, seccional == "4 OESTE"),
                 aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_4)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 4 (Oeste)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))
# {barra5} Leste, ordem: 2013 ######
barra5 <- ggplot(data = filter(pdados, seccional == "5 LESTE"),
                 aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_5)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 5 (Leste)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))
# {barra6} Santo Amaro, ordem: 2013 ######
barra6 <- ggplot(data = filter(pdados, seccional == "6 SANTO AMARO"),
                 aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_6)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 6 (Santo Amaro)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))

# {barra7} Itaquera, ordem: 2013 ######
barra7 <- ggplot(data = filter(pdados, seccional == "7 ITAQUERA"),
                 aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_7)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 7 (Itaquera)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))
# {barra8} São Mateus, ordem: 2013 ######
barra8 <- ggplot(data = filter(pdados, seccional == "8 SÃO MATEUS"),
                 aes(x = distrito, y = homic, fill = ano)) +
  geom_bar(stat     = "identity",
           position = "dodge") +
  theme(plot.title            = element_text(hjust=.5),
        plot.subtitle         = element_text(hjust=.5),
        axis.text             = element_text(colour="black"),
        axis.text.x           = element_text(size=8,angle=90,hjust=1,vjust=.3),
        axis.ticks            = element_line(),
        axis.line             = element_line(size=1,colour="black"),
        legend.position       = c(0.95, 0.85),
        legend.title.align    = .5,
        panel.background      = element_rect(fill="white")) +
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits_8)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 8 (São Mateus)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "orange"))

#----
#----
# [pontos] (x, y, z) ########
xyz <- ggplot(data  = filter(dados, ano==2003),
              aes(x = jov1524,
                  y = homic)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = as.factor(seccional),
                 size  = eformais)) +
  theme_classic()
#----
#----
##################################################################
#'@Tabelas                                              =
##################################################### objetivo ######
# Colocar em tabelas os dados
#----
#----
library(huxtable)
dados2013 <- filter(dados, ano==2013)

for(i in 1:80) {
  dados$favela[i] <- print(dados$favela[i])+0.00001
}

jov1524      <- lm(dados2013$homic ~ log(dados2013$jov1524))
baixopadrao  <- lm(dados2013$homic ~ log(dados2013$baixopadrao))
rendamedia   <- lm(dados2013$homic ~ log(dados2013$rendamedia))
dprendamdia  <- lm(dados2013$homic ~ log(dados2013$dprendamedia))
favela       <- lm(dados2013$homic ~ log(dados2013$favela))
eformais     <- lm(dados2013$homic ~ log(dados2013$eformais))
mandato      <- lm(dados2013$homic ~ log(dados2013$mandato))
flagrante    <- lm(dados2013$homic ~ log(dados2013$flagrante))
densidade    <- lm(dados2013$homic ~ log(dados2013$densidade))

huxreg(jov1524,
        baixopadrao,
        rendamedia,
        dprendamdia,
        favela,
        eformais,
        mandato,
        flagrante,
        densidade)

