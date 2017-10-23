rm(list=ls())

library(tidyverse)
library(readr)
library(plm)

setwd("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output")
dir()
txt <- read_rds("C:\\Users\\User\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output\\tab_FINAL")

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

limits <- dados %>% filter(ano == 2003) %>%
  select(distrito, homic) %>% arrange(desc(homic)) %>% select(distrito)



ggplot(data = pdados,
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
  scale_x_discrete(name   = "Distritos", limits=as_vector(limits)) +
  labs(title    = "Taxa de homicídios por 100000 hab (2003 e 2013)",
       subtitle = "Distritos MSP - Seccional 1 (Centro)",
       y        = "Taxa de homicídio",
       color    = "ano") + 
  scale_fill_manual("Ano",values=c("black", "darkgrey"))






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

