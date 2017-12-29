
# Recortes importantes:

# a tibble básica
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

# um exemplo de layout para obj ggplot2

mylayout <- list( # para colocar no barra2
  geom_bar(stat       = "identity",
           position    = "dodge",
           show.legend = F,
           color       = "black"), # salva lista e troca '+' por ','
  theme(plot.title     = element_text(hjust=.5),
        plot.subtitle    = element_text(hjust=.5),
        axis.text        = element_text(colour="black"),
        axis.text.x      = element_text(size=13,angle=90,hjust=1,vjust=.3),
        axis.text.y      = element_text(size=13),
        axis.ticks       = element_line(),
        axis.line        = element_line(size=1,colour="black"),
        axis.title.x     = element_blank(),
        axis.title.y     = element_blank(),
        panel.background = element_rect(fill="white")),
  labs(color = "ano"),
  scale_fill_manual("Ano",
                    values = c("white", "black")),
  scale_color_manual("Ano",
                     values = c("black", "black")),
  scale_y_continuous(limits = c(0,255),
                     breaks = c(0,25,50,75,100,125,150,175,200,225,250))
)
