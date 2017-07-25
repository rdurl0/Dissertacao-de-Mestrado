# Análise exploratória de dados

# subset
txt2013 <- subset(txt, Q1>2003)
txt2003 <- subset(txt, Q1<2013)

# Y
txt2013$homic
txt2013$roubovlc
txt2013$furtovlc

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


# gráfico de barras
library(plyr)
homic13 <-arrange(txt2013,desc(txt2013$homic))
roubovcl13 <-arrange(txt2013,desc(txt2013$roubovlc))
furtovlc13 <-arrange(txt2013,desc(txt2013$furtovlc))


# par() par muda os parâmetros do gráfico, esta função vem antes do gráfico:
par(mar = c (4, 5, 0.1, 0.1), # tam. margens c(base, esq, topo, dir) 
    mfrow = c(1,1),           # qtos gráficos na msm imagem
    las = 3,                  # pos. legendas
    cex = 0.9,                # tam fonte das legendas, títulos, pontos, etc.
    cex.lab = 0.95,           #
    cex.axis = 0.9,
    mgp = c(2 ,0.7, 0),
    tcl = 0.3)
boxplot(txt2013$homic)
barplot(a$homic, names.arg = a$Q13)
help(barplot)
