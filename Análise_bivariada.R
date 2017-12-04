# análise bivariada

# Os zeros da coluna favela
for(i in 1:160) {
  dados$favela[i] <- print(dados$favela[i])+0.000001
}

# Modelo log
jov1524      <- lm(dados2013$homic ~ log(dados2013$jov1524))
baixopadrao  <- lm(dados2013$homic ~ log(dados2013$baixopadrao))
rendamedia   <- lm(dados2013$homic ~ log(dados2013$rendamedia))
dprendamdia  <- lm(dados2013$homic ~ log(dados2013$dprendamedia))
favela       <- lm(dados2013$homic ~ log(dados2013$favela))
eformais     <- lm(dados2013$homic ~ log(dados2013$eformais))
mandato      <- lm(dados2013$homic ~ log(dados2013$mandato))
flagrante    <- lm(dados2013$homic ~ log(dados2013$flagrante))
densidade    <- lm(dados2013$homic ~ log(dados2013$densidade))


#```{r Editor de tabelas, message=FALSE, warning=FALSE, include=FALSE}

coefic = c("jov1524" = log(dados2013$jov1524),
           "baixopadrao",
           "rendamedia",
           "dprendamdia",
           "favela",
           "eformais",
           "mandato",
           "flagrante",
           "densidade")

ht <- huxreg(jov1524,
             baixopadrao,
             rendamedia,
             dprendamdia,
             favela,
             eformais,
             mandato,
             flagrante,
             densidade, coefs=c("Intercept" = "(Intercept)",
                                "jovens 1524" = "log(dados2013$jov1524)",
                                "baixopadrao" = "log(dados2013$baixopadrao)",
                                "rendamedia" = "log(dados2013$rendamedia)",
                                "dprendamedia" = "log(dados2013$dprendamedia)",
                                "favela" = "dados2013$favela",
                                "eformais" = "log(dados2013$eformais)",
                                "mandato" = "log(dados2013$mandato)",
                                "flagrante" = "log(dados2013$flagrante)",
                                "densidade" = "log(dados2013$densidade)"))
ht <- set_font_size(ht, 8)
ht <- set_latex_float(ht, 'b')

#```

dados$ano <- as.character(dados$ano)
histograma <- gghistogram(dados, x = "homic",
                          add = "mean", rug = TRUE,
                          color = "ano", fill = "ano",
                          palette = c("#00AFBB", "#E7B800")) + coord_flip()

boxplot <- ggboxplot(dados, x = "ano", y = "homic",
                     color = "ano", palette =c("#E7B800", "#00AFBB"),
                     add = "jitter", shape = "ano")
# Draw the summary table of Sepal.Length
#::::::::::::::::::::::::::::::::::::::
# Compute descriptive statistics by groups
stable <- desc_statby((dados), measure.var = "homic",
                      grps = "ano")
stable <- stable[, c("ano", "length", "min","max","mean", "median", "sd")]
# Summary table plot, medium orange theme
stable.p <- ggtexttable(stable, rows = NULL)

p1 <- ggarrange(boxplot,histograma, ncol=2, nrow=1, common.legend=T, align="hv")
p2 <- ggarrange(p1, stable.p, ncol=1, nrow=2)
