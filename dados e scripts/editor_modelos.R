rm(list=ls())

# diretório
#setwd("C:\\Users\\User\\Google Drive\\Pesquisa\\SUR SUR-lag SUR-error")
setwd("C:\\Users\\Raul\\Google Drive\\Pesquisa\\SUR SUR-lag SUR-error")
dir()

# tabela
dados <- read.table("dados.txt", h=T)
dados <- read_deliim("dados.txt", col_names=F, delim=",")
is.data.frame(dados) # TRUE
#
#install.packages("readr")
# Pacotes necessários--------------------

libs <- c("readr",
  "spdep", "systemfit", "plyr", 
          "spse", "splm", "MASS", "ggplot2", "stargazer")

for (x in libs){
      print (paste0(x, " is already installed "))
      library(x, character.only = TRUE)
    }

# subsets---------------------

# data.frames
dados2013 <- subset(dados, Ano>2003)
dados2003 <- subset(dados, Ano<2013)

# painel
painel <- plm.data(dados, indexes = c("Dpol", "Ano"))
pdados <- pdata.frame(dados, c("Dpol", "Ano"))

# sistema (Y ~ X1 + X2) ---------------------------

# com subset
eq1 <- dados2003$homic ~ dados2003$jov1524 + dados2003$baixopadrao + dados2003$rendamedia + dados2003$dprendamedia + dados2003$favela + dados2003$eformais + dados2003$mandato
eq2 <- dados2013$homic ~ dados2013$jov1524 + dados2013$baixopadrao + dados2013$rendamedia + dados2013$dprendamedia + dados2013$favela + dados2013$eformais + dados2013$mandato
system <- list(eq1 = eq1, eq2 = eq2)

# sem subset
e1 <- homic ~ jov1524 + baixopadrao + rendamedia + dprendamedia + favela + eformais + mandato
e2 <- homic ~ jov1524 + baixopadrao + rendamedia + dprendamedia + favela + eformais + mandato
system2 <- list(tp1 = e1, tp2 = e2)

# Estatísticas descritivas -----------
# preparando tabela com estatísticas descritivas
# summary
stargazer(dados2003, dados2013,
          title="Estatísticas descritivas",
          out="C:\\Users\\Raul\\Google Drive\\Pesquisa\\SUR SUR-lag SUR-error\\result.htm",
          type="html")

# [barras] crime x dpol
barra <- ggplot(data = subset(pdados, Seccional == "1 CENTRO"),
            aes(x=Distrito, y=homic, fill=Ano)) +
          geom_bar(stat="identity", position="dodge")

# [pontos] (x, y, z)
xyz <- ggplot(data = dados2013,
          aes(x=jov1524, y=homic,
                color=as.factor(Seccional), size=eformais)) +
        geom_point() #+ geom_smooth(method="lm") + facet_wrap(~Seccional)

# [boxplot] 2003\2013
box <- Boxplot(pdados$homic, 
               pdados$Ano, 
               labels=as.character(pdados$Distrito),
               id.n=Inf) # id outliers

ggbox <- ggplot(data=pdados,
            aes(Ano, homic)) +
          geom_boxplot() +
          facet_wrap(~Ano) # tem q arrumar


# Matriz W --------------------------------

# diretório
dir()
queen_txt<-read.table("queen.txt", h=T)
dim(queen_txt) # 80x80

# matriz esparsa
queen<-as.matrix(queen_txt)
is.matrix(queen)

# Nomeando colunas e linhas com o num do DPol correspondente.
DPol<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","21","22","23","24","25","27","28","29","31","33","34","36","37","38","40","41","42","44","46","47","48","50","51","53","54","55","56","58","59","62","63","65","66","67","68","70","74","75","77","78","81","87","89","91","92","93","96","98","99","100","102","103","1857","2073","3052","3264","3597","4380","4572","4969","85101","193990","268395")
colnames(queen)<-DPol
rownames(queen)<-DPol
isSymmetric(queen) # pergunta se queen é simetrica
dim(queen)

# objeto 'listw', style="W" (padronizada na linha)
w <- mat2listw(queen, row.names = NULL, style="M")
listw <- nb2listw(w$neighbours, glist=NULL, style="W", zero.policy=NULL) 

# OLS------
eqsim <- systemfit(system, method="OLS", data = dados, pooled = TRUE)
summary(eqsim) # exibe resultados

# Modelo: SUR a-espacial-----------------------

# Os parâmetros são diferentes entre as localidades, mas constantes nos períodos.

sur <- systemfit(system, method = "SUR", data = dados, pooled = TRUE)
summary(sur) # exibe resultados

# Modelo: SUR espacial--------------------------

#         SUR-lag---------------------------------------
# modelo
surlag <- spseml(system2,
                 data= painel,
                 panel = TRUE,
                 index = "Dpol",
                 w = listw,
                 method="eigen",
                 model = "lag"
                 )
summary(surlag)

#         SUR-error------------------------------------------
surerror <- spseml(system2,
                 data= painel,
                 panel = TRUE,
                 index = "Dpol",
                 w = listw,
                 method="eigen",
                 model = "error"
                  )
summary(surerror)




# AEDE--------------
# Moran