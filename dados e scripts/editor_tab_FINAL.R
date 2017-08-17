## ----Busca no diret?rio-----------------
setwd("C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts")
dir()
txt<-read.table("tab_FINAL.txt", h=T)

## ----?NDICE DE VARI?VEIS-------------------
Q1 		# Ano (2013) (SSP)
Q12		# Num Distrito Policial (SSP)
Q13		# Nome Distrito Policial (SSP)
Q22		# Homic?dio doloso (SSP)
Q24		# Tentativa de homic?dio (SSP)
Q27		# Les?o corporal dolosa (SSP)
Q30		# Latroc?nio (SSP)
Q32		# Estupro (SSP)
Q34		# Ocorr?ncias de tr?fico de entorpecentes (SSP)
Q37		# Ocorr?ncias de porte ilegal de arma (SSP)
Q40		# Roubo de ve?culo (SSP)
Q41		# Roubo - outros (SSP)
Q43		# Furto - outros (SSP)
Q44		# Furto de ve?culos (SSP)
V1		# ?bitos por causas externas (por local de resid?ncia/DATASUS)
P		# Popula??o total (2013/IBGE-SEADE)
PJ_ONU	# Popula??o jovem ONU(15 a 24 anos)(2013/IBGE-SEADE)
PJ_CNJ	# Popula??o jovem CNJ(15 a 29 anos)(2013/IBGE-SEADE)
PM		# Popula??o masculina(2013/IBGE-SEADE)
PMJ_ONU	# Popula??o jovem mascul ONU(2013/IBGE-SEADE)
PMJ_CNJ	# Popula??o jovem mascul CNJ(2013/IBGE-SEADE)
fvl		# Estimativa de domic?lios em favelas (2010) (SEHAB/HABISP)
Tot_DR	# Total de domic?lios particulares permanentes com rendimento (2010)(CENSO/IBGE) 
at?_5		# Domic?lios part perm com rendimento at? 5 s.m.
X5_a_10	# Domic?lios part perm com rendimento >5 at? 10 s.m
X10_a_20	# Domic?lios part perm com rendimento >10 at? 20 s.m
X20_		# Domic?lios part perm com rendimento >20 s.m
RM_DOM	# Renda domiciliar m?dia pnderada, em s.m. 
S_RM_DOM 	# Desvio padr?o RM_DOM
perc_5	#
perc5_a_10	#
perc10_a_20	#
perc20_	#
Total_EF	# N?mero de empregos formais (RAIS/CAGED)
P2000		# Popula??o total, ano 2010
EF_P		# raz?o (emprego_formal/popula??o2000)
Q70		# Total de inqu?ritos instaurados (SSP)		
Q71		# N?mero de flagrantes lavrados (SSP)
Q73		# N?mero de ve?culos recuperados (SSP)
Q74		# N?mero de pris?es efetuadas (SSP)
Q75		# N?mero de pessoas presas em flagrante (SSP)
Q76		# N?mero de pessoas presas por mandato (SSP)
Q77		# N?mero de infratores apreendidos em flagrante (SSP)
Q78		# N?mero de infratores apreendidos por mandato (SSP)
Q79		# N?mero de armas de fogo apreendidas (SSP)
Km2		# ?rea da localidade em Km2
AC_1		# ?rea constru?da 1 - uso resid?ncial horizontal de baixo padr?o (TCPL/SEMPLA/SMDU)
AC_2		# ?rea constru?da 2 - uso resid?ncial horizontal de m?dio padr?o (TCPL/SEMPLA/SMDU)
AC_3		# ?rea constru?da 3 - uso resid?ncial horizontal de alto padr?o (TCPL/SEMPLA/SMDU)
AC_4		# ?rea constru?da 4 - uso resid?ncial vertical de m?dio padr?o (TCPL/SEMPLA/SMDU)
AC_5		# ?rea constru?da 5 - uso resid?ncial vertical de alto padr?o (TCPL/SEMPLA/SMDU)
AC_6		# ?rea Constru?da 6 - Uso Com?rcio e Servi?o Horizontal
AC_7		# ?rea Constru?da 7 - Uso Com?rcio e Servi?o Vertical
AC_8		# ?rea Constru?da 8 - Uso Industrial
AC_9		# ?rea Constru?da 9 - Uso Armaz?ns e Dep?sitos
AC_10		# ?rea Constru?da 10 - Uso Especial ( Hotel, Hospital, Cart?rio, Etc. )
AC_11		# ?rea Constru?da 11 - Uso Escola
AC_12		# ?rea Constru?da 12 - Uso Coletivo ( Cinema, Teatro, Clube, Templo, Etc. )
AC_14		# ?rea constru?da 14 - uso resid?ncial vertical de baixo padr?o (TCPL/SEMPLA/SMDU)
AC_15		# ?rea Constru?da 15 - Uso Garagens n?o-residenciais
AC_99		# ?rea constru?da 99 - outros usos (uso e padr?o n?o previsto) (TCPL/SEMPLA/SMDU)
AC_ResBP	# ?rea constru?da - total residencial baixo padr?o (=soma(AC_1;AC_14;AC_99))
AC_nRes	# ?rea constru?da - total n?o residencial ((=soma(AC_6,AC_7,AC_8,AC_9,AC_10,AC_11,AC_12,AC_15))
#tx_Qx 	# Taxa de crime (Qx) por 100000 habitantes
#
####txt2013
# 
is.numeric(AC_15) # verifique se as vari?veis s?o do tipo 'numeric'


## ----Seccionais------------

sc <- as.data.frame(rbind(c(1, "1 CENTRO"), c(2, "1 CENTRO"), c(3, "1 CENTRO"), c(4, "1 CENTRO"), c(5, "1 CENTRO"),
                         c(6, "1 CENTRO"), c(7,"3 OESTE"), c(8, "1 CENTRO"), c(9, "4 OESTE"), c(10, "5 LESTE"), c(11, "6 SANTO AMARO"), c(12, "1 CENTRO"), c(13,"4 OESTE"), c(14, "3 OESTE"),
                         c(15,"3 OESTE"), c(16, "2 SUL"), c(17, "2 SUL"), c(21, "5 LESTE"), c(22, "7 ITAQUERA"),
                         c(23, "3 OESTE"), c(24, "7 ITAQUERA"), c(25, "6 SANTO AMARO"), c(27, "2 SUL"), c(28, "4 OESTE"),
                         c(29, "5 LESTE"), c(31, "5 LESTE"), c(33, "3 OESTE"), c(34, "3 OESTE"), c(36, "2 SUL"),
                         c(37, "3 OESTE"), c(38, "4 OESTE"), c(40, "4 OESTE"), c(41, "8 S?O MATEUS"), c(42, "5 LESTE"),
                         c(44, "8 S?O MATEUS"), c(46, "3 OESTE"), c(47, "6 SANTO AMARO"), c(48, "6 SANTO AMARO"),
                         c(50, "7 ITAQUERA"), c(51, "3 OESTE"), c(53, "8 S?O MATEUS"), c(54, "8 S?O MATEUS"),
                         c(55, "8 S?O MATEUS"), c(56, "5 LESTE"), c(58, "5 LESTE"), c(59, "7 ITAQUERA"), c(62, "7 ITAQUERA"),
                         c(63, "7 ITAQUERA"), c(65, "7 ITAQUERA"), c(66, "8 S?O MATEUS"), c(67, "7 ITAQUERA"),
                         c(68, "7 ITAQUERA"), c(70, "8 S?O MATEUS"), c(74, "4 OESTE"), c(75, "3 OESTE"), c(77, "1 CENTRO"),
                         c(78, "1 CENTRO"), c(81, "5 LESTE"), c(87, "3 OESTE"), c(89, "3 OESTE"), c(91, "3 OESTE"),
                         c(92, "6 SANTO AMARO"), c(93, "3 OESTE"), c(96, "2 SUL"), c(98, "6 SANTO AMARO"), c(99, "6 SANTO AMARO"),
                         c(100, "6 SANTO AMARO"), c(102, "6 SANTO AMARO"), c(103, "7 ITAQUERA"), c(1857, "5 LESTE"),
                         c(2073, "4 OESTE"), c(3052, "5 LESTE"), c(3264, "7 ITAQUERA"), c(3597, "2 SUL"), c(4380, "6 SANTO AMARO"),
                         c(4572, "4 OESTE"), c(4969, "8 S?O MATEUS"), c(85101, "6 SANTO AMARO"), c(193990, "4 OESTE"),
                         c(268395, "2 SUL")))
colnames(sc) <- c("Q12", "Seccional")
#txt <- merge(txt, sc, by = intersect("Q12", "Q12"))

# exportar para .txt
#write.table(txt, "C:\\Users\\Raul\\Google Drive\\Pesquisa\\SUR SUR-lag SUR-error\\novoFINAL.txt")

# normalizando vari?veis ----------------

# indexes
Ano <- txt$Q1
Distrito <- txt$Q13
Dpol <- txt$Q12

# Y
txt$homic <- (txt$Q22/txt$P)*100000
txt$roubovlc <- (txt$Q40/txt$P)*100000
txt$furtovlc <- (txt$Q43/txt$P)*100000

# X
txt$jov1524 <- (txt$PMJ_ONU/txt$P)
txt$baixopadrao <- (txt$AC_ResBP/1000)
txt$rendamedia <- txt$RM_DOM
txt$dprendamedia <- txt$S_RM_DOM
txt$favela <- (txt$fvl/txt$P)*100000
txt$eformais <- txt$EF_P
txt$mandato <- (txt$Q76/txt$P)*100000

# outras
txt$prisoes <- (txt$Q74/txt$P)*100000
txt$flagrante <- (txt$Q75/txt$P)*100000
txt$DENS <- (txt$P/txt$Km2)


# data.frame-----------------------------

dados <- data.frame(Ano = as.integer(Ano),
                    Distrito = as.factor(Distrito),
                    Dpol = as.integer(Dpol),
                    Seccional = as.factor(txt$Seccional),
                    homic = as.numeric(txt$homic),
                    roubovcl = as.numeric(txt$roubovlc),
                    furtovcl = as.numeric(txt$furtovlc),
                    jov1524 = as.numeric(txt$jov1524),
                    baixopadrao = as.numeric(txt$baixopadrao),
                    rendamedia = as.numeric(txt$rendamedia),
                    dprendamedia = as.numeric(txt$dprendamedia),
                    favela = as.numeric(txt$favela),
                    eformais = as.numeric(txt$eformais),
                    mandato = as.numeric(txt$mandato),
                    stringsAsFactors = FALSE)

# exportar para .txt
write.table(dados, "C:\\Users\\Raul\\Google Drive\\Pesquisa\\SUR SUR-lag SUR-error\\dados.txt")




## ----vetor com nomes------------
crimes <- c("Homic?dio (Q22)", "Tentat Homic?dio (Q24)", "Les?o Corporal dol.(Q27)",
            "Latroc?nio (Q30)", "Estupro (Q32)", "Tr?fico (Q34)", "Porte de arma (Q37)",
            "Roubo - ve?culos (Q40)", "Roubo - outros (Q41)", "Furto - ve?culos (Q44)",
            "Furto - outros (Q44)", "?bitos por agress?o (V1)")
