##################################################################
#'@editor_analise_1                                              =
##################################################### objetivo ######
# Comparar as taxas de ocorrência de crime e de  
# atividade policial por 100000 habitantes de MSP com GSP e
# Interior entre os anos 2000 e 2010
#----
#----
# bibliotecas utilizadas #########################################

rm(list=ls())

library(tidyverse)
library(readxl)
library(stringr)
library(xml2)

library(rvest)


# Diretório ######################################################
rm(list=ls())
setwd("C:\\Users\\rauld\\Google Drive\\meu_projeto\\dados e scripts\\planilhas_input")
dir()

#----
#----
##################################################################
#'@Series_trimestrais_3T1995/3T2016                              =
######################################################## fonte #####
# http://www.ssp.sp.gov.br/Estatistica/Trimestrais.aspx
#----
#----
# Ocorrências por natureza ###############

# leitura do arquivo
ocorr_natureza <- read_excel("plan_estadoSP_ssp.xlsx", sheet="Plan1", na="-")

# guarda os nomes para tabela de descrição
xlsx <- tibble(Cod           = seq(1:11),
               nome_original = names(ocorr_natureza[,1:11]))

# variáveis com novos nomes
ocorr_natureza$ano              <- ocorr_natureza$`Ocorrência/período`
ocorr_natureza$pessoa           <- ocorr_natureza$`Contra a pessoa`
ocorr_natureza$patrimônio       <- ocorr_natureza$`Contra o patrimônio`
ocorr_natureza$contravencionais <- ocorr_natureza$`Contravencio-is`
ocorr_natureza$outros_criminais <- ocorr_natureza$`Outros crimi-is (não inclui contravenções)`
ocorr_natureza$outros_delitos   <- ocorr_natureza$`Outros delitos (Inclui contravenções)`
ocorr_natureza$violentos        <- ocorr_natureza$`Total de Crimes Violentos ( Hom.Doloso, Roubo, Latrocínio, Estupro e EMS)`
ocorr_natureza$total_delitos    <- ocorr_natureza$`Total de delitos`
ocorr_natureza$costumes         <- ocorr_natureza$`Contra os constumes (*)`
ocorr_natureza$entorpecentes    <- ocorr_natureza$Entorpecentes
  # exclui as variáveis com nome antigo
ocorr_natureza$`Ocorrência/período`   <- NULL
ocorr_natureza$`Contra a pessoa`      <- NULL
ocorr_natureza$`Contra o patrimônio`  <- NULL
ocorr_natureza$`Contravencio-is`      <- NULL
ocorr_natureza$`Outros crimi-is (não inclui contravenções)` <- NULL
ocorr_natureza$`Outros delitos (Inclui contravenções)`      <- NULL
ocorr_natureza$`Total de Crimes Violentos ( Hom.Doloso, Roubo, Latrocínio, Estupro e EMS)` <- NULL
ocorr_natureza$`Total de delitos`        <- NULL
ocorr_natureza$`Contra os constumes (*)` <- NULL
ocorr_natureza$grupo         <- NULL
ocorr_natureza$Entorpecentes <- NULL
glimpse(ocorr_natureza)

# reordena para mesclar com a tabela de descrição
ocorr_natureza <- ocorr_natureza %>% select("ano", "local", "pessoa", "patrimônio",
                                            "costumes", "entorpecentes", "contravencionais",
                                            "outros_criminais", "outros_delitos", "violentos",
                                            "total_delitos")

# cria tabela de descrição
descricao_natureza <- tibble(Cod_va                = xlsx$Cod,
                             Nome_original         = xlsx$nome_original,
                             Novo_nome             = names(ocorr_natureza),
                             Descrição_da_variável = c("Ano de registro das ocorrências",
                                                       "Interior, Grande São Paulo, Capital",
                                                        "Ocorrências de crime contra pessoa",
                                                        "Ocorrências de crime contra o patrimònio",
                                                        "Ocorrências de crime contra os costumes (até 2009)/contra a dignidade sexual (2010-atual)",
                                                        "Ocorrências de tráfico de Entorpecentes",
                                                        "Ocorrências de contravenções (https://goo.gl/QccSm2)",
                                                        "Ocorrências de outros criminais - exceto contravenções",
                                                        "Ocorências de outros delitos - inclusive contravenções",
                                                        "Total de crimes violentos (Homicidio Doloso, Roubo, Latrocínio, Estupro e EMS)",
                                                        "Total de delitos"))

# separa ano de trimestre
trim                     <- rep(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)), 20)
ocorr_natureza$trimestre <- c(rep(3,3), rep(4,3), trim, rep(1,3))

# precisou reordenar de novo :(
ocorr_natureza <- ocorr_natureza %>%
                    select("ano", "trimestre", "local", "pessoa", "patrimônio",
                           "costumes", "entorpecentes", "contravencionais", "outros_criminais",
                           "outros_delitos", "violentos", "total_delitos")

# e tira os caracteres indesejados
ocorr_natureza$local <- gsub("Gde", "Grande", ocorr_natureza$local)
ocorr_natureza$ano   <- gsub("-1T", "", ocorr_natureza$ano)
ocorr_natureza$ano   <- gsub("-2T", "", ocorr_natureza$ano)
ocorr_natureza$ano   <- gsub("-3T", "", ocorr_natureza$ano)
ocorr_natureza$ano   <- gsub("-4T", "", ocorr_natureza$ano)
ocorr_natureza$ano   <- gsub("-T4", "", ocorr_natureza$ano)
ocorr_natureza$ano   <- as.integer(ocorr_natureza$ano)

# Mais uma vez atualiza a tabela de descrição
descricao_natureza         <- rbind(descricao_natureza[1,],
                                    c("", "Ocorrência/período", "trimestre",
                                    "Trimestre de registro das ocorrências"),
                                    descricao_natureza[2:11,])
descricao_natureza$Cod_var <- seq(1:12)
View(descricao_natureza)

# Agora salve e faça bom uso no report...
write_rds(descricao_natureza, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_trim_ocorrencias_por_natureza_descricao.rds")
write_rds(ocorr_natureza, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_trim_ocorrencias_por_natureza.rds")


#----
# Ocorrencias por tipo ####################
rm(list=ls())
dir()

# leia a planilha e guarde os nomes
ocorr_tipo    <- read_excel("plan_estadoSP_ssp.xlsx", sheet="Plan2", na="-")
nome_original <- names(ocorr_tipo)
nome_original <- c(rep(nome_original[1],2), nome_original[2:16]) # é preciso desmembrar o ano do trimestre

# ajustando os nomes, com a função transmute() fica bem mais fácil! :)
ocorr_tipo <- ocorr_tipo %>%
                transmute(ano                 = `Ocorrência/período`,
                          trimestre           = `Ocorrência/período`,
                          local               = `Ocorrências policiais registradas, por tipo`,
                          homicidio           = `Homicídio doloso (i)`,
                          vitima_homicidio    = `Nº de Vítimas em Homicídio Doloso`,
                          tentativa_homicidio = `Tentativa de homicídio`,
                          latrocinio          = Latrocínio,
                          vitima_latrocinio   = `Nº de Vítimas de Latrocínio`,
                          estupro             = Estupro,
                          extorsao_med_sequestro = `Extorsão mediante seqüestro (5)`,
                          trafico       = `Tráfico de entorpecentes`,
                          roubo         = `Roubo - outros (6) (i)`,
                          roubo_veiculo = `Roubo de veículos`,
                          roubo_banco   = `Roubo a Banco`,
                          roubo_carga   = `Roubo de Carga`,
                          furto         = `Furto - outros`,
                          furto_veiculo = `Furto de veículos`)

# Limpando valores de local, ano e trimestre...
trim                 <- rep(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)), 20)
ocorr_tipo$trimestre <- c(rep(3,3), rep(4,3), trim, rep(1,3))

ocorr_tipo$local <- gsub("Gde", "Grande", ocorr_tipo$local)
ocorr_tipo$ano   <- gsub("-1T", "", ocorr_tipo$ano)
ocorr_tipo$ano   <- gsub("-2T", "", ocorr_tipo$ano)
ocorr_tipo$ano   <- gsub("-3T", "", ocorr_tipo$ano)
ocorr_tipo$ano   <- gsub("-4T", "", ocorr_tipo$ano)
ocorr_tipo$ano   <- gsub("-T4", "", ocorr_tipo$ano)
ocorr_tipo$ano   <- as.integer(ocorr_tipo$ano)

# criando uma tabela de descrição...
descricao_tipo <- tibble(Cod = 1:17,
                         Nome_original = nome_original,
                         Novo_nome     = names(ocorr_tipo),
                         Descrição     = c("Ano de registro das ocorrências",
                                       "Trimestre de registro das ocorrências",
                                       "Interior, Grande São Paulo, Capital",
                                       "Ocorrências de homicídio doloso (exclui por acidente de trânsito",
                                       "Número de vítimas de homicídio doloso - a partir de 2005",
                                       "Ocorrências de tentativa de homicídio",
                                       "Ocorrências de latrocínio",
                                       "Número de vítimas de latrocínio - a partir de 2005",
                                       "Ocorrências de estupro",
                                       "Extorsão mediante sequestro - (5) Dados do Serviço de Informações Criminais da Divisão Anti-sequestro",
                                       "Ocorrências de tráfico de entorpecentes",
                                       "Ocorrências de roubo-outros (inclusive roubo de carga e banco)",
                                       "Ocorrências de roubo de veículos",
                                       "Ocorrências de roubo de banco - a partir de 2005",
                                       "Ocorrências de roubo de carga - a partir de 2005",
                                       "Ocorrências de furto - outros",
                                       "Ocorrências de furto de veículos"))


# Agora salve e faça bom uso no report...
write_rds(descricao_tipo, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_trim_ocorrencias_por_tipo_descricao.rds")
write_rds(ocorr_tipo, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_trim_ocorrencias_por_tipo.rds")


#----
# Atividade policial #####################


rm(list=ls())

# leia a planilha e guarde os nomes
atividade_policial <- read_excel("plan_estadoSP_ssp.xlsx", sheet="Plan3", na="-")
nome_original      <- names(atividade_policial)
nome_original      <- c(rep(nome_original[1],2), nome_original[2:5]) # é preciso desmembrar o ano do trimestre

atividade_policial <- atividade_policial %>%
                        transmute(ano            = `Ocorrência/período`,
                                  trimestre      = `Ocorrência/período`,
                                  local          = `ATIVIDADES POLICIAIS`,
                                  prisoes        = `Prisões efetuadas`,
                                  armas_apreend  = `Armas de fogo apreendidas`,
                                  veículos_recup = `N° de veículos recuperados (ii)`)

# Limpando valores de local, ano e trimestre...
trim                         <- rep(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)), 20)
atividade_policial$trimestre <- c(rep(3,3), rep(4,3), trim, rep(1,3))

atividade_policial$local <- gsub("Gde", "Grande", atividade_policial$local)
atividade_policial$ano   <- gsub("-1T", "", atividade_policial$ano)
atividade_policial$ano   <- gsub("-2T", "", atividade_policial$ano)
atividade_policial$ano   <- gsub("-3T", "", atividade_policial$ano)
atividade_policial$ano   <- gsub("-4T", "", atividade_policial$ano)
atividade_policial$ano   <- gsub("-T4", "", atividade_policial$ano)
atividade_policial$ano   <- as.integer(atividade_policial$ano)

descricao_atividade_policial <- tibble(Cod = 1:6,
                                       Nome_original = nome_original,
                                       Variável = names(atividade_policial),
                                       Descrição = c("Ano de registro das ocorrências",
                                                     "Trimestre de registro das ocorrências",
                                                     "Interior, Grande São Paulo, Capital",
                                                     "Número de prisões efetuadas (em flagrante + por mandado)",
                                                     "Número de armas de fogo apreendidas",
                                                     "Número de veículos recuperados"))

# Agora salve e faça bom uso no report...
write_rds(descricao_atividade_policial, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_trim_atividade_policial_descricao.rds")
write_rds(atividade_policial, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_trim_atividade_policial.rds")


#----
#----
##################################################################
#'@População_1992/2016                                           =
######################################################## fonte ####
# População Residente - Estimativas para o TCU - São Paulo
# http://www2.datasus.gov.br/DATASUS/index.php?area=02
#
#----
#----
# População residente por município ######
rm(list=ls())

# o banco de dados contém:
descricao_pop <- tibble(descrição = c("População estimada por Município e Ano - Estimativa para TCU"),
                        período   = c("1992-1995, 1997-2016"),
                        Fonte     = c("IBGE/DATASUS"))

# ler csv
dir()
pop <- read_delim("plan_populacao.csv", na="-", delim=";")

# austando encoding...
names(pop)    <- iconv(names(pop), to="latin1")
pop$Município <- iconv(pop$Município, to="latin1//TRANSLIT")

# recortando células inúteis...
pop <- pop[1:646,] %>%
  mutate(municipio = 1:646,
         nome_municipio = iconv(Município, to="ASCII//TRANSLIT"),
         Cod_IBGE = iconv(Município, to="ASCII//TRANSLIT"))

# separando codigo e nome do município. ex:[1] "350010 Adamantina"

letr <- c(letters, LETTERS, "'", "-")# vetores auxiliares
num  <- c("1", "2", "3", "4", "5  ", "6", "7", "8", "9", "0  ")

    pop$Cod_IBGE      <- gsub(" ", "", pop$Cod_IBGE)# só o código 
    for (i in letr[1:54]) {  
        pop$Cod_IBGE  <- gsub(i, "", pop$Cod_IBGE)
        }
         pop          <- pop[1:645,]
         pop$Cod_IBGE <- as.integer(pop$Cod_IBGE)
  
    for(i in num[1:10]) { # só o nome
        pop$nome_municipio <- gsub(i, "", pop$nome_municipio)
        }
         pop$nome_municipio <- gsub("5 ", "", pop$nome_municipio)
         pop$nome_municipio <- gsub("0 ", "", pop$nome_municipio)
         pop$nome_municipio <- gsub("5", "", pop$nome_municipio)
         pop$nome_municipio <- gsub("0", "", pop$nome_municipio)
         pop$nome_municipio <- gsub("-", " ", pop$nome_municipio)
        
print(pop$nome_municipio)

# agora que desmembrou, dispensar a coluna suja...
pop$Município <- NULL

# tem um ano que não tem...
pop$`1996` <- c(rep(NA, 645))

# reordena
pop <- select(pop, municipio, nome_municipio, Cod_IBGE,
            "1992","1993","1994","1995", "1996", "1997","1998",
            "1999","2000","2001","2002","2003","2004",
            "2005","2006","2007","2008","2009","2010",
            "2011","2012","2013","2014","2015","2016")

# dplyr::gather() para converter variáveis em observações
pop     <- pop %>% gather(names(pop)[4:28], key="ano", value="populacao")
pop$ano <- as.integer(pop$ano)

write_rds(pop, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_populacao_SP.rds")

#----
#----
##################################################################
#'@Obtendo_taxas                                                 =
##################################################################
#----
#----
# Diretório ######################################################
rm(list=ls())
setwd("C:\\Users\\rauld\\Google Drive\\meu_projeto\\dados e scripts\\tabelas_output\\tab_análise_1")
dir()
#----
#----
# Tabela para taxas de homicídio por 100000 hab. #####
oc_tipo <- read_rds("tab_trim_ocorrencias_por_tipo.rds")
pop     <- read_rds("tab_populacao_SP.rds")

# subset dos crimes e da população de 2000 até 2010

homicidio <- select(oc_tipo, local, ano, homicidio) %>%
              group_by(local, ano) %>%
              summarise(homicidio=sum(homicidio)) %>%
              filter(homicidio, ano<=2010, ano>=2000)

roubo_vcl <- select(oc_tipo, local, ano, roubo_veiculo) %>%
              group_by(local, ano) %>%
              summarise(roubo_veiculo=sum(roubo_veiculo)) %>%
              filter(roubo_veiculo, ano<=2010, ano>=2000)

furto_vcl <- select(oc_tipo, local, ano, furto_veiculo) %>%
              group_by(local, ano) %>%
              summarise(furto_veiculo=sum(furto_veiculo)) %>%
              filter(furto_veiculo, ano<=2010, ano>=2000)

pop       <- select(pop, nome_municipio, ano, populacao) %>%
              filter(populacao, ano<=2010, ano>=2000)

# subset para região metropolitana de SP (39 municípios)
names(pop)

url  <- "https://www.emplasa.sp.gov.br/RMSP"
rmsp <- read_html(url) %>% 
         rvest::html_node("table") %>%
         rvest::html_table(dec = '.') %>%
         as_tibble() %>%
         select(Municípios) %>%
         transmute(nome_municipio = str_replace(iconv(Municípios, from="UTF-8", to="ASCII//TRANSLIT")
                                                , "-", " ")) %>%
         slice(c(    2,      # capital
                  4:14,      # sub-região leste
                 17:21,      # sub-região norte
                 24:30,      # sub-região oeste
                 33:39,      # sub-região sudeste (aka ABCD)
                 42:49)) %>% # sub-região sudoeste
         arrange(nome_municipio)

# RMSP está pronta. basta criar Interior e MSP.

rmsp[9,] <- "Embu das Artes"

pop_rmsp     <- inner_join(rmsp[-36,], pop, by="nome_municipio") %>%
                  mutate(local=rep("Grande SP", 418)) %>%
                  group_by(ano, local) %>%
                  summarise(populacao = sum(populacao))

pop_interior <- anti_join(pop, rmsp, by="nome_municipio") %>%
                  mutate(local=rep("Interior", 6666)) %>%
                  group_by(ano, local) %>%
                  summarise(populacao = sum(populacao))

pop_msp      <- filter(pop, nome_municipio=="Sao Paulo") %>%
                  mutate(local=rep("Capital", 11)) %>%
                  select(ano, local, populacao)

taxa_crimes_SP <- bind_rows(pop_msp, pop_interior, pop_rmsp) %>%
                 left_join(. ,homicidio, by=c("local", "ano")) %>%
                 left_join(. ,furto_vcl, by=c("local", "ano")) %>%
                 left_join(. ,roubo_vcl, by=c("local", "ano"))

write_rds(taxa_crimes_SP, "C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_compara_txcrime_estadoSP.rds")

# O próximo passo é plotar as taxas de crimes por 100000 habitantes:

#----
# Gráficos #########
rm(list=ls())
setwd("C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output")

estado_sp <- read_rds("tab_compara_txcrime_estadoSP.rds")

ggplot(data=estado_sp, mapping=aes(x=factor(ano),
                                   y=((homicidio/populacao)*100000),
                                   color=local)) + 
          geom_line() +
          geom_point() +
          theme_classic() +
          labs(title="Taxa de homicídios por 100000 habitantes - Estado de São Paulo: Capital, Grande SP e Interior",
               subtitle="Fonte: Estatísticas Trimestrais da Secretaria de Segurança Pública do Estado de São paulo",
               y="Taxa de homicídio",
               x="Ano") +
          theme(legend.position = c(.8, .8))
