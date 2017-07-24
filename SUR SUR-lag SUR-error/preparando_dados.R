rm(list=ls())

library(tidyverse)
library(readxl)

#################################
#'
#'@Ocorrências_por_natureza
#'
#################################

# Diretório
setwd("C:\\Users\\Raul\\Documents\\meu_projeto\\SUR SUR-lag SUR-error")
dir()

# leitura do arquivi
ocorr_natureza <- read_excel("tidy_agredados_ssp.xlsx", sheet="Plan1", na="-")

# guarda os nomes para tabela de descrição
xlsx <- tibble(Cod = seq(1:11),
               XLSX = names(ocorr_natureza[,1:11]))
xlsx

# variáveis com novos nomes
ocorr_natureza$ano <- ocorr_natureza$`Ocorrência/período`
ocorr_natureza$pessoa <- ocorr_natureza$`Contra a pessoa`
ocorr_natureza$patrimônio <- ocorr_natureza$`Contra o patrimônio`
ocorr_natureza$contravencionais <- ocorr_natureza$`Contravencio-is`
ocorr_natureza$outros_criminais <- ocorr_natureza$`Outros crimi-is (não inclui contravenções)`
ocorr_natureza$outros_delitos <- ocorr_natureza$`Outros delitos (Inclui contravenções)`
ocorr_natureza$violentos <- ocorr_natureza$`Total de Crimes Violentos ( Hom.Doloso, Roubo, Latrocínio, Estupro e EMS)`
ocorr_natureza$total_delitos <- ocorr_natureza$`Total de delitos`
ocorr_natureza$costumes <- ocorr_natureza$`Contra os constumes (*)`
ocorr_natureza$entorpecentes <- ocorr_natureza$Entorpecentes
  # exclui as variáveis com nome tidy
ocorr_natureza$`Ocorrência/período` <- NULL
ocorr_natureza$`Contra a pessoa` <- NULL
ocorr_natureza$`Contra o patrimônio`  <- NULL
ocorr_natureza$`Contravencio-is`  <- NULL
ocorr_natureza$`Outros crimi-is (não inclui contravenções)` <- NULL
ocorr_natureza$`Outros delitos (Inclui contravenções)` <- NULL
ocorr_natureza$`Total de Crimes Violentos ( Hom.Doloso, Roubo, Latrocínio, Estupro e EMS)` <- NULL
ocorr_natureza$`Total de delitos` <- NULL
ocorr_natureza$`Contra os constumes (*)` <- NULL
ocorr_natureza$grupo <- NULL
ocorr_natureza$Entorpecentes <- NULL

# reordena para mesclar com a tabela de descrição
ocorr_natureza <- ocorr_natureza %>% select("ano", "local", "pessoa", "patrimônio",
                                            "costumes", "entorpecentes", "contravencionais", "outros_criminais",
                                            "outros_delitos", "violentos", "total_delitos")

# cria tabela de descrição
descricao_natureza <- tibble(Cod = xlsx$Cod,
                             XLSX = xlsx$XLSX,
                             Variável = names(ocorr_natureza),
                             Descrição = c("Ano de registro das ocorrências",
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
trim <- rep(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)), 20)
ocorr_natureza$trimestre <- c(rep(3,3), rep(4,3), trim, rep(1,3))

# precisou reordenar de novo :(
ocorr_natureza <- ocorr_natureza %>% select("ano", "trimestre", "local", "pessoa", "patrimônio",
                                            "costumes", "entorpecentes", "contravencionais", "outros_criminais",
                                            "outros_delitos", "violentos", "total_delitos")

# e tira os caracteres indesejados
ocorr_natureza$local <- gsub("Gde", "Grande", ocorr_natureza$local)
ocorr_natureza$ano <- gsub("-1T", "", ocorr_natureza$ano)
ocorr_natureza$ano <- gsub("-2T", "", ocorr_natureza$ano)
ocorr_natureza$ano <- gsub("-3T", "", ocorr_natureza$ano)
ocorr_natureza$ano <- gsub("-4T", "", ocorr_natureza$ano)
ocorr_natureza$ano <- gsub("-T4", "", ocorr_natureza$ano)
ocorr_natureza$ano <- as.integer(ocorr_natureza$ano)

# Mais uma vez atualiza a tabela de descrição
descricao_natureza <- rbind(descricao_natureza[1,],
                      c("", "Ocorrência/período", "trimestre", "Trimestre de registro das ocorrências"),
                      descricao_natureza[2:11,])
descricao_natureza$Cod <- seq(1:12)
View(descricao_natureza)

# Agora salve e faça bom uso no report...
write_rds(descricao_natureza, "C:\\Users\\Raul\\Documents\\meu_projeto\\SUR SUR-lag SUR-error\\serie_trimestral_descricao_ocorrencias_por_natureza.rds")
write_rds(ocorr_natureza, "C:\\Users\\Raul\\Documents\\meu_projeto\\SUR SUR-lag SUR-error\\serie_trimestral_ocorrencias_por_natureza.rds")


#################################
#'
#'@Ocorrências_por_tipo
#'
#################################
rm(list=ls())
setwd("C:\\Users\\Raul\\Documents\\meu_projeto\\SUR SUR-lag SUR-error")
dir()

ocorr_tipo <- read_excel("tidy_agredados_ssp.xlsx", sheet="Plan2", na="-")
names(ocorr_tipo)

ocorr_tipo$período <- ocorr_tipo$`Ocorrência/período`
ocorr_tipo$local <-ocorr_tipo$`Ocorrências policiais registradas, por tipo`
ocorr_tipo$homicidio <-ocorr_tipo$`Homicídio doloso (i)`
ocorr_tipo$vitima_homicidio <-ocorr_tipo$`Nº de Vítimas em Homicídio Doloso`
ocorr_tipo$tentativa_homicidio <-ocorr_tipo$`Tentativa de homicídio`
ocorr_tipo$latrocinio <-ocorr_tipo$Latrocínio
ocorr_tipo$vimima_latrocinio <-ocorr_tipo$`Nº de Vítimas de Latrocínio`
ocorr_tipo$`Extorsão mediante seqüestro (5)` 


ocorr_tipo$`Ocorrência/período` <- NULL
ocorr_tipo$`Ocorrências policiais registradas, por tipo` <- NULL
ocorr_tipo$`Homicídio doloso (i)` <- NULL
ocorr_tipo$`Nº de Vítimas em Homicídio Doloso` <- NULL
ocorr_tipo$`Tentativa de homicídio` <- NULL
ocorr_tipo$Latrocínio <- NULL
ocorr_tipo$`Nº de Vítimas de Latrocínio` <- NULL
ocorr_tipo$Estupro <- NULL

names(ocorr_tipo)

descricao_tipo <- tibble(Variável = names(ocorr_tipo),
                         descrição = c("período",
                                       "interior, Gde Sp, Capital",
                                       "Ocorrências de crime contra pessoa",
                                       "Ocorrências de crime contra o patrimònio",
                                       "Ocorrências de crime contra os costumes (até 2009)/contra a dignidade sexual (2010-atual)",
                                       "Ocorrências de Tráfico de Entorpecentes",
                                       "Ocorrências de contravenções (https://goo.gl/QccSm2)",
                                       "Ocorrências de outros criminais - exceto contravenções",
                                       "Ocorências de Outros delitos - inclusive contravenções",
                                       "Total de crimes violentos (Homicidio Doloso, Roubo, Latrocínio, Estupro e EMS)",
                                       "Total de delitos",
                                       "Ocorrências policiais registradas, por natureza"))


View(ocorr_natureza)

ocorr_tipo <- read_excel("tidy_agredados_ssp.xlsx", sheet="Plan2", na="-")

