rm(list=ls())

library(tidyverse)
library(readxl)

#################################
#'
#'@Ocorrências_por_natureza #####
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

# leia a planilha e guarde os nomes
ocorr_tipo <- read_excel("tidy_agredados_ssp.xlsx", sheet="Plan2", na="-")
xlsx <- names(ocorr_tipo)
xlsx <- c(rep(xlsx[1],2), xlsx[2:16]) # é preciso desmembrar o ano do trimestre

# ajustando os nomes, com a função transmute() fica bem mais fácil! :)
ocorr_tipo <- ocorr_tipo %>% transmute(ano = `Ocorrência/período`,
                                       trimestre = `Ocorrência/período`,
                                       local = `Ocorrências policiais registradas, por tipo`,
                                       homicidio = `Homicídio doloso (i)`,
                                       vitima_homicidio = `Nº de Vítimas em Homicídio Doloso`,
                                       tentativa_homicidio = `Tentativa de homicídio`,
                                       latrocinio = Latrocínio,
                                       vitima_latrocinio = `Nº de Vítimas de Latrocínio`,
                                       estupro = Estupro,
                                       extorsao_med_sequestro = `Extorsão mediante seqüestro (5)`,
                                       trafico = `Tráfico de entorpecentes`,
                                       roubo = `Roubo - outros (6) (i)`,
                                       roubo_veiculo = `Roubo de veículos`,
                                       roubo_banco = `Roubo a Banco`,
                                       roubo_carga = `Roubo de Carga`,
                                       furto = `Furto - outros`,
                                       furto_veiculo = `Furto de veículos`)

# Limpando valores de local, ano e trimestre...
trim <- rep(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)), 20)
ocorr_tipo$trimestre <- c(rep(3,3), rep(4,3), trim, rep(1,3))

ocorr_tipo$local <- gsub("Gde", "Grande", ocorr_tipo$local)
ocorr_tipo$ano <- gsub("-1T", "", ocorr_tipo$ano)
ocorr_tipo$ano <- gsub("-2T", "", ocorr_tipo$ano)
ocorr_tipo$ano <- gsub("-3T", "", ocorr_tipo$ano)
ocorr_tipo$ano <- gsub("-4T", "", ocorr_tipo$ano)
ocorr_tipo$ano <- gsub("-T4", "", ocorr_tipo$ano)
ocorr_tipo$ano <- as.integer(ocorr_tipo$ano)

# criando uma tabela de descrição...
descricao_tipo <- tibble(Cod = 1:17,
                         XLSX = xlsx,
                         Variável = names(ocorr_tipo),
                         Descrição = NA)


# Agora salve e faça bom uso no report...
write_rds(descricao_tipo, "C:\\Users\\Raul\\Documents\\meu_projeto\\SUR SUR-lag SUR-error\\serie_trimestral_descricao_ocorrencias_por_tipo.rds")
write_rds(ocorr_tipo, "C:\\Users\\Raul\\Documents\\meu_projeto\\SUR SUR-lag SUR-error\\serie_trimestral_ocorrencias_por_tipo.rds")

