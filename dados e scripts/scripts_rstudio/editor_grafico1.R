rm(list=ls())

estado_sp <- readr::read_rds("C:\\Users\\Raul\\Documents\\meu_projeto\\dados e scripts\\tabelas_output\\tab_compara_txcrime_estadoSP.rds")

# Gerando variáveis ####
estado_sp$tx_roubo     <- (estado_sp$roubo_veiculo/estado_sp$populacao)*100000
estado_sp$tx_furto     <- (estado_sp$furto_veiculo/estado_sp$populacao)*100000
estado_sp$tx_homicidio <- (estado_sp$homicidio/estado_sp$populacao)*100000
estado_sp$ano          <- lubridate::year(lubridate::make_date(estado_sp$ano))
estado_sp              <- plm::pdata.frame(estado_sp, index=c("ano", "local"))


# Gráfico de tx de homicidios ####
plot_homicidio <- ggplot2::ggplot(data=estado_sp,
                                  mapping=ggplot2::aes(x     = ano,
                                                       y     = tx_homicidio,
                                                       color = local,
                                                       group = local)) + 
                       ggplot2::geom_line() +
                       ggplot2::geom_point() +
                       ggplot2::theme_classic() +
                       ggplot2::xlab(NULL) +
                       ggplot2::theme(legend.position = c(.9, 1)) +
                       ggplot2::labs(title    = "Taxa de homicídios (100000 hab)",
                                     subtitle = "Estado de São Paulo (2000 até 2010)",
                                     y        = "Taxa de homicídio",
                                     x        = "Ano",
                                     color    = "Região",
                                     caption  = "Fonte: Elaboração própria a partir de dados da SSP/SP")

# Gráfico com tx de roubo de veiculo #####
plot_roubo    <- ggplot2::ggplot(data    = estado_sp,
                                 mapping = ggplot2::aes(x     = ano,
                                                        y     = tx_roubo,
                                                        group = local,
                                                        color = local)) +
                       ggplot2::geom_line() +
                       ggplot2::geom_point() +
                       ggplot2::theme_classic() +
                       ggplot2::theme(legend.position = "none") +
                       ggplot2::xlab(NULL) +
                       ggplot2::labs(title    = "Taxa de roubo e furto de veículo (100000 hab)",
                                     subtitle = "Estado de São Paulo (2000 até 2010)",
                                     y        = "Taxa de roubo de veículo")

# Gráfico com tx de furto de veiculo ####
plot_furto    <- ggplot2::ggplot(data    = estado_sp,
                                 mapping = ggplot2::aes(x     = ano,
                                                        y     = tx_furto,
                                                        group = local,
                                                        color = local)) +
                       ggplot2::geom_line() +
                       ggplot2::theme_classic() +
                       ggplot2::geom_point() +
                       ggplot2::theme(legend.position = "none") +
                       ggplot2::labs(title    = "",
                                     subtitle = "",
                                     y        = "Taxa de roubo de veículo",
                                     x        = "Ano",
                                     caption  = "Fonte: Elaboração própria a partir de dados da SSP/SP")

# colocando num grid
grid::grid.newpage()
grid::grid.draw(rbind(ggplot2::ggplotGrob(plot_homicidio),
                      ggplot2::ggplotGrob(plot_roubo),
                      ggplot2::ggplotGrob(plot_furto),
                      size = "last"))

 