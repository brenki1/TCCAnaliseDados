library(ggplot2)
library(tidyverse)

arquivo <- "20150101.txt"

kyoto01012015 <- read_delim(
  arquivo,
  delim = "\t",
  col_names = FALSE, 
  show_col_types = FALSE
)


kyoto01012015 <- kyoto01012015 %>% rename( 
       Duracao = X1,
       Servico = X2,
       Bytes_fonte = X3,
       Bytes_destino = X4,
       Qtd = X5,
       Tx_msm_servico = X6,
       Tx_Serro = X7,
       Tx_Serro_servico = X8,
       Destino_qtd_host = X9,
       Destino_host_qtd_servico = X10)





