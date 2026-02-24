library(ggplot2)
library(tidyverse)

arquivo <- "20150101.txt"

kyoto01012015 <- read_delim(
  arquivo,
  delim = "\t",
  col_names = FALSE, 
  show_col_types = FALSE
)


kyoto01012015 <- kyoto01012015 |> rename(
       Duracao = X1,
       Servico = X2,
       Bytes_origem = X3,
       Bytes_destino = X4,
       Qtd = X5,
       Tx_msm_servico = X6,
       Tx_Serro = X7,
       Tx_Serro_servico = X8,
       Destino_qtd_host = X9,
       Destino_host_qtd_servico = X10,
       Destino_host_msm_tx_porta_origem = X11,
       Destino_host_tx_serro = X12,
       Destino_host_tx_serro_servico = X13,
       Flag = X14,
       Detec_IDS = X15,
       Detec_Malw = X16,
       Detec_Ashula = X17,
       Rotulo = X18,
       IP_Origem = X19,
       Porta_Origem = X20,
       IP_Destino = X21,
       Porta_Destino = X22,
       T_Comeco = X23,
       Protocolo = X24)

# Análise dos serviços
kyoto01012015 |>
  count(Servico, sort = TRUE) |>
  head(10) |>
  ggplot(aes(x = reorder(Servico, n), y = n)) +
  geom_col(fill = "#f0690a") +
  theme_minimal()+
  coord_flip()+
  labs(title = "10 serviços mais detectados", x="Serviço",y="Qtd")

# Análise de detecção pelo IDS (Intrusion Detection System)

kyoto01012015 |>
  count(Detec_IDS, sort = TRUE) |>
  head(10) |>
  ggplot(aes(x = reorder(Detec_IDS, n), y = n)) +
  geom_col(fill = "#f0690a") +
  theme_minimal()+
  coord_flip()+
  labs(title = "Códigos de deteção pelo IDS", x="Código",y="Qtd")

# Análise de detecção de malware 

kyoto01012015 |>
  count(Detec_Malw, sort = TRUE) |>
  head(10) |>
  ggplot(aes(x = reorder(Detec_Malw, n), y = n)) +
  geom_col(fill = "#f0690a") +
  theme_minimal()+
  coord_flip() +
  labs(title = "Malwares detectados", x="Tipo",y="Qtd")

# Heatmap

kyoto01012015|>
  count(Servico, Flag) |>
  ggplot(aes(x = Servico, y = Flag, fill = n))+
  geom_tile(color = "#ffffff")+
  scale_fill_viridis_c(trans = "log2")+ 
  theme_minimal()+
  labs(
    title = "Heatmap: Serviço vs. Estado da Conexão (Flags)",
    fill = "Frequência",
    x = "Serviço",
    y = "Flags"
  )
