library(e1071)
library(class)
library(tidyverse)

dados <- c("20150101.txt")

kyoto <- read_delim(
  dados,
  delim = "\t",
  col_names = FALSE, 
  show_col_types = FALSE
)

kyoto <- kyoto |> rename(
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

kyoto <- kyoto |>
  filter(Rotulo != -2)

kyoto$Rotulo <- as.factor(kyoto$Rotulo)
kyoto$Servico <- as.factor(kyoto$Servico)
kyoto$Protocolo <- as.factor(kyoto$Protocolo)
kyoto$Flag <- as.factor(kyoto$Flag)

filtro <- c("Rotulo","Duracao","Servico","Bytes_origem","Bytes_destino","Qtd","Destino_qtd_host","Destino_host_qtd_servico","Destino_host_tx_serro","Flag","Protocolo")

kyotoFiltrada <- kyoto[,filtro]
kyotoFiltrada <- na.omit(kyotoFiltrada)

n <- round(0.8*nrow(kyotoFiltrada))
set.seed(895769)
indices_treino <- sample(1:nrow(kyotoFiltrada), size = n, replace = FALSE)

treino <- kyotoFiltrada[indices_treino,]
teste <- kyotoFiltrada[-indices_treino,]

treino$Rotulo <- as.factor(ifelse(treino$Rotulo == -1, "Ataque", "Normal"))
teste$Rotulo  <- as.factor(ifelse(teste$Rotulo == -1, "Ataque", "Normal"))

indices_reduzido <- sample(1:nrow(treino), size = 100000, replace = FALSE)
treino_reduzido <- treino[indices_reduzido, ]

inicio <- Sys.time()

modeloSVM <- svm(
  formula = Rotulo ~ .,
  data = treino_reduzido,
  type = "C-classification",
  kernel = "radial",
  scale = TRUE
)
fim <- Sys.time()

print(format(fim - inicio))

previsao <- predict(modeloSVM, teste)

res <- table(Previsao = previsao, Real = teste$Rotulo)

print(res)

acuracia <- mean(previsao == teste$Rotulo)
print(paste("Acurácia: ", acuracia))
 