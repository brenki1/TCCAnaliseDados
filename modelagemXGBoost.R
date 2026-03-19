library(xgboost)
library(class)
library(tidyverse)

dados <- "20150101.txt"

kyoto01012015 <- read_delim(
  dados,
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

kyoto01012015 <- kyoto01012015|>
  filter(Rotulo != -2)

kyoto01012015$Rotulo <- as.factor(kyoto01012015$Rotulo)
kyoto01012015$Servico <- as.factor(kyoto01012015$Servico)
kyoto01012015$Protocolo <- as.factor(kyoto01012015$Protocolo)
kyoto01012015$Flag <- as.factor(kyoto01012015$Flag)

filtro <- c("Rotulo", "Duracao", "Servico", "Bytes_origem", "Bytes_destino","Qtd", "Tx_msm_servico", "Tx_Serro", "Tx_Serro_servico", "Destino_qtd_host", "Destino_host_qtd_servico", "Destino_host_msm_tx_porta_origem", "Destino_host_tx_serro", "Destino_host_tx_serro_servico", "Flag", "Protocolo")

kyotoFiltrada <- kyoto01012015[,filtro]
kyotoFiltrada <- na.omit(kyotoFiltrada)

n <- round(0.8*nrow(kyotoFiltrada))
set.seed(895769)
indices_treino <- sample(1:nrow(kyotoFiltrada), size = n, replace = FALSE)

treino <- kyotoFiltrada[indices_treino,]
teste <- kyotoFiltrada[-indices_treino,]

X_treino <- model.matrix(Rotulo ~ . -1, data = treino)
X_teste  <- model.matrix(Rotulo ~ . -1, data = teste)

y_treino <- as.factor(ifelse(treino$Rotulo == -1, 1, 0))
y_teste  <- as.factor(ifelse(teste$Rotulo == -1, 1, 0))

modeloXG <- xgboost(
  x = X_treino,
  y = y_treino,
  max_depth = 5,
  learning_rate = 0.5,
  nrounds = 2000,
  nthreads = 11,
  objective = "binary:logistic"
)

probabilidades <- predict(modeloXG, newdata = X_teste)
previsoes <- as.factor(ifelse(probabilidades > 0.5, 1, 0))
table(previsoes,teste$Rotulo)
acuracia <- mean(previsoes == y_teste)
print(paste("Acurácia final:", acuracia))

