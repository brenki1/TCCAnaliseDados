library(xgboost)
library(class)
library(tidyverse)

dados <- c("20150101.txt", "20150102.txt")

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

kyoto <- kyoto|>
  filter(Rotulo != -2)

kyoto$Rotulo <- as.factor(kyoto$Rotulo)
kyoto$Servico <- as.factor(kyoto$Servico)
kyoto$Protocolo <- as.factor(kyoto$Protocolo)
kyoto$Flag <- as.factor(kyoto$Flag)

filtro <- c("Rotulo", "Duracao", "Servico", "Bytes_origem", "Bytes_destino","Qtd","Destino_qtd_host", "Destino_host_qtd_servico", "Destino_host_tx_serro", "Flag", "Protocolo")

kyotoFiltrada <- kyoto[,filtro]
kyotoFiltrada <- na.omit(kyotoFiltrada)

n_simulacoes <- 1000
tempos_execucao <- numeric(n_simulacoes)
acuracias <- numeric(n_simulacoes)
vetor_medias_tempo <- numeric(n_simulacoes)
vetor_medias_acuracia <- numeric(n_simulacoes)

n <- round(0.8 * nrow(kyotoFiltrada))

for (i in 1:n_simulacoes) {
  set.seed(895769 + i)
  
  indices_treino <- sample(1:nrow(kyotoFiltrada), size = n, replace = FALSE)
  
  treino <- kyotoFiltrada[indices_treino,]
  teste <- kyotoFiltrada[-indices_treino,]
  
  X_treino <- model.matrix(Rotulo ~ . -1, data = treino)
  X_teste  <- model.matrix(Rotulo ~ . -1, data = teste)
  
  y_treino <- as.factor(ifelse(treino$Rotulo == -1, 1, 0))
  y_teste  <- as.factor(ifelse(teste$Rotulo == -1, 1, 0))
  
  inicio <- Sys.time()
  modeloXG <- xgboost(
    x = X_treino,
    y = y_treino,
    max_depth = 5,
    learning_rate = 0.6,
    nrounds = 2000,
    device = "cuda",
    tree_method = "hist",
    nthreads = 16,
    objective = "binary:logistic",
  )
  fim <- Sys.time()
  
  tempos_execucao[i] <- as.numeric(difftime(fim, inicio, units = "secs"))
  
  probabilidades <- predict(modeloXG, newdata = X_teste)
  previsoes <- as.factor(ifelse(probabilidades > 0.5, 1, 0))
  
  acuracias[i] <- mean(previsoes == y_teste)
  
  vetor_medias_tempo[i] <- mean(tempos_execucao[1:i])
  vetor_medias_acuracia[i] <- mean(acuracias[1:i])
}

resultados <- data.frame(
  Simulacao = 1:n_simulacoes,
  Tempo_Execucao_Segundos = tempos_execucao,
  Media_Tempo_Cumulativa = vetor_medias_tempo,
  Acuracia = acuracias,
  Media_Acuracia_Cumulativa = vetor_medias_acuracia
)

write.csv(resultados, "resultados_monte_carlo.csv", row.names = FALSE)