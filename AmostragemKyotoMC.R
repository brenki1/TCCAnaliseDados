library(xgboost)
library(tidyverse)
library(class)

ano <- "2012"
mes <- "07"

caminho <- paste0("^", ano, mes, ".*\\.txt$")
arquivos <- list.files(path = "dias", pattern = caminho, full.names = TRUE)

dados <- list()

for(arq in arquivos) {
  dia <- read_delim(
    arq,
    delim = "\t",
    col_names = FALSE,
    show_col_types = FALSE,
    col_types = cols(.default = "c")
  )
  
  dia <- dia |> rename(
    Duracao = X1, Servico = X2, Bytes_origem = X3, Bytes_destino = X4, Qtd = X5,
    Tx_msm_servico = X6, Tx_Serro = X7, Tx_Serro_servico = X8, Destino_qtd_host = X9,
    Destino_host_qtd_servico = X10, Destino_host_msm_tx_porta_origem = X11,
    Destino_host_tx_serro = X12, Destino_host_tx_serro_servico = X13, Flag = X14,
    Detec_IDS = X15, Detec_Malw = X16, Detec_Ashula = X17, Rotulo = X18,
    IP_Origem = X19, Porta_Origem = X20, IP_Destino = X21, Porta_Destino = X22,
    T_Comeco = X23, Protocolo = X24
  )
  
  dia <- dia |> filter(Rotulo != -2)
  dados[[length(dados) + 1]] <- dia
}

iteracoes <- 1000

vetor_acuracia <- numeric(iteracoes)
vetor_tempo <- numeric(iteracoes)
vetor_acuracia_cumulativa <- numeric(iteracoes)
vetor_tempo_cumulativo <- numeric(iteracoes)

filtro <- c("Rotulo", "Duracao", "Servico", "Bytes_origem", "Bytes_destino",
            "Qtd", "Destino_qtd_host", "Destino_host_qtd_servico", 
            "Destino_host_tx_serro", "Flag", "Protocolo")

for(i in 1:iteracoes) {
  
  set.seed(as.integer(Sys.time()) + i * 137)
  
  leitura <- list()
  
  for(dia in dados) {
    qtdAmostras <- sample(10000:15000, size = 1)
    qtdAmostras <- min(qtdAmostras, nrow(dia))
    
    if(qtdAmostras > 0) {
      indicesDia <- sample(1:nrow(dia), size = qtdAmostras, replace = FALSE)
      amostra <- dia[indicesDia,]
      leitura[[length(leitura)+1]] <- amostra
    }
  }
  
  kyoto <- bind_rows(leitura)
  kyoto <- type.convert(kyoto, as.is = TRUE)
  
  kyoto$Rotulo <- as.factor(kyoto$Rotulo)
  kyoto$Servico <- as.factor(kyoto$Servico)
  kyoto$Protocolo <- as.factor(kyoto$Protocolo)
  kyoto$Flag <- as.factor(kyoto$Flag)
  
  kyotoFiltrada <- kyoto[,filtro]
  kyotoFiltrada <- na.omit(kyotoFiltrada)
  
  n <- round(0.8*nrow(kyotoFiltrada))
  indices_treino <- sample(1:nrow(kyotoFiltrada), size = n, replace = FALSE)
  
  treino <- kyotoFiltrada[indices_treino,]
  teste <- kyotoFiltrada[-indices_treino,]
  
  X_treino <- model.matrix(Rotulo ~ . -1, data = treino)
  X_teste  <- model.matrix(Rotulo ~ . -1, data = teste)
  
  y_treino <- as.factor(ifelse(treino$Rotulo == -1, 1, 0))
  y_teste  <- as.factor(ifelse(teste$Rotulo == -1, 1, 0))
  

  y_teste  <- as.factor(teste$Rotulo == "-1")
  
  inicio <- Sys.time()
  modeloXG <- xgboost(
    x = X_treino,
    y = y_treino,
    max_depth = 5,
    learning_rate = 0.6,
    nrounds = 2000,
    nthreads = 16,
    objective = "binary:logistic"
  )

  fim <- Sys.time()
  
  tempoExec <- as.numeric(difftime(fim, inicio, units = "secs"))
  
  probabilidades <- predict(modeloXG, newdata = X_teste)
  previsoes <- ifelse(probabilidades > 0.5, 1, 0)
  acuracia <- mean(previsoes == y_teste)
  
  vetor_acuracia[i] <- acuracia
  vetor_tempo[i] <- tempoExec
  vetor_acuracia_cumulativa[i] <- mean(vetor_acuracia[1:i])
  vetor_tempo_cumulativo[i] <- sum(vetor_tempo[1:i])
}

resultados_finais <- data.frame(
  ite = 1:iteracoes,
  Acuracia = vetor_acuracia,
  TExecS = vetor_tempo,
  MediaAcCumulativa = vetor_acuracia_cumulativa,
  MediaTCumulativa = vetor_tempo_cumulativo
)

write.csv(resultados_finais, "resultados_monteCarlo_amostragem.csv", row.names = FALSE)
