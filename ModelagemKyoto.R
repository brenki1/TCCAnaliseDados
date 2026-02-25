# Modelagem de um Sistema de Detecção de Intrusão (IDS) usando randomForest e árvores de decisão do R

library(ggplot2)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)
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

kyoto01012015$Rotulo <- as.factor(kyoto01012015$Rotulo)
kyoto01012015$Servico <- as.factor(kyoto01012015$Servico)
kyoto01012015$Protocolo <- as.factor(kyoto01012015$Protocolo)
kyoto01012015$Flag <- as.factor(kyoto01012015$Flag)

filtro <- c("Rotulo", "Duracao", "Servico", "Bytes_origem", "Bytes_destino","Qtd", "Tx_msm_servico", "Tx_Serro", "Tx_Serro_servico", "Destino_qtd_host", "Destino_host_qtd_servico", "Destino_host_msm_tx_porta_origem", "Destino_host_tx_serro", "Destino_host_tx_serro_servico", "Flag", "Protocolo")

kyotoFiltrada <- kyoto01012015[,filtro]
kyotoFiltrada <- na.omit(kyotoFiltrada)

n <- round(0.8*nrow(kyotoFiltrada))
set.seed(06112022)
indices_treino <- sample(1:nrow(kyotoFiltrada), size = n, replace = FALSE)

treino <- kyotoFiltrada[indices_treino,]
teste <- kyotoFiltrada[-indices_treino,]

treino$Rotulo <- as.factor(treino$Rotulo)
treino$Servico <- as.factor(treino$Servico)
treino$Protocolo <- as.factor(treino$Protocolo)
treino$Flag <- as.factor(treino$Flag)

teste$Rotulo <- as.factor(teste$Rotulo)
teste$Servico <- as.factor(teste$Servico)
teste$Protocolo <- as.factor(teste$Protocolo)
teste$Flag <- as.factor(teste$Flag)

# --- FLORESTA ALEATÓRIA ---

floresta <- randomForest(formula = Rotulo ~ ., data = treino, ntree=200)

floresta

previsao.floresta <- predict(floresta, newdata = teste)

previsao.floresta
mean(previsao.floresta == teste$Rotulo)

importancia <- as.data.frame(importance(floresta))
importancia$Variavel <- rownames(importancia)

ggplot(importancia, aes(x = reorder(Variavel, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "#4b059c") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Importância das variáveis na Floresta Aleatória",
    x = "variável",
    y = "importância (mean decrease gini)"
  )

rpart.plot(floresta)

# --- ÁRVORE DE DECISÃO ---

arvore <- rpart(formula = Rotulo ~., data= treino)
previsao.arvore <- predict(arvore, newdata=teste, type="class")

mean(previsao.arvore == teste$Rotulo)
rpart.plot(arvore, type=3, extra=101, fallen.leaves=TRUE)

# neste caso, 1 = normal, -1 = ataque!

# --- CONCLUSÃO --- 

print("Nessas condições, a floresta teve uma acurácia de 99,69956%. Enquanto a árvore 98,56076%")
