library(ggplot2)
library(tidyverse)

dados <-read_csv("monteCarloGBM.csv", show_col_types = FALSE)

Acuracia <- ggplot(dados, aes(x = ite)) +
  geom_point(aes(y = Acuracia), color="darkgrey", alpha = 0.5)+
  geom_smooth(aes(y=MediaAcCumulativa),color="black", method = "gam", se = FALSE)+
  labs(title = "Acurácia média LightGBM (1000 iterações)", 
       subtitle = "Linha: média cumulativa      Pontos: média por iteração",
       x = "Iteração",
       y = "Acurácia")+
  theme_classic()

Tempo <- ggplot(dados,aes(x= ite)) +
  geom_point(aes(y=tExecS), color="lightblue", alpha = 0.5)+
  geom_smooth(aes(y=MediaTCumulativa), color = "blue", method = "gam", se = FALSE)+
  labs(title="Tempo de execução médio LightGBM (1000 iterações)",
       subtitle = "Linha: média cumulativa     Pontos: média por iteração",
       x = "Iteração",
       y = "Tempo (s)")+
  theme_classic()

histogramaTempo <- ggplot(dados, aes(x = tExecS)) +
  geom_histogram(fill = "purple", color = "white", bins = 20, alpha = 0.8) +
  labs(title = "Histograma tempo de execução",
       x = "Tempo de execução (s)",
       y = "Frequência") +
  theme_classic()

histogramaAcuracia <- ggplot(dados, aes(x = Acuracia)) +
  geom_histogram(fill = "purple", color = "white", bins = 30, alpha = 0.8) +
  labs(title = "Histograma acurácia",
       x = "Acurácia",
       y = "Frequência") +
  theme_classic()



Acuracia
Tempo
histogramaTempo
histogramaAcuracia
