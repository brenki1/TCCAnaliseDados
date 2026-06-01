library(ggplot2)

dadosXG<-read_csv("monteCarloXG.csv", show_col_types=FALSE)
dadosPR<-read_csv("monteCarloPR.csv", show_col_types = FALSE)
dadosPP<-read_csv("monteCarloPP.csv", show_col_types = FALSE)

# -- XGBOOST --

AcuraciaXG <- ggplot(dadosXG, aes(x = ite)) +
  geom_point(aes(y = Acuracia), color="darkgrey", alpha = 0.5)+
  geom_smooth(aes(y=MediaAcCumulativa),color="black", method = "gam", se = FALSE)+
  labs(title = "Acurácia média XGBoost (1000 iterações)", 
       subtitle = "Linha: média cumulativa      Pontos: média por iteração",
       x = "Iteração",
       y = "Acurácia")+
  theme_classic()

TempoXG <- ggplot(dadosXG,aes(x= ite)) +
  geom_point(aes(y=tExecS), color="lightblue", alpha = 0.5)+
  geom_smooth(aes(y=MediaTCumulativa), color = "blue", method = "gam", se = FALSE)+
  labs(title="Tempo de execução médio XGBoost (1000 iterações)",
       subtitle = "Linha: média cumulativa     Pontos: média por iteração",
       x = "Iteração",
       y = "Tempo (s)")+
  theme_classic()

histogramaTempoXG <- ggplot(dadosXG, aes(x = tExecS)) +
  geom_histogram(fill = "purple", color = "white", bins = 10, alpha = 0.8) +
  labs(title = "Histograma tempo de execução",
       x = "Tempo de execução (s)",
       y = "Frequência") +
  theme_classic()

# -- Python SVM Radial --

AcuraciaPR <- ggplot(dadosPR, aes(x = ite)) +
  geom_point(aes(y = Acuracia), color="darkgrey", alpha = 0.5)+
  geom_smooth(aes(y=MediaAcCumulativa),color="black", method = "gam", se = FALSE)+
  labs(title = "Acurácia média SVM Radial (100 iterações)", 
       subtitle = "Linha: média cumulativa      Pontos: média por iteração",
       x = "Iteração",
       y = "Acurácia")+
  theme_classic()

TempoPR <- ggplot(dadosPR,aes(x= ite)) +
  geom_point(aes(y=tExecS), color="lightblue", alpha = 0.5)+
  geom_smooth(aes(y=MediaTCumulativa), color = "blue", method = "gam", se = FALSE)+
  labs(title="Tempo de execução médio SVM Radial (100 iterações)",
       subtitle = "Linha: média cumulativa     Pontos: média por iteração",
       x = "Iteração",
       y = "Tempo (s)")+
  theme_classic()

histogramaTempoPR <- ggplot(dadosPR, aes(x = tExecS)) +
  geom_histogram(fill = "purple", color = "white", bins = 10, alpha = 0.8) +
  labs(title = "Histograma tempo de execução",
       x = "Tempo de execução (s)",
       y = "Frequência") +
  theme_classic()

# -- Python SVM Polinomial -- 

AcuraciaPP <- ggplot(dadosPP, aes(x = ite)) +
  geom_point(aes(y = Acuracia), color="darkgrey", alpha = 0.5)+
  geom_smooth(aes(y=MediaAcCumulativa),color="black", method = "gam", se = FALSE)+
  labs(title = "Acurácia média SVM Polinomial (100 iterações)", 
       subtitle = "Linha: média cumulativa      Pontos: média por iteração",
       x = "Iteração",
       y = "Acurácia")+
  theme_classic()

TempoPP <- ggplot(dadosPP,aes(x= ite)) +
  geom_point(aes(y=tExecS), color="lightblue", alpha = 0.5)+
  geom_smooth(aes(y=MediaTCumulativa), color = "blue", method = "gam", se = FALSE)+
  labs(title="Tempo de execução médio SVM Polinomial (100 iterações)",
       subtitle = "Linha: média cumulativa     Pontos: média por iteração",
       x = "Iteração",
       y = "Tempo (s)")+
  theme_classic()

histogramaTempoPP <- ggplot(dadosPP, aes(x = tExecS)) +
  geom_histogram(fill = "purple", color = "white", bins = 10, alpha = 0.8) +
  labs(title = "Histograma tempo de execução",
       x = "Tempo de execução (s)",
       y = "Frequência") +
  theme_classic()


AcuraciaXG
TempoXG
histogramaTempoXG

AcuraciaPR
TempoPR
histogramaTempoPR

AcuraciaPP
TempoPP
histogramaTempoPP