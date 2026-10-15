library(pROC)
library(ggplot2)
library(dplyr)
library(knitr)
library(gridExtra)
library(grid)

kyotoROC <- read.csv("kyotoROC.csv")
unswROC  <- read.csv("unswROC.csv")
adfaROC  <- read.csv("adfaROC.csv")
metricas  <- read.csv("metricas.csv")


metricasFormat <- metricas |>
  mutate(across(c(Acuracia, Precisao, Recall, F1_Score), ~ round(., 4)))

print(kable(metricasFormat, format = "markdown"))

plotROC <- function(dados, dataset) {
  
  rocSVM  <- roc(dados$y_true, dados$prob_svm, quiet = TRUE)
  rocXGB  <- roc(dados$y_true, dados$prob_xgb, quiet = TRUE)
  rocLGBM <- roc(dados$y_true, dados$prob_lgbm, quiet = TRUE)
  
  aucSVM  <- round(auc(rocSVM), 4)
  aucXGB  <- round(auc(rocXGB), 4)
  aucLGBM <- round(auc(rocLGBM), 4)
  
  legSVM  <- paste0("SVM (AUC = ", aucSVM, ")")
  legXGB  <- paste0("XGBoost (AUC = ", aucXGB, ")")
  legLGBM <- paste0("LightGBM (AUC = ", aucLGBM, ")")
  

  lROC <- list()
  lROC[[legSVM]]  <- rocSVM
  lROC[[legXGB]]  <- rocXGB
  lROC[[legLGBM]] <- rocLGBM
  
  grafico <- ggroc(lROC, size = 1) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("ROC - ", dataset),
      x = "Taxa de Falsos Positivos",
      y = "Taxa de Verdadeiros Positivos",
      color = "Modelos"
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.text = element_text(size = 11)
    ) +

    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color = "grey50", linetype = "dashed")
  print(grafico)
}

plotROC(kyotoROC, "Kyoto")
plotROC(unswROC, "UNSW-NB15")
plotROC(adfaROC, "ADFA-LD")

# Plot da tabela

#metricasFormat <- metricas
#metricasFormat[, 3:7] <- round(metricasFormat[, 3:7], 4)
#grid.newpage()
#grid.table(metricasFormat)