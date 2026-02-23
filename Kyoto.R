library(ggplot2)
library(tidyverse)

arquivo <- "20150101.txt"

kyoto01012015 <- read_delim(
  arquivo,
  delim = "\t",
  col_names = FALSE, 
  show_col_types = FALSE
)

sum(kyoto01012015)

kyoto01012015 <- kyoto01012015 %>% rename(
  
)
