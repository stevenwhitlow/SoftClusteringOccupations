library(tidyverse)

setwd("/Users/steven/Documents/research/occupations/")
source("gom_estimation.R")

clean_for_gom()

out_final <- estimate_gom(gom_data, 968, 128, 7, 1.6, 1, 750, c(3, 2, 5, 1, 6, 4, 7))

extract_classifications(out_final, 'classifications.csv')
