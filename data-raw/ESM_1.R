library(tidyverse)

CSV <- dir("inst/extdata/", full.names = T)[1:2]
adply()


read_csv(file = "inst/extdata/survey_responses_5715.csv")

dir("inst/extdata/", full.names = T)[1] %>%
  plyr::adply(read_csv)

