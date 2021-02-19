library(tidyverse)

ESM_raw <-
  dir("inst/extdata/", full.names = T) %>%
  purrr::map(read_csv)

usethis::use_data(ESM_raw)

