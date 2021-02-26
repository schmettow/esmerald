#' ESM data summary
#'
#' @param x
#'
#' @return data frame
#' @export

summary.tbl_esm <-
  function(x){
    x %>%
      group_by(Part, Scale) %>%
      summarize(mean = mean(response, na.rm = T),
                sd = sd(response, na.rm = T),
                N = n())

}
