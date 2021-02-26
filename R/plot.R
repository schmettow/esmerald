#' Overview plot
#'
#' returns a long ESM table
#' @param x tbl_esm
#' @return None
#' @author Martin Schmettow
#' @import tidyverse
#' @export

plot.tbl_esm <- function(x)
  group_by(Part, Scale, T_Scheduled) %>%
  summarize(mean_resp = mean(response, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = T_Scheduled,
             y = mean_resp,
             col = Scale)) +
  geom_jitter(alpha = .3) +
  geom_smooth(se = F) +
  facet_wrap(Part ~ ., scales = "free_x")
