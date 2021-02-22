#' Overview plot
#'
#' returns a long ESM table
#' @param x tbl_esm
#' @return None
#' @author Martin Schmettow
#' @import tidyverse
#' @export

plot.tbl_esm <- function(x)
  x %>%
  group_by(Part) %>%
  arrange(T_Response) %>%
  mutate(order_resp = row_number()) %>%
  ggplot(aes(x = order_resp, y = response, col = Action)) +
  geom_jitter(alpha = .2) +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(~Part)
