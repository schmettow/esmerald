#' Import Ethica CSV file
#'
#' returns a long ESM table
#' @param file csv file name
#' @return ESM table (tbl_esm)
#' @author Martin Schmettow
#' @import tidyverse
#' @export


read_ethica_csv <- function(file)
{
  out <-
    read_csv(file, col_types = cols()) %>%
    rename(Part = Name,
           Device = `Device ID`,
           T_Scheduled = `Scheduled Time`,
           T_Issued = `Issued Time`,
           T_Response = `Response Time`) %>%
    mutate(Action = file) %>%
    mutate_at(vars(starts_with("T_")), lubridate::as_datetime) %>%
    mutate(duration = T_Response - T_Issued) %>%
    pivot_longer(starts_with("["), names_to = "Item_raw", values_to = "Response") %>%
    mutate(response = as.numeric(str_match(Response, "[0-9]+")),
           Obs = row_number()) %>%
    select(Obs, Action, Part, Device,
           T_Scheduled, T_Issued, T_Response,
           Item_raw, Response, response)
  class(out) <- c("tbl_esm", class(out))
  out
}


#' @rdname read_ethica_csv
#' @export

print.tbl_esm <-
  function(x){
    n_Obs <- nrow(x)
    n_shown <- min(8, nrow(x))
    n_Part <- nrow(distinct(x,Part))
    n_Item <- nrow(distinct(x,Item_raw))
    n_Scheduled <- nrow(distinct(x,T_Scheduled))
    cap = stringr::str_c("Table: ESM data with ",
                         n_Obs, " observations (",
                         n_shown," shown), ",
                         n_Part, " participants and ",
                         n_Item, " items")
    print(cap)
    sample_n(x, 8) %>% base:::print.data.frame()
    invisible(x)
  }

#' @rdname read_ethica_csv
#' @export

export_item_labels <- function(tbl_esm){
    tbl_esm %>%
    select(Item_raw) %>%
    distinct() %>%
    mutate(Scale = "",
           Item = "",
           Label = "",
           min = "",
           max = "",
           reverse = T)
  }

