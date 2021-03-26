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
    mutate(Activity = as.numeric(stringr::str_extract(file, "[0-9]+"))) %>%
    mutate_at(vars(starts_with("T_")), lubridate::as_datetime) %>%
    mutate(duration = T_Response - T_Issued) %>%
    pivot_longer(starts_with("["), names_to = "Item_raw", values_to = "Response") %>%
    mutate(response = as.numeric(str_match(Response, "[0-9]+")),
           Obs = row_number()) %>%
    select(Obs, Activity, Part, Device,
           T_Scheduled, T_Issued, T_Response,
           Item_raw, Response, response)
  class(out) <- c("tbl_esm_eth", class(out))
  out
}





#' Augment imported file with meta data
#'
#' adds participant and item level meta data
#' @param x csv file name
#' @param Items data frame item meta data
#' @param Parts data frame with traits
#' @return ESM table (tbl_esm)
#' @author Martin Schmettow
#' @import tidyverse
#' @export

augment <-
  function (x, ...) {
    UseMethod("augment", x)
  }

#' @rdname augment
#' @export

augment.tbl_esm_eth <- function(x, Parts = NULL, Items = NULL, ...){
  out <- x %>%
    group_by(Part, Item_raw) %>%
    arrange(T_Scheduled) %>%
    mutate(T_pos = row_number()) %>%
    ungroup()

  if(!is_null(Items))  {
    print("Joining Items")
    out <-
      out %>%
      left_join(Items, by = "Item_raw") %>%
      select(-Item_raw)}

  if(!is_null(Parts)) {
    out <-
      out %>%
      left_join(Parts, by = "Part")}

  class(out) <- c("tbl_esm", class(x))
  out
}


#' Normalizes item responses
#'
#' when augmentation meta data is available,
#' rating scale responses are reversed and scaled to a unit interval
#'
#' @param x csv file name
#' @param Items data frame item meta data
#' @param Parts data frame with traits
#' @return ESM table (tbl_esm)
#' @author Martin Schmettow
#' @import tidyverse
#' @export


normalize_Scales <- function(x) {
  out <-
    x %>%
    mutate(response = mascutils::rescale_unit(lower = min, upper = max),
           response = if_else(reverse, 1 - response,
                              response)) %>%
    select(-min, -max, -reverse)

}



#' @rdname read_ethica_csv
#' @export

print.tbl_esm <-
  function(x){
    n_Obs <- nrow(x)
    n_shown <- min(8, nrow(x))
    n_Part <- nrow(distinct(x,Part))
    n_Item <- nrow(distinct(x,Item))
    n_Scheduled <- nrow(distinct(x,T_Scheduled))
    n_Scales <- nrow(distinct(x,Scale))
    cap = stringr::str_c("Table: ESM data with ",
                         n_Obs, " observations (",
                         n_shown," shown), ",
                         n_Part, " participants and ",
                         n_Item, " items")
    print(cap)
    sample_n(x, 8) %>% base:::print.data.frame()
    invisible(x)
  }

#' Export empty meta data for scales and items
#'
#' Creates an empty table with filled in raw item labels.
#' @param tbl_esm_eth ESm table produced by read_ethica_csv
#' @return data frame
#' @author Martin Schmettow
#' @import tidyverse
#' @export

export_items_template <-
  function(tbl_esm_raw){
    tbl_esm_raw %>%
      select(Activity, Item_raw, response) %>%
      as_tibble() %>%
      group_by(Activity, Item_raw) %>%
      summarize(min = min(response, na.rm = T),
                max = max(response, na.rm = T)) %>%
      ungroup() %>%
      mutate(Inventory = "",
             Scale = "",
             Subscale = "",
             Item = "",
             Label = "",
             reverse = "") %>%
      select(Item_raw, Inventory, Scale, Subscale, Item, Label, reverse, min, max)
  }


