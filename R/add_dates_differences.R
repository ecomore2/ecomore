#' Adding dates difference
#'
#' This function adds 6 dates difference between the 4 following dates variables:
#' "onset", "hospitalization", "consultation" and "sample_collection". The names
#' of these new variables are "onset_hospitalization", "onset_consultation", etc...
#'
#' @param df a data frame of the structure of PACS data. It should contain at
#' least 4 Date columns: "onset", "hospitalization", "consultation" and
#' "sample_collection".
#'
#' @author Marc Choisy
#'
#' @examples
#' pacs <- readr::read_csv("../../R code/pacs/data/pacs.csv",
#'                         col_types = paste(c("icfnD", rep("c", 5), rep("D", 4), rep("f", 3)), collapse = ""))
#' require(magrittr)
#' pacs %<>% add_dates_differences()
#'
#' @importFrom dplyr %>% mutate
#'
#' @export
add_dates_differences <- function(df) {
  df %>%
    mutate(onset_hospitalization             = as.integer(onset           - hospitalization),
           onset_consultation                = as.integer(onset           - consultation),
           onset_sample_collection           = as.integer(onset           - sample_collection),
           hospitalization_consultation      = as.integer(hospitalization - consultation),
           hospitalization_sample_collection = as.integer(hospitalization - sample_collection),
           consultation_sample_collection    = as.integer(consultation    - sample_collection))
}
