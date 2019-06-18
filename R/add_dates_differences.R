#' Adding dates difference
#'
#' This function adds 6 dates difference between the 4 following dates variables:
#' "onset", "hospitalization", "consultation" and "sample_collection". The names
#' of these new variables all start with "diff_": "diff_onset_hospitalization",
#' "diff_onset_consultation", etc...
#'
#' @param df a data frame of the structure of PACS data. It should contain at
#' least 4 Date columns: "onset", "hospitalization", "consultation" and
#' "sample_collection".
#'
#' @return the data frame df augmented with 6 columns.
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
    mutate(diff_onset_hospitalization             = as.integer(onset           - hospitalization),
           diff_onset_consultation                = as.integer(onset           - consultation),
           diff_onset_sample_collection           = as.integer(onset           - sample_collection),
           diff_hospitalization_consultation      = as.integer(hospitalization - consultation),
           diff_hospitalization_sample_collection = as.integer(hospitalization - sample_collection),
           diff_consultation_sample_collection    = as.integer(consultation    - sample_collection))
}
