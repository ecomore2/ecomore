#' Generates corrected onset dates
#'
#' This generates corrected onset dates from a data frame that contains 4 dates
#' columns: "onset", "hospitalization", "consultation", "sample_collection".
#'
#' @param df a data frame of the structure of PACS data. It should contain at
#' least 4 Date columns: "onset", "hospitalization", "consultation" and
#' "sample_collection".
#'
#' @return a vector of dates.
#'
#' @author Marc Choisy
#'
#' @examples
#' require(dplyr)
#'
#' pacs <- readr::read_csv("../../R code/pacs/data/pacs.csv",
#'                         col_types = paste(c("icfnD", rep("c", 5), rep("D", 4), rep("f", 3)), collapse = ""))
#'
#' pacs %>%
#'   mutate(onset2 = correct_onset(.))
#'
#' @importFrom dplyr %>% select_at vars starts_with if_else
#'
#' @export
correct_onset <- function(df) {
  corrections <- df %>%
    add_dates_differences() %>%
    select_at(vars(starts_with("diff_onset"))) %>%
    sapply(mean, na.rm = TRUE) %>%
    round()

  with(df, if_else(is.na(onset),
                   if_else(is.na(hospitalization),
                           if_else(is.na(consultation),
                                   sample_collection + corrections["diff_onset_sample_collection"],
                                   consultation + corrections["diff_onset_consultation"]),
                           hospitalization + corrections["diff_onset_hospitalization"]),
                   onset))
}
