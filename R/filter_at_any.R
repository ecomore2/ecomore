#' Filter at at least
#'
#' This function filters a data frame so that at least one of the columns with a
#' name matching a given pattern fullfills a condition specified by a given
#' function.
#'
#' @param df a data frame of the structure of PACS data. It should contain at
#' least 4 Date columns: "onset", "hospitalization", "consultation" and
#' "sample_collection".
#' @pattern character string to be matched with sel.
#' @f function that codes the condition that at least one of the selected columns
#' should fullfills.
#' @sel one tidyselect function.
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
#'   ecomore::add_dates_differences() %>%
#'   filter_at_any("diff_", function(x) abs(x) > threshold) %>%
#'   select(id, onset, hospitalization, consultation, sample_collection)
#'
#' @importFrom dplyr %>% mutate mutate_at filter select starts_with rowwise do
#'
#' @export
filter_dates <- function(df, pattern, f, sel) {
  df %>%
    mutate_at(vars(starts_with(pattern)), f) %>%
    mutate(problem = select(., starts_with(pattern)) %>%
             rowwise() %>%
             do(a = any(is.na(.))) %>%
             unlist()) %>%
    filter(problem) %>%
    select(-problem)
}
