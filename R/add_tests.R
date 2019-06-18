#' Adding test information to the PACS data frame
#'
#' This funtion adds 2 columns to a data frame of PACS structure: one column
#' named "tested" informing whether a test (either PCR, NS1 or serotype) has
#' been performed on the sample and a column named "confirmed" indicating whether
#' the sample is confirmed positive.
#'
#' @param df a data frame of the structure of PACS data. It should contain at
#' least 3 columns: "pcr" and "ns1" for which positive and negative should be
#' coded "positive" and "negative" respectively and "serotype" where dengue
#' serotype x should be coded by a character string containing "dengue", with all
#' other values not containing this pattern.
#'
#' @return the data frame df augmented with 2 columns.
#'
#' @author Marc Choisy
#'
#' @examples
#' pacs <- readr::read_csv("../../R code/pacs/data/pacs.csv",
#'                         col_types = paste(c("icfnD", rep("c", 5), rep("D", 4), rep("f", 3)), collapse = ""))
#' require(magrittr)
#' pacs %<>% adds_test()
#'
#' @importFrom dplyr %>% mutate
#'
#' @export
add_tests <- function(df) {
  df %>%
    mutate(tested    = pcr == "positive" | ns1 == "positive" | (pcr == "negative" & ns1 == "negative") | grepl("dengue", serotype),
           tested    = ifelse(is.na(tested), FALSE, tested),
           confirmed = pcr == "positive" | ns1 == "positive" | grepl("dengue", serotype),
           confirmed = ifelse(tested, confirmed, NA))
}


