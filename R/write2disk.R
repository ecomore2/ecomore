#' Writing data to disk
#'
#' This function writes data in CSV and RDS in a folder that will be `up` levels
#' up the current working directory.
#'
#' @param x object to save to disk.
#' @param folder name of the folder we want to write the files to.
#' @param filename name of the CSV and RDS files. Default value is NULL and, in
#' this case, the name of the file is the name of the object.
#' @param up integer, number of levels up the current working directy that
#' `folder` is. Default value is 2.
#'
#' @author Marc Choisy
#'
#' @examples
#' ## Writting mtcars to disk in a subdirectory test of the current working
#' ## directory:
#' write2disk(mtcars, "test", up = 0)
#' dir()
#'
#' @importFrom utils write.csv
#'
#' @export
write2disk <- function(x, folder, filename = NULL, up = 2) {
  if (is.null(filename)) filename <- as.character(as.list(match.call())$x)
  folder <- paste0(paste(rep("../", 2), collapse = ""), folder)
  if (! dir.exists(folder)) dir.create(folder)
  path <- paste0(folder, "/", filename)
  write.csv(x, paste0(path, ".csv"))
  saveRDS(x, paste0(path, ".rds"))
}
