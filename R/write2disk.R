#' Writing data to disk
#'
#' This function writes data in CSV and RDS in a folder that will be `up` levels
#' up the current working directory.
#'
#' @param x object to save to disk.
#' @param folder name of the folder we want to write the files to.
#' @param up integer, number of levels up the current working directy that
#' `folder` is. Default value is 2.
#'
#' @author Marc Choisy
#' @export
write2disk <- function(x, folder, up = 2) {
  filename <- as.character(as.list(match.call())$x)
  folder <- paste0(paste(rep("../", 2), collapse = ""), folder)
  if (! dir.exists(folder)) dir.create(folder)
  path <- paste0(folder, "/", filename)
  write.csv(x, paste0(path, ".csv"))
  saveRDS(x, paste0(path, ".rds"))
}
