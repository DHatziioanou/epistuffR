#' Archives files from a folder based on modification date.
#'
#'
#' @param path Path to folder containing files to archive.
#'
#' @param folder Folder name to archive files to. Default is Archive
#'
#' @param date Minimum modification date to exclude files from archiving. Default is Sys.Date().
#'
#' @return Moves older files based on date to an archive subfolder
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords archive
#'
#' @examples
#' 
#' archive("C:..../analysis")
#' 
#' 
#' @export
archive <- function(path, folder, date) {
  if (!require("filesstrings")) {
    install.packages("filesstrings")
    library(filesstrings)
  }
  # Archive folder name
  if (missing(folder)) {
    folder <- "Archive"
  }
  
  # Date cut off
  if (missing(date)) {
    date <- Sys.Date()
  }
  
  # Make Archive folder
  if (!(dir.exists(file.path(path, folder)))) { 
    dir.create(file.path(path, folder))
  }  
  
  # Files to archive
  files <- list.files(path = path, recursive = F)
  files <- as.data.frame(files)
  files$files <- as.character(files$files)
  files$Modified <- file.info(file.path(path, files[,1]))$ctime
  files$paths  <- file.path(path, files$file)
  files <- files[files$Modified < date & 
                   !(files$paths %in% list.dirs(path, recursive = F)),]
  # Move files 
  filesstrings::file.move(files = files$paths, destinations = file.path(path,folder), overwrite = TRUE)
  
}  
