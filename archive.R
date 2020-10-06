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

  # Archive folder name
  if (missing(folder)) {
    folder <- "Archive"
  }

  # Date cut off
  if (missing(date)) {
    date <- Sys.Date()
  }

  # Destination type 
  destination <- ifelse(length(str_split(folder, pattern = "/")[[1]])>1, "path", "subfolder")
  
  # Archive folder present
  if(destination == "path"){
    if (!(dir.exists(folder))) {
     dir.create(file.path(folder))
    }
  } else if (destination == "subfolder" & !(dir.exists(file.path(path, folder)))) {
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
  ifelse(destination == "path", 
         file.copy(from = files$paths, to = folder, overwrite = TRUE, recursive = TRUE,
                   copy.mode = TRUE, copy.date = TRUE),
         file.copy(from = files$paths, to = file.path(path,folder), overwrite = TRUE, recursive = TRUE,
                   copy.mode = TRUE, copy.date = TRUE))
  file.remove(files$paths)
}
