#' Retrieve most recent file from a file path
#'
#' Retrieves the latest file name and modification details  for a specified path with the option to only look include files with a defined name string and exclude files with other defined name strings. The function can be set to attempt file retrieval a defined number of times to overcome server connectivity problems.
#'
#' @param folder_path Folder path containing the file to retrieve. Folders within this path will not be searched.
#'
#' @param file_string Character string within the file names to retrieve (optional). If not defined all file names within the folder are retrieved.
#'
#' @param exclusions Character string within the file names for file exclusion (optional). If not defined nothing is excluded from the retrieved file name list.
#'
#' @param return_type Use "all" to retrieve a data.frame with the file name, file path and modiciation date, "name" to retrieve the file name or "path" to retrieve the full file path. Default is "path". (optional)
#'
#' @param maxTries Maximum number of times to attempt file information retrieval. Default is 1. (optional)
#'
#' @return Returns the last ctime file path, file name or a data.frame with the file name, path and modification details as defined by return_type where size is the file size in bytes, mtime is the last modification time, ctime is last status change time, atime is last access time.
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords file name, file path
#'
#' @examples
#' Return a path
#' file_path <- getlatestfile(file.path("...","subfolder","subfolder"))
#'
#'Import the latest csv file
#'df <- fread(getlatestfile(folder_path = file.path("...","subfolder","subfolder"), file_string = "csv",exclusions = "unwanted", return_type = "path", maxTries = 5))
#'
#'Record the modification details
#'Data_as_of <- getlatestfile(folder_path = file.path("...","subfolder","subfolder"), file_string = "csv",exclusions = "unwanted", return_type = "all", maxTries = 5)$ctime
#'
#' @export
getlatestfile <- function(folder_path, file_string, exclusions, return_type, maxTries){

  # Retrieve files
  if ( missing(maxTries)) {
    maxTries <- 1
  }

  # Retrieve file names in folder_path
  count <- 0
  repeat {
    # Look for files
    if (missing(file_string)) {
      file <- list.files(path = file.path(folder_path),
                         include.dirs = FALSE,
                         recursive = FALSE)
    } else {
      file <- list.files(path = file.path(folder_path),
                         include.dirs = FALSE,
                         recursive = FALSE,
                         pattern = file_string)
    }
    # Keep looking up to maxTries
    if (count <= maxTries) {

      count <- count + 1
      # Stop looking after maxTries
    } else {
      break
    }
    # Stop looking when files retrieved
    if (!is.null(dim(file) )) {
      break
    }

  }
  # Stop function if no files retrieved
  if (length(file) < 1) {

    stop("No files found. Check folder_path and your connection and increase maxTries if necessary")

  }

  # Exclude unwanted files
  if ( missing(exclusions)) {
    file <- file[!grepl("\\$", file)]
  } else {
    file <- file[!grepl(paste("\\$", exclusions, sep = "|"), file)]
  }

  # Stop function if no files left
  if (length(file) < 1) {

    stop("All files from folder_path have been excluded. Check exclusion criteria and try again.")

  }

  # Retrieve latest file by modification date
  file <- as.data.frame(file, row.names = NULL)
  file[,1] <- as.character(file[,1])

  repeat {

    file <- cbind(file, file.info(file.path(folder_path, file[,1])))

    if (count <= maxTries &  all(is.na(file$ctime))) {

      count <- count + 1

    } else {
      break
    }
  }
  if (count == maxTries &  all(is.na(file$ctime))) {

    stop("File names retrieved but modification date not retrieved. Check your connection and increase maxTries if necessary")

  }

  file <- file[with(file, order(ctime)), ]
  file <- file[NROW(file),]
  file$path <- file.path(folder_path, file$file)
  rownames(file) <- 1

  # Output
  if ( missing(return_type)) {
    return_type <- "path"
  }

  if ( return_type == "all") {

    return(file)

  } else if (return_type == "name") {

    message(paste0("File modification date: ", file$ctime))
    return(as.character(file$file))

  } else if (return_type == "path") {

    message(paste0("File modification date: ", file$ctime))
    return(as.character(file$path))

  }

}
