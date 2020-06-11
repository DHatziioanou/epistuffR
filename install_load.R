  
#' For a list of packages install missing ones and load them all.
#'
#' 
#' @param package1 vector of required package names
#'
#' @return Named packages installed and loaded
#'
#' @examples
#' 
#' install_load("data.table")
#' 
#' install_load("data.table", "dplyr", "tidyr", "ggplot2")
#'
install_load <- function(package1, ...)  {
  packages <- c(package1, ...)
  
  # Packages needing installed
  missing <- packages[!(packages %in% rownames(installed.packages()))]
  
  # Install missing packages
  if (is.null(dim(missing))) {
    message("Please wait while necessary packages are installed...")
    
    for (package in missing) {  
      
    t <- try(install.packages(package, dependencies = TRUE, quiet = TRUE))
    ifelse(inherits(t, "try-error"), 
           alternativeFunction(install.packages(package, type = "binary")), 
           t)
    message(paste(package, "installed"))
    }
  }
  
  # Load packages
  for (package in packages) {
    
    # if package is installed locally, load
    if (package %in% rownames(installed.packages()))
      do.call('library', list(suppressPackageStartupMessages(package)))

  }
  message("All loaded.")
}
