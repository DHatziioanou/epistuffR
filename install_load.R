
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
install_load <- function(package1, ..., repo)  {
  packages <- c(package1, ...)
  
  if(missing(repo)){
    repo <- 'http://cran.rstudio.com/'
  }
  
  # Packages needing installed
  missing <- packages[!(packages %in% rownames(installed.packages()))]
  
  # Install missing packages
  if (length(missing)  > 0) {
    message("Please wait while necessary packages are installed...")
    
    for (package in missing) {  
      
      t <- try(install.packages(package, type = "source", dependencies = T, quiet = F, repos=repo))
      ifelse(inherits(t, "try-error"), 
             alternativeFunction(install.packages(package, type = "binary", dependencies = T, quiet = F, repos=repo)), 
             F)
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
