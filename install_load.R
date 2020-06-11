
#' For a list of packages install missing ones and load them all.
#'
#' 
#' @param package1 vector of required package names
#'
#' @param repo repository path (optional)
#'
#' @param lib library path (optional)
#'
#' @param quiet True or False
#'
#' @return Named packages installed and loaded
#'
#' @examples
#' 
#' install_load("data.table")
#' 
#' install_load(c("data.table", "dplyr", "tidyr", "ggplot2"), quiet = F)
#'
install_load <- function(packages, ..., repo, lib, quiet)  {
  packages <- c(packages, ...)
  
  if (missing(repo)) {
    repo <- 'http://cran.rstudio.com/'
  }
  
  if (missing(lib)) {
    lib <- .libPaths()[1]
  }
  
  if (missing(quiet)) {
    quiet <- T
  }
  
  
  # Packages needing installed
  missing <- packages[!(packages %in% rownames(installed.packages()))]
  
  # Install missing packages
  if (length(missing)  > 0) {
    message("Please wait while necessary packages are installed...")
    
    if (sum("pkgbuild" %in% packages) > 0) {
      packages <- c("pkgbuild", packages)
    }
    
    for (package in missing) {  
      
      t <- try(install.packages(package, type = "source", dependencies = T, quiet = quiet, repos = repo, lib = lib))
      t2 <- try(ifelse(inherits(t, "try-error"), 
             alternativeFunction(install.packages(package, type = "binary", dependencies = T, quiet = quiet, repos = repo, lib = lib)), 
             F))
      ifelse(inherits(t2, "try-error"), 
             alternativeFunction(install.packages(package, type = "binary", dependencies = F, quiet = quiet, repos = repo, lib = lib)), 
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
