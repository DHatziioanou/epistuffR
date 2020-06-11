
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
#' install_load("data.table", "dplyr", "tidyr", "ggplot2")
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
    message(paste("Please wait while",length(missing),"packages are installed..."))
    
    if (sum("devtools" %in% packages) > 0 & !("pkgbuild" %in% rownames(installed.packages()))) {
      missing <- c("pkgbuild", "ps", missing)
    }
    
    for (package in missing) {  
      if (!is.null(sessionInfo()$otherPkgs)) lapply(paste('package:' ,names(sessionInfo()$otherPkgs), sep = ""), detach,character.only = TRUE, unload = TRUE)
      
      t <- try(install.packages(package, type = "source", dependencies = T, quiet = quiet, repos = repo, lib = lib))
      t2 <- try(ifelse(inherits(t, "try-error"), 
                       alternativeFunction(install.packages(package, type = "binary", dependencies = T, quiet = quiet, repos = repo, lib = lib)), 
                       F))
    
      if (!(package %in% rownames(installed.packages()))) {
        if (!is.null(sessionInfo()$otherPkgs)) lapply(paste('package:' ,names(sessionInfo()$otherPkgs), sep = ""), detach,character.only = TRUE, unload = TRUE)
        install.packages(package, type = "binary", dependencies = T, quiet = quiet, repos = repo, lib = lib)
      }
      if (package %in% rownames(installed.packages())) message(paste(package, "installed")) else message(paste(package, "not installed"))
    } 
  }
  
  missing <- packages[!(packages %in% rownames(installed.packages()))]
  if ( length(missing) > 0) {
    message(paste("Packages not installed:"))
    print(missing)
    message("Try installing manually")
  }
  
  # Load packages
  for (package in packages) {
    
    # if package is installed locally, load
    if (package %in% rownames(installed.packages()))
     t <- try( do.call('library', list(suppressPackageStartupMessages(package))))
    if (inherits(t, "try-error")) { 
           install.packages(package, type = "binary", dependencies = T, quiet = quiet, repos = repo, lib = lib)
           do.call('library', list(suppressPackageStartupMessages(package)))
    }
  }
  loaded_packages <- (.packages())
  message(sum(loaded_packages %in% packages), " packages out of ",length(packages), " loaded.")
  message("Not loaded:")
  print(loaded_packages[!(packages %in% loaded_packages)])
}
