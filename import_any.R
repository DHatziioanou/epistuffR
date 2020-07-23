#' Import various formats of files into R with one command
#'
#'
#' @param file file to import. If not in your working directory the path needs to be included.
#'
#' @param sheet Optional; sheet name or number within workbooks to import. Default is 1.
#'
#' @return files imported into R
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords import
#'
#' @examples
#' 
#' import_any("C:..../file.csv")
#' 
#' import_any("C:..../file.xlsx", sheet = "Data")
#' 
#' import_any("C:..../file.xls", sheet = 4)
#' 
#' @export
import_any <- function(file, sheet){
  
  # Sheet name or number for workbooks
  if (missing(sheet)) {
    sheet <- 1
  }
  
  # Import csv
  if (file_ext(file) == "csv") {
    if (!require("data.table")) {
      install.packages("data.table")
      suppressWarnings(library(data.table))
    }
    df <- fread(file, header = T, stringsAsFactors = F, showProgress = T) 
    
    
  } else if (file_ext(file) == "xlsx") {
    # Import xlsx
    if (!require("readxl")) {
      install.packages("readxl")
      suppressWarnings(library(readxl))
    }
    df <- readxl::read_xlsx(file, sheet = sheet,  col_names  =  T, col_types = "text") 


  } else if (file_ext(file) == "xls") {
    # Import xls
    if (!require("readxl")) {
      install.packages("readxl")
      suppressWarnings(library(readxl))
    }
    df <- readxl::read_xls(file, sheet = sheet,  col_names  =  T, col_types = "text") 
    
    # Import dta
  } else if (file_ext(file) == "dta") {
    if (!require("haven")) {
      install.packages("haven")
      suppressWarnings(library(haven))
    }
    df <- haven::read_dta(file, encoding = NULL)

  }
  
  # Format as data.frame
  df <- as.data.frame(df)
  # Save file to Environment  
  return(df)
}
