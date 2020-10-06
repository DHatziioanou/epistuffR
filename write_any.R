#' Write various formats of files into R with one command
#'
#'
#' @param data object with data to write
#'
#' @param file file name and path to write to. If missing the object name will be used.
#'
#' @param type file type to save data as. Default is csv
#'
#' @return writes data to file
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords write
#'
#' @examples
#'
#' write_any(data, "C:..../file.csv")
#'
#' write_any(data, "C:..../file.xlsx", sheet = "Data")
#'
#' write_any(data, "C:..../file.xls", sheet = 4)
#'
#' @export
write_any <- function(data, file, type, sheet){
  
  # File type
  if (missing(type)) {
    type <- "csv"
  }
  # File name
  if (missing(file)) {
    file <- paste0(deparse(substitute(data)), format(Sys.Date(), "%Y-%m-%d"))
  }
  
  # Write csv
  if (type == "csv") {
    if (!require("data.table")) {
      install.packages("data.table")
      suppressWarnings(library(data.table))
    }
    df <- fwrite(data, paste0(file, ".csv"), row.names = F, col.names = T, append = F)
    
    
  } else if (type == "xlsx") {
    # Write xlsx
    if (!require("openxlsx")) {
      install.packages("openxlsx")
      suppressWarnings(library(openxlsx))
    }
    
    if (missing(sheet)) {
      sheet <- paste0(format(Sys.Date(), "%Y%m%d"), " data")
    }
    
    # CREATE WORKBOOK
    style2 <- openxlsx::createStyle(fontSize = 12, 
                                    halign = "center", 
                                    border = "TopBottomLeftRight", 
                                    borderStyle = "thick",
                                    fontColour = "#822433", 
                                    wrapText = T,
                                    textDecoration = "bold")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, data, startCol = 1, startRow = 1, borders = "surrounding", borderStyle = "thick", headerStyle = style2)
    setColWidths(wb, sheet, cols = 1:ncol(data), widths = "auto")
    openxlsx::addFilter(wb, sheet, row = 1, cols = 1:ncol(data))
    # SAVE WORKBOOK
    openxlsx::saveWorkbook(wb, paste0(file, format(Sys.Date(), format = "%Y%m%d"),".xlsx"), overwrite = T)
    
    
  } else if (type == "xlsx2") {
    # Write xls
    if (!require("readxl")) {
      install.packages("readxl")
      suppressWarnings(library(readxl))
    }
    if (missing(sheet)) {
      sheet <- paste0(format(Sys.Date(), "%Y%m%d"), " data")
    }
    
    readxl::write.xlsx(data, paste0(file, ".xlsx"), sheetName = sheet,
                       col.names = T, row.names = F, append = F)
    
    # Write dta
  } else if (type == "dta") {
    if (!require("haven")) {
      install.packages("haven")
      suppressWarnings(library(haven))
    }
    df <- haven::write_dta(data, file, version = 14)
    
  }
}
