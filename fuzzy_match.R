#' Fuzzy match a custom line list to a dabatase by Suname, first name and DOB
#' 
#' Matches custom line list persons by full name and DOB to a database, assigns scores and creates warnings based on the match quality and returns the original line list with all the columns of the database and database information where there is a match to the custom line list
#' 
#' 
#' @param lla Input Line-list list containing the following items;
#' @param ob Line-list object to search. Rows with no surname or first name will be excluded
#' @param sn surname column within the Line-list object as a string
#' @param fn first name column within the Line-list object as a string
#' @param dob date of birth column within the Line-list object as a string
#' @param pc postcode column within the Line-list object as a string
#' @param geo Geography where most individuals can be found. This string must be present in a column of dba.
#' 
#' @param dba Input database list containing the following items;
#' @param ob database object to search. All columns of this database will be appended to the input line list
#' @param sn surname column within the database object as a string
#' @param fn first name column within the database object as a string
#' @param dob date of birth column within the database object as a string
#' @param pc postcode column within the database object as a string
#' @param geo Geography column containing the location in string format from lla geo where most individuals can be found. 
#' 
#'  
#' @param stringency Optional; stringency required stringency of matching; one of high, medium or low. Default is high. 
#' @param pc_format postcode format where l = letters and n = numbers. Default is the UK format "lnl"
#' @param case_column Column within the custom line list with one unique case number per row
#' @param parallel Optional; process in parallel. Default is TRUE
#' 
#' 
#' @return Returns the original input object as a data.frame with added columns of the input databse where there is a match base on full name and DOB
#'
#'
#' @author Diane Hatziioanou
#' 
#' @examples
#' 
#' # Prepare database to include the required columns and anythign else to be appended to the custom line list
#' db <- db[,c("patient_surname", "patient_forename", "patient_date_of_birth", "Postcode", "specimen_date)]
#' db$Positive <- "Yes"
#' 
#' LL <- LL[,c("patient_surname", "patient_forename", "patient_date_of_birth", "Postcode", "specimen_date, "ETHNICITY")]
#' 
#' # Create lists of the objects and columns to fuzzy match
#' lla <- list(ob = custom_LL, sn = "SURNAME", fn = "FORENAMES", dob = "BIRTHDATE", pc = "Postcode")
#' dba <- list(ob = db, sn = "patient_surname", fn = "patient_forename", dob = "patient_date_of_birth", pc = "Postcode")
#' 
#' # Create a new line list with the database information appended
#' Positives <- Fuzzy_match(lla, dba, stringency = F, pc_format = "lnl)
#' Negatives <- Fuzzy_match(lla, dba, stringency = T, case_column = "ID) 
#' 
fuzzy_match <- function(lla, dba, stringency, pc_format, case_column, parallel){
  library(stringr)
  library(stringi)
  library(data.table)
  library(stringdist)
  library(foreach)
  library(doParallel)
  library(rvest)
  
  if (base::missing(stringency)) {
    stringency <- "high"
  }
  # Expected postcode format
  l <- "[A-Za-z]"
  n <- "[0-9]"
  if (missing(pc_format)) {
    pc_format <- "lnl"
  }

  pattern <- paste0(get(substr(pc_format,1,1)),".*")
  if(length(pattern >1)){
    for (letter in 1:(nchar(pc_format)-1)){
      pattern <- paste0(pattern, get(substr(pc_format,letter+1,letter+1)), collapse = "*")
    }
  }
 
# Format  --------------------------------------------------  
  
  # Format to data.table for speed
    dbaob <- copy(as.data.table(dba$ob))
    llaob <- copy(as.data.table(lla$ob))
    
  if (!(missing(case_column))) {
    # If duplicate case numbers stop
    if(sum(duplicated(llaob[,get(case_column)])) >0 ){
      stop(paste0("Duplicates found in ", case_column, ". Remove duplicates or omit the case_number argument to assign new case numbers"))
  }}
  
    # Fix encoding
    Search_db$Surname <- stri_encode(Search_db$Surname, "", "UTF-8")
    Search_db$Surname <- stri_trans_general(Search_db$Surname, "Latin-ASCII")
    
    Search_db$Forename <- stri_encode(Search_db$Forename, "", "UTF-8")
    Search_db$Forename <- stri_trans_general(Search_db$Forename, "Latin-ASCII")
    
    
  # Format Surnames  
    # Encode UTF-8
   dbaob[, db_Surname := stri_encode(get(dba$sn), "", "UTF-8")] 
   dbaob[, db_Surname := stri_trans_general(db_Surname, "Latin-ASCII")] 
   llaob[, ll_Surname := stri_encode(get(lla$sn), "", "UTF-8")] 
   llaob[, ll_Surname := stri_trans_general(ll_Surname, "Latin-ASCII")] 
    # Uppercase
   dbaob[, db_Surname := toupper(db_Surname)]  
   llaob[, ll_Surname := toupper(ll_Surname)]  
    # Remove special characters
   dbaob[, db_Surname := gsub("[[:punct:]]", " ", db_Surname)]  
   llaob[, ll_Surname := gsub("[[:punct:]]", " ", ll_Surname)]  

  # Format first names 
   # Encode UTF-8
   dbaob[, db_Name := stri_encode(get(dba$fn), "", "UTF-8")] 
   dbaob[, db_Name := stri_trans_general(db_Name, "Latin-ASCII")] 
   llaob[, ll_Name := stri_encode(get(lla$fn), "", "UTF-8")] 
   llaob[, ll_Name := stri_trans_general(ll_Name, "Latin-ASCII")] 
    # Uppercase
   dbaob[, db_Name := toupper(db_Name)]  
   llaob[, ll_Name := toupper(ll_Name)]  
    # Remove special characters
   dbaob[, db_Name := gsub("[[:punct:]]", " ", db_Name)]  
   llaob[, ll_Name := gsub("[[:punct:]]", " ", ll_Name)]  

  # Full name
   dbaob[, db_surname_firstname := paste(db_Surname, db_Name), by = 1:nrow(dbaob)]  
   llaob[, ll_surname_firstname := paste(ll_Surname, ll_Name), by = 1:nrow(llaob)]

  # Format postcode
   dbaob[, db_Postcode := toupper(gsub(" |[[:punct:]]", "", get(dba$pc)))]  
   llaob[, ll_Postcode := toupper(gsub(" |[[:punct:]]", "", get(lla$pc)))]
  # Remove postcodes which don't follow the format character-number-character 
   dbaob[,db_Postcode:= as.character(db_Postcode)]
   llaob[,ll_Postcode:= as.character(ll_Postcode)]
   dbaob[!(grepl(pattern, db_Postcode)), db_Postcode := NA]
   llaob[!(grepl(pattern, ll_Postcode)), ll_Postcode := NA]

   # Format DOB
      if(suppressWarnings(sum(!(is.na(lubridate::parse_date_time(dbaob[, get(dba$dob)], orders = c("dmy", "mdy", "ymd")))))) == 0) {
     dbaob[, (dba$dob) := as.numeric(dba$ob[, get(dba$dob)])]
     dbaob[, (dba$dob) := as.Date(dba$ob[, get(dba$dob)], origin = "1899-12-30")]
    }
     dbaob[, db_DOB := as.Date(lubridate::parse_date_time(dbaob[, get(dba$dob)], orders = c("dmy", "mdy", "ymd")))]
      if(suppressWarnings(sum(!(is.na(lubridate::parse_date_time( llaob[, get(lla$dob)], orders = c("dmy", "mdy", "ymd")))))) == 0) {
        llaob[, (lla$dob) := as.numeric(llaob[, get(lla$dob)])]
        llaob[, (lla$dob) := as.Date(llaob[, get(lla$dob)], origin = "1899-12-30")]
      }
      llaob[, ll_DOB := as.Date(lubridate::parse_date_time( llaob[, get(lla$dob)], orders = c("dmy", "mdy", "ymd")))]

      
  # Remove lines with no names and DOB
     llaob <- llaob[!(is.na(get(lla$sn))) & !(is.na(get(lla$fn)))]
     dbaob <- dbaob[!(is.na(get(dba$sn))) & !(is.na(get(dba$fn)))]
      
     setkey(llaob, "ll_surname_firstname")
     
     # Reserve database outside of geography
     if (isTRUE(!is.na(dba$geo))) {
     dbaob_reserve <- copy(dbaob[!(get(dba$geo) %in% lla$geo) ])
     dbaob <- dbaob[(get(dba$geo) %in% lla$geo) ]
     if(nrow(dbaob_reserve)>0){
       reserve_search <- "yes"
     }
     }
     if(!(exists("reserve_search"))){
       reserve_search <- "no"
     }
     
# Fuzzy search function based on names and DOB
fuz_search <- function(dbaob, sn_fun){
       x <- as.character("")
       x <- lapply(sn_fun, function(w) {agrep(w, dbaob$db_surname_firstname, ignore.case=F, max.distance = 0.3, value=TRUE, useBytes = FALSE, costs = list(ins=2, del=2, sub=3))})
       # Give sn_fun name used to each list df by level
       for (r in seq_along(x)) {
         x[[r]] <- suppressWarnings(copy(data.table("db_surname_firstname" = x[[r]], "Matched_surname_firstname" = sn_fun[r])))
       }
       # Collect list results
       x <- rbindlist(x, fill = T, use.names = T)
       x <- x[!(is.na(db_surname_firstname))]
       
       if (nrow(x) > 0){
       # Remove duplicate hits with same name
       x[, Name_score := stringdist::stringsim(Matched_surname_firstname, db_surname_firstname, method = "jaccard"), by = 1:nrow(x)]
       x[, Name_score_pos := stringdist::stringsim(Matched_surname_firstname, db_surname_firstname, method = "lv"), by = 1:nrow(x)]
       x <- x[with(x, ave(Name_score_pos, db_surname_firstname, FUN=max)==Name_score_pos),]
       } else{
         # Fill empty
       x[, Name_score := NA]
       } 
       
       # Retrieve matches by DOB
       if (!(is.na(llaob$ll_DOB[i]))) {
         d <- as.character("")
         d <- agrep(llaob$ll_DOB[i],dbaob$db_DOB,
                    ignore.case=TRUE, max.distance = 2, value=TRUE, useBytes = FALSE, costs = list(ins=2, del=2, sub=2))
         # Retain only best db matched DOB
         d <- data.table("db_DOB" = d)
         d[, DOB_score := stringdist::stringsim(as.character(llaob$ll_DOB[i]), db_DOB, method = "hamming")]
         d[, db_DOB := as.Date(lubridate::parse_date_time(db_DOB, orders = c("dmy", "mdy", "ymd")))]
       } else {
         # Fill missing list object
         d <- data.table(DOB_score=numeric())
       }
       return(list(x,d))
     }
     
# Search ll against db by full name and dob  --------------------------------------------------

if(missing(parallel)){
  parallel <- T
}

if(parallel == F){
  for (i in 1:nrow(llaob)) {
    setDT(llaob)
    # Show progress
    if(i == 1) { 
      cat(paste0(nrow(llaob), " rows to match. Matching row; ", i))  
    } else {
    cat(paste0(", ", i))
    }
    # All names
    fn <- tstrsplit(llaob$ll_Name[i], " ")
    sn <- tstrsplit(llaob$ll_Surname[i], " ")
    # All name combinations
    sn_fun <- c(paste(llaob$ll_Surname[i], llaob$ll_Name[i], sep = " "), paste(rep(sn, each = length(fn)), fn, sep = " "), paste(rep(fn, each = length(sn)), sn, sep = " "))
    sn_fun <- sn_fun[!duplicated(sn_fun)]
    #sn_fun <- as.data.table(sn_fun)
        
#   Retrieve reduced db matches by all full name combinations ------------------------
    matches  <- fuz_search(dbaob, sn_fun)
    

# If no decent matches retrieve db matches outside of geography  ------------------------
 if(nrow(matches[[1]]) == 0 | suppressWarnings(max(matches[[1]]$Name_score) < 0.8) & reserve_search == "yes"){
    
# Fuzzy match reserve
   matches_reserve <- fuz_search(dbaob_reserve, sn_fun)
   
     # Gather geography and reserve matches
     x <- rbindlist(list(matches[[1]], matches_reserve[[1]]), fill = T, use.names = T)
     x <- x[x$Name_score>=0.7,]
     d <- rbindlist(list(matches[[2]], matches_reserve[[2]]), fill = T, use.names = T)
     
 } else {
     # Gather geography matches
     x <- matches[[1]]
     x <- x[x$Name_score>=0.7,]
     d <- matches[[2]]
   }

    # Remove duplicate rows
     x <- x[x$Name_score >= 0.7,]
     x <- unique(x)
     d <- unique(d)

     # if(nrow(x)>10){ 
     #   x <- x[order(Name_score, Name_score_pos, na.last = T, decreasing = T)]
     #   x <- x[1:10,]
     #   }
     
# If no matches skip loop and go to next record ------------------------
    if (nrow(x) == 0) {
      if (exists("unmatched")) {
        unmatched <- c(unmatched, i)
      } else {
        unmatched <- i
      }  
suppressWarnings(rm( list = Filter( exists, c("x", "d", " b", "fn", "sn", "sn_fun", "matches","matches_reserve" ) ) ))
      next
    }
       
  
# Gather results  -----------------------------------------------
     
    # Retrieve matched records from db and add ll info
     x[,ll_surname_firstname := llaob$ll_surname_firstname[i]]
     x[,ll_DOB := llaob$ll_DOB[i]]
     #x[,Matched_surname_firstname := x][,x:=NULL]
     setkey(x, "db_surname_firstname")
     
    # Matched db records from names  
     if(exists("matches_reserve")){
       y <- copy(dbaob_reserve[dbaob_reserve$db_surname_firstname %in% c(x$db_surname_firstname),])
       y <- rbindlist(list(y,dbaob[dbaob$db_surname_firstname %in% c(x$db_surname_firstname),]), use.names = T, fill = T)
     } else {
       y <- copy(dbaob[dbaob$db_surname_firstname %in% c(x$db_surname_firstname),])
     }

    # Add line list info to matched db records
     setkey(y, "db_surname_firstname")
     y[, c(dba$sn, dba$fn, dba$dob, dba$pc) := NULL]
     y <- unique(y)
     y <- merge(y, x, by = "db_surname_firstname", all = T, allow.cartesian=TRUE)
     
     # Matched records from dob search
    if (nrow(d)>0) {
      if(exists("matches_reserve")){
        b <- copy(dbaob_reserve[db_DOB %in% d$db_DOB])
        b <- rbindlist(list(b,dbaob[dbaob$db_DOB %in% d$db_DOB,]), use.names = T, fill = T)
      } else {
        b <- copy(dbaob[db_DOB %in% d$db_DOB])     
      }
       b[, c(dba$sn, dba$fn, dba$dob, dba$pc) := NULL]
       b[,ll_surname_firstname := llaob$ll_surname_firstname[i]]
       b[,ll_DOB := llaob$ll_DOB[i]]
       b[, Name_score := stringdist::stringsim(ll_surname_firstname, db_surname_firstname, method = "jaccard"), by = 1:nrow(b)]
       b[, Name_score_pos := stringdist::stringsim(ll_surname_firstname, db_surname_firstname, method = "lv"), by = 1:nrow(b)]
       b[, Matched_surname_firstname := "DOB match"]
       b <- b[Name_score >= 0.7, ]
       # Add dob matches to name matches
       y <- rbindlist(list(y,b), use.names = T, fill = T)
    } 
     
     # Add basic info and match scores
     if(missing(case_column)){
       case_column <- "Case"
            y[, (case_column):= i]
     } else {
       case_n <- as.character(llaob[i, ..case_column])
            suppressWarnings(y[, (case_column):= case_n])
     }

     y[,ll_Postcode := llaob$ll_Postcode[i]]
     y[,DOB_score := stringdist::stringsim(as.character(ll_DOB), as.character(db_DOB), method = "lv")]
     #z[, DOB_score := unique(DOB_score[!is.na(DOB_score)]), by = Case]
     y[,PC_score := stringdist::stringsim(ll_Postcode, db_Postcode, method = "lv")]
     y[,Fuz_score := sum(Name_score, DOB_score, PC_score, na.rm = T), by = 1:nrow(y)]
     # Remove duplicated rows with all db identical by max Name_score_pos
     y <- y[with(y, order(Fuz_score, decreasing = T)), ]
     
     # Find db duplicate records and retain one copy per db record
     db_col <- names(y)[names(y) %in% names(dbaob)]
     dup <- copy(y[, .SD, .SDcols=db_col])
     y <- y[!(which(duplicated(dup) == TRUE)),]

    if (exists("z")) {
      z <- rbindlist(list(z,y), fill = T, use.names = T)
    } else {
      z <- copy(y)
    }
     
     # Clear for next cycle   
     suppressWarnings(rm( list = Filter( exists, c("x", "d", " b", "y", "fn", "sn", "sn_fun", "dup") ) ))
      }
} else {

    # Number of available cores
    numCores <- detectCores()
    # Open cluster
    cluster <- makeCluster(numCores[1]-1, outfile = "")
    cases <- 1:nrow(llaob)
    
    # Search ll against db by full name and dob  -------------------------------------------------
    retrieve_data <- function(i, case_column){
      setDT(llaob)
      # Show progress
      if(i == 1) { 
        cat(paste0(nrow(llaob), " rows to match. Matching row; ", i))  
      } else {
        cat(paste0(", ", i))
      }
      # All names
      fn <- tstrsplit(llaob$ll_Name[i], " ")
      sn <- tstrsplit(llaob$ll_Surname[i], " ")
      # All name combinations
      sn_fun <- c(paste(llaob$ll_Surname[i], llaob$ll_Name[i], sep = " "), paste(rep(sn, each = length(fn)), fn, sep = " "), paste(rep(fn, each = length(sn)), sn, sep = " "))
      sn_fun <- sn_fun[!duplicated(sn_fun)]
      #sn_fun <- as.data.table(sn_fun)
      
      #   Retrieve reduced db matches by all full name combinations ------------------------
      matches  <- fuz_search(dbaob, sn_fun)
      
      
      # If no decent matches retrieve db matches outside of geography  ------------------------
      if(nrow(matches[[1]]) == 0 | suppressWarnings(max(matches[[1]]$Name_score) < 0.8) & reserve_search == "yes"){
        
        # Fuzzy match reserve
        matches_reserve <- fuz_search(dbaob_reserve, sn_fun)
        
        # Gather geography and reserve matches
        x <- rbindlist(list(matches[[1]], matches_reserve[[1]]), fill = T, use.names = T)
        x <- x[x$Name_score>=0.7,]
        d <- rbindlist(list(matches[[2]], matches_reserve[[2]]), fill = T, use.names = T)
        
      } else {
        # Gather geography matches
        x <- matches[[1]]
        x <- x[x$Name_score>=0.7,]
        d <- matches[[2]]
      }
      
      # Remove duplicate rows
      x <- x[x$Name_score >= 0.7,]
      x <- unique(x)
      d <- unique(d)
      
      if(nrow(x)>10){ 
        x <- x[order(Name_score, Name_score_pos, na.last = T, decreasing = T)]
        x <- x[1:10,]
      }
      
      # If no matches skip loop and go to next record else continue ------------------------
      if (nrow(x) == 0) {
        if (exists("unmatched")) {
          unmatched <- c(unmatched, i)
        } else {
          unmatched <- i
        }  
        suppressWarnings(rm( list = Filter( exists, c("x", "d", " b", "fn", "sn", "sn_fun", "matches","matches_reserve" ) ) ))
        
      } else {
        
        # Gather results  -----------------------------------------------
        
        # Retrieve matched records from db and add ll info
        x[,ll_surname_firstname := llaob$ll_surname_firstname[i]]
        x[,ll_DOB := llaob$ll_DOB[i]]
        #x[,Matched_surname_firstname := x][,x:=NULL]
        setkey(x, "db_surname_firstname")
        
        # Matched db records from names  
        if(exists("matches_reserve")){
          y <- copy(dbaob_reserve[dbaob_reserve$db_surname_firstname %in% c(x$db_surname_firstname),])
          y <- rbindlist(list(y,dbaob[dbaob$db_surname_firstname %in% c(x$db_surname_firstname),]), use.names = T, fill = T)
        } else {
          y <- copy(dbaob[dbaob$db_surname_firstname %in% c(x$db_surname_firstname),])
        }
        
        # Add line list info to matched db records
        setkey(y, "db_surname_firstname")
        y[, c(dba$sn, dba$fn, dba$dob, dba$pc) := NULL]
        y <- unique(y)
        y <- merge(y, x, by = "db_surname_firstname", all = T)
        
        # Matched records from dob search
        if (nrow(d)>0) {
          if(exists("matches_reserve")){
            b <- copy(dbaob_reserve[db_DOB %in% d$db_DOB])
            b <- rbindlist(list(b,dbaob[dbaob$db_DOB %in% d$db_DOB,]), use.names = T, fill = T)
          } else {
            b <- copy(dbaob[db_DOB %in% d$db_DOB])     
          }
          b[, c(dba$sn, dba$fn, dba$dob, dba$pc) := NULL]
          b[,ll_surname_firstname := llaob$ll_surname_firstname[i]]
          b[,ll_DOB := llaob$ll_DOB[i]]
          b[, Name_score := stringdist::stringsim(ll_surname_firstname, db_surname_firstname, method = "jaccard"), by = 1:nrow(b)]
          b[, Name_score_pos := stringdist::stringsim(ll_surname_firstname, db_surname_firstname, method = "lv"), by = 1:nrow(b)]
          b[, Matched_surname_firstname := "DOB match"]
          b <- b[Name_score >= 0.7, ]
          # Add dob matches to name matches
          y <- rbindlist(list(y,b), use.names = T, fill = T)
        } 
        
        # Add basic info and match scores
        if(missing(case_column)){
          setDT(y)
          case_column <- "Case"
          y[, (case_column):= i]
        } else {
          setDT(llaob)
          case_n <- as.character(llaob[i, ..case_column])
          suppressWarnings(y[, (case_column):= case_n])
        }
        
        y[,ll_Postcode := llaob$ll_Postcode[i]]
        y[,DOB_score := stringdist::stringsim(as.character(ll_DOB), as.character(db_DOB), method = "lv")]
        #z[, DOB_score := unique(DOB_score[!is.na(DOB_score)]), by = Case]
        y[,PC_score := stringdist::stringsim(ll_Postcode, db_Postcode, method = "lv")]
        y[,Fuz_score := sum(Name_score, DOB_score, PC_score, na.rm = T), by = 1:nrow(y)]
        # Remove duplicated rows with all db identical by max Name_score_pos
        y <- y[with(y, order(Fuz_score, decreasing = T)), ]
        
        # Find db duplicate records and retain one copy per db record
        db_col <- names(y)[names(y) %in% names(dbaob)]
        dup <- copy(y[, .SD, .SDcols=db_col])
        y <- y[!(which(duplicated(dup) == TRUE)),]
        
        if (exists("z")) {
          z <- rbindlist(list(z,y), fill = T, use.names = T)
        } else {
          z <- copy(y)
        }
        
        # Clear for next cycle   
        suppressWarnings(rm( list = Filter( exists, c("x", "d", " b", "y", "fn", "sn", "sn_fun", "dup", "matches","matches_reserve" ) ) ))
      }
      
      if (!exists("z")) {
        z <- data.table(ll_surname_firstname = NA)
      }
      if (!exists("unmatched")) {
        unmatched <- NA
      }
      setDT(z)
      
      par_return <- list(data = z, skipped = unmatched)
      return(par_return)
    }

    registerDoParallel(cl = cluster)  # use multicore, set to the number of our cores
    par_all <- foreach (i=cases, .packages = c("data.table"), .verbose = T) %dopar% {
      par_list <- retrieve_data(i, case_column)
      return(par_list)
    }
    # Close cluster
    stopCluster(cluster)
    
      # Collect list results

  z <- rbindlist(rvest::pluck(par_all, 1), fill = T, use.names = T)
  unmatched <- unlist(rvest::pluck(par_all, 2), use.names=FALSE)
  unmatched <- unmatched[!(unmatched %in% NA)]
  z <- z[!(is.na(db_surname_firstname))]
  }
  

  
# Manage retrieved data ----------------



      # Add any unmatchedlines back
      if (exists("unmatched")) {
      z <- rbindlist(list(z,setDF(llaob)[unmatched,names(llaob) %in% names(z)]), fill = T, use.names = T)
      setDT(llaob)
      }
      
     
if(exists("z") == F) stop("No matches found")

# Closest match by Name, DOB, PC
#z <- z[with(z, order(case_column, Name_score, DOB_score, Fuz_score)), ]
# Fuz_scores under 1 or name scores under 0.5 unlikely to be a match
z <- z[Fuz_score>=0.9 & Name_score >0.5]

# Add WARNINGS for cases needing checked
z[, Case_ID_Warning := ""]
z[, Case_ID_Warning := ifelse(db_surname_firstname==ll_surname_firstname,
                                             'name match',
                                             'WARNING! name difference'), 
                  by = 1:nrow(z)]
z[,Case_ID_Warning := ifelse(db_surname_firstname== "" | is.na(db_surname_firstname),
                                             'not found',
                                             Case_ID_Warning), 
                  by = 1:nrow(z)]
# summary.factor(z$Case_ID_Warning)

# Add DOB match info
z[,Case_ID_Warning := if (is.na(ll_DOB) | is.na(db_DOB)) paste0(Case_ID_Warning,' no DOB'), by = 1:nrow(z)]
#   DOB check
z[,Case_ID_Warning := ifelse(!(grepl("no DOB", Case_ID_Warning)) & ll_DOB==db_DOB,
  paste0(Case_ID_Warning,' DOB match'),
  paste0("WARNING! ", Case_ID_Warning,' DOB difference')), 
  by = 1:nrow(z)]

z[,Case_ID_Warning := ifelse(
  Case_ID_Warning=="name match DOB match",
  'Perfect match',
  Case_ID_Warning), 
  by = 1:nrow(z)]
# Tidy warnings
z[,Case_ID_Warning:= gsub("no DOB DOB difference", "no DOB", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING not found no DOB", "not found", Case_ID_Warning)]
#z[,Case_ID_Warning:= gsub(" WARNING WARNING! name difference no DOB", "WARNING! name difference no DOB", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING! WARNING! name difference no DOB", "WARNING! name difference no DOB", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING! name difference DOB match", "WARNING! name difference", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING! WARNING! name difference DOB difference", "WARNING! name and DOB difference", Case_ID_Warning)]
#summary.factor(z$Case_ID_Warning)

# Add RAG rating
z[, Fuzzy_match_RAG := ""]
z[, Fuzzy_match_RAG := ifelse(grepl("Perfect match", Case_ID_Warning),
                             "GREEN",
                             ifelse(grepl("WARNING! name match DOB difference|WARNING! name match no DOB", Case_ID_Warning), 
                                    "AMBER",
                                    ifelse(is.na(Case_ID_Warning) | Case_ID_Warning == "not found",
                                           "Not found",
                                           "RED"))),  
  by = 1:nrow(z)]
z[, Fuzzy_match_RAG := factor(Fuzzy_match_RAG, levels = c("GREEN", "AMBER", "RED"))]
#summary.factor(z$Fuzzy_match_RAG)


# Stringency adjustment for high and medium options. Low retains everything
z <- z[with(z, order(get(case_column), Fuz_score, decreasing = F)), ]
z[,Name_score_pos:= NULL]

removal_cols <- c(c(names(dba$ob)[!(names(dba$ob) %in% c(dba$sn, dba$fn, dba$dob, dba$pc))]), "db_Surname", "db_Name", "db_surname_firstname", "db_DOB", "db_Postcode", "Matched_surname_firstname", "Fuz_score", "Name_score",	"DOB_score",	"PC_score", "Fuzzy_match_RAG",	"Case_ID_Warning")
if (stringency == "medium") {
  # Remove medium and low score data
  z[Name_score < 0.72, which(names(z) %in% removal_cols) := NA]
  z[DOB_score < 0.7 & !is.na(DOB_score), which(names(z) %in% removal_cols) := NA]
  # Remove duplicated rows with low match data
  z <- z[!duplicated(z),]
  # Retain unmatched only if no matches
  cases_NA <- copy(z[is.na(z$db_surname_firstname),get(case_column)])
  cases_matched <- copy(unique(z[!is.na(z$db_surname_firstname),get(case_column)]))
  cases_to_keep <- cases_NA[!(cases_NA %in% cases_matched)]
  z <- z[!(duplicated(z)),]

} else if(stringency == "high"){
  # Remove medium and low score data
  z[Name_score < 0.75, which(names(z) %in% removal_cols) := NA]
  z[DOB_score < 0.8 & !is.na(DOB_score), which(names(z) %in% removal_cols) := NA]
  
  # Remove duplicated rows with low match data
  z <- z[!duplicated(z),]
  # Retain unmatched only if no matches
  cases_NA <- copy(z[is.na(z$db_surname_firstname),get(case_column)])
  cases_matched <- copy(unique(z[!is.na(z$db_surname_firstname),get(case_column)]))
  cases_to_keep <- cases_NA[!(cases_NA %in% cases_matched)]
  z <- z[!(duplicated(z)),]
} else if(stringency == "top"){
  # Remove medium and low score data
  z[Fuzzy_match_RAG == "RED", which(names(z) %in% removal_cols) := NA]
 
  # Remove duplicated rows with low match data
  z <- z[!duplicated(z),]
  # Retain unmatched only if no matches
  cases_NA <- copy(z[is.na(z$db_surname_firstname),get(case_column)])
  cases_matched <- copy(unique(z[!is.na(z$db_surname_firstname),get(case_column)]))
  cases_to_keep <- cases_NA[!(cases_NA %in% cases_matched)]
  z <- z[!(duplicated(z)),]

}


# Add custom line list columns back
if(!(case_column %in% names(llaob))) llaob[,(case_column):= 1:nrow(llaob)]
setDT(llaob)[,(case_column):= lapply(.SD, as.character), .SDcols = case_column]
names <- c(case_column, names(llaob)[!(names(llaob) %in% names(z))])
z <- merge(z, llaob[,..names], by = case_column, all = T)


# Order columns and rows
setDT(z)
setcolorder(z, c(case_column,"Fuzzy_match_RAG", "Case_ID_Warning", "ll_surname_firstname", "Matched_surname_firstname", "db_surname_firstname", "ll_DOB", "db_DOB", "ll_Postcode", "db_Postcode", "Fuz_score", "Name_score", "DOB_score", "PC_score", "db_Surname", "db_Name"))
setnames(z, c("ll_surname_firstname", "db_surname_firstname", "Matched_surname_firstname"), c("ll_surname_firstname", "db_surname_firstname", "Matched_surname_firstname"), skip_absent=T)


cat(" Search completed")

# Return data.frame  
z <- as.data.frame(z)
return(z)
}


