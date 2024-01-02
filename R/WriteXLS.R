###############################################################################
##
## WriteXLS.R
##
## Write R data frames to an Excel binary file using a Perl script
##
## Copyright 2015-2024, Marc Schwartz <marc_schwartz@me.com>
##
## This software is distributed under the terms of the GNU General
## Public License Version 2, June 1991.  




## Excel 2007 specifications and limitations
## http://office.microsoft.com/en-us/excel-help/excel-specifications-and-limits-HP010073849.aspx




WriteXLS <- function(x, ExcelFileName = "R.xls", SheetNames = NULL, perl = "perl", verbose = FALSE,
                     Encoding = c("UTF-8", "latin1", "cp1252"), AllText = FALSE,
                     row.names = FALSE, col.names = TRUE,
                     AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
                     na = "", FreezeRow = 0, FreezeCol = 0,
                     envir = parent.frame())
{
  
  ## Fix up ExcelFileName to support tilde expansion, etc.
  ExcelFileName <- normalizePath(ExcelFileName, mustWork = FALSE)

  ## Set flag for XLSX file versus XLS file
  XLSX <- grepl("\\.XLSX$", toupper(ExcelFileName))

  
  ## 'x' can be a data frame object, a list object of data frames,
  ## the name (character vector or factor) of one or more data frames or the name (character vector or factor) of a list of data frames.
  ## create a list of data frames from x, for consistency in subsequent processing.

  ## Should a future version of WriteXLS only accept objects, rather than names?
  ## THIS WOULD BREAK EXISTING CODE IN CRAN PACKAGES THAT USE WRITEXLS AND STAND ALONE CODE
  ## UNLESS THERE IS STRONG JUSTIFICATION, DO NOT BREAK EXISTING CODE!  

  ## TRUE if 'x' is a list or data frame object
  if (is.list(x)) {
    if (is.data.frame(x)) {
      DF.LIST <- list(x)
      names(DF.LIST) <- deparse(substitute(x))
    } else {
      DF.LIST <- x
      if ((length(names(DF.LIST)) != length(DF.LIST)) & is.null(SheetNames))
        stop("Either 'x' must be a list with names for each data frame or 'SheetNames' must be specified.")
    }    
  } else if (is.character(x) | is.factor(x)) { 
    if (length(x) == 1) {
      TMP <- get(as.character(x), envir = envir)
      if (is.data.frame(TMP)) {
        DF.LIST <- list(TMP)
        names(DF.LIST) <- x  
      } else if (is.list(TMP)) {
        DF.LIST <- TMP
      } else {
        stop("'x' must be the name of a data frame or the name of a list of data frames.")    
      }  
    } else {
      DF.LIST <- sapply(as.character(x), function(x) get(x, envir = envir), simplify = FALSE)
      names(DF.LIST) <- x
    }
  } else {
    stop("'x' must be the name of a data frame, the name of a list of data frames, a data frame object, a list object of data frames.")    
  }

  
  ## Check to be sure that each element of DF.LIST is a data frame
  if (!all(sapply(DF.LIST, is.data.frame)))
    stop("One or more of the objects named in 'x' is not a data frame or does not exist")

  if (XLSX) {
    ## Additional checks for Excel 2007 limitations
    ## 16,384 columns, including rownames, if included
    ## 1,048,576 rows (including header row)
    if (!all(sapply(DF.LIST, function(x) (nrow(x) <= 1048576) & (ncol(x) <= 16384))))
      stop("One or more of the data frames named in 'x' exceeds 1,048,576 rows or 16,384 columns")
  } else {
    ## Additional checks for Excel 2003 limitations
    ## 256 columns, including rownames, if included
    ## 65,536 rows (including header row)
    if (!all(sapply(DF.LIST, function(x) (nrow(x) <= 65535) & (ncol(x) <= 256))))
      stop("One or more of the data frames named in 'x' exceeds 65,535 rows or 256 columns")
  }

  Encoding <- match.arg(Encoding)
  
  ## Check to see if SheetNames is specified and if so:
  ##  check for duplications
  ##  they are same length as the number of dataframes
  ##  check to see if any SheetNames are >31 chars, which is the Excel Limit
  ##  check for invalid characters: []:*?/\
  ## ELSE
  ##  check to see if first 31 characters of data frame names are unique
  if (!is.null(SheetNames)) {
    if (any(duplicated(SheetNames))) {  
      message("At least one entry in 'SheetNames' is duplicated. Excel worksheets must have unique names.")
      return(invisible(FALSE))
    }
     
    if (length(DF.LIST) != length(SheetNames)) {  
      message("The number of 'SheetNames' specified does not equal the number of data frames in 'x'")
      return(invisible(FALSE))
    }

    if (any(nchar(SheetNames) > 31)) {
      message("At least one of 'SheetNames' is > 31 characters, which is the Excel limit")
      return(invisible(FALSE))
    }

    if (any(grep("\\[|\\]|\\*|\\?|:|/|\\\\", SheetNames))) {  
      message("Invalid characters found in at least one entry in 'SheetNames'. Invalid characters are: []:*?/\\")
      return(invisible(FALSE))
    }

    names(DF.LIST) <- SheetNames
   
  } else {
    if (any(duplicated(substr(names(DF.LIST), 1, 31)))) {
      message("At least one data frame name in 'x' is duplicated up to the first 31 characters. Excel worksheets must have unique names.")
      return(invisible(FALSE))
    }

    if (any(grep("\\[|\\]|\\*|\\?|:|/|\\\\", names(DF.LIST)))) {  
      message("Invalid characters found in at least one data frame name in 'x'. Invalid characters are: []:*?/\\")
      return(invisible(FALSE))
    }  
  }

  ## Function to double any embedded double quote characters
  ## in a field before writing to the CSV files.
  dblQuote <- function(x) {
    gsub("\"", "\"\"", x)
  }
  
  ## Get path to WriteXLS.pl or WriteXLSX.pl
  Perl.Path <- system.file("Perl", package = "WriteXLS")

  PerlScript <- ifelse(XLSX, "WriteXLSX.pl", "WriteXLS.pl")
  
  Fn.Path <- file.path(Perl.Path, PerlScript)

  
  ## Get path for Tmp.Dir for CSV files
  Tmp.Dir <- file.path(tempdir(), "WriteXLS")

  ## Remove Tmp.Dir and Files
  clean.up <- function() {
    if (verbose)
      cat("Cleaning Up Temporary Files and Directory\n\n")

    unlink(Tmp.Dir, recursive = TRUE)
  }

  ## Clean up on function exit
  on.exit(clean.up())

  ## Cleanup now, in case Tmp.Dir still exists from a prior run
  if (file.exists(Tmp.Dir)) {
    if (verbose)
      cat("Cleaning Up Temporary Files and Directory From Prior Run\n\n")
    
    unlink(Tmp.Dir, recursive = TRUE)
  }

  ## Create Tmp.Dir for new run
  if (verbose)
    cat("Creating Temporary Directory for CSV Files: ", Tmp.Dir, "\n\n")
  
  dir.create(Tmp.Dir, recursive = TRUE)

  ##  Write Comma Delimited CSV files
  for (i in seq(along = DF.LIST)) {
    if (verbose)
      cat("Creating CSV File: ", i, ".csv", "\n", sep = "")

    ## Get any column comment attributes and pre-pend to the
    ## data frame as the first row. Non-existing comments
    ## will be NULL, so convert to "" so that there is an
    ## entry for each column and we get a vector, not a list.
    COMMENTS <- sapply(DF.LIST[[i]],
                       function(x) ifelse(is.null(attr(x, "comment")),
                                          "",
                                          attr(x, "comment")))
        
    ## Pre-pend "WRITEXLS COMMENT:" to each comment so that we can differentiate
    ## the comment row from column names, which may or may not be written
    ## out depending upon 'col.names' argument
    COMMENTS <- paste("WRITEXLS COMMENT:", COMMENTS)

    ## Prepare the data frame data for export
    
    ## Need to convert all columns in DF.LIST[[i]] to character
    ## Everything is going to be output via writeLines() as character anyway.
    ## Preserve the rownames from the original DF.LIST, lest they get
    ## re-named to numbers by default.
    ## Set 'optional = TRUE' so that make.names() is not used on non-syntactially
    ## correct column names.
    ROWNAMES <- rownames(DF.LIST[[i]])
    COLNAMES <- colnames(DF.LIST[[i]])
    DF.LIST[[i]] <- data.frame(lapply(DF.LIST[[i]], as.character),
                               stringsAsFactors = FALSE, check.names = FALSE)
    rownames(DF.LIST[[i]]) <- ROWNAMES
    colnames(DF.LIST[[i]]) <- COLNAMES
    
    ## First, replace NAs as indicated by 'na' argument
    NA.IND <- which(is.na(DF.LIST[[i]]), arr.ind = TRUE)
    if (nrow(NA.IND) > 0) {
      DF.LIST[[i]][NA.IND] <- na
    }
    
    ## Add row names to DF and a blank column name if row.names = TRUE
    if (row.names) {
      DF.LIST[[i]] <- cbind(rownames(DF.LIST[[i]]), DF.LIST[[i]])
      colnames(DF.LIST[[i]]) <- c("", COLNAMES)
      COMMENTS <- c("WRITEXLS COMMENT: ", COMMENTS)
    }
    
    ## paste() together each row in preparation for line by line output using writeLines()
    DF.Data <- apply(DF.LIST[[i]], 1, function(x) {paste('"', dblQuote(x), '"', sep = "", collapse = ",")})

    ## paste() together the COMMENTS row
    COMMENTS <- paste('"', dblQuote(COMMENTS), '"', sep = "", collapse = ",")

    ## Output to file using writeLines() with useBytes = TRUE to preserve encodings
    if (col.names) {
      ## paste() together the column names for output
      Cols.Out <- paste('"', dblQuote(colnames(DF.LIST[[i]])), '"', sep = "", collapse = ",")
      Lines.Out <- c(Cols.Out, COMMENTS, DF.Data)
    } else {
      Lines.Out <- c(COMMENTS, DF.Data)
    }
    
    writeLines(Lines.Out, con = paste(Tmp.Dir, "/", i, ".csv", sep = ""), useBytes = TRUE)
  }

  ## Write 'x' (character vector of data frame names) to file
  ## appending Tmp.Dir and ".csv" to each x
  x <- paste(Tmp.Dir, "/", seq(length(DF.LIST)), ".csv", sep = "")
  write(as.matrix(x), file = paste(Tmp.Dir, "/FileNames.txt", sep = ""))

  if (verbose)
    cat("Creating SheetNames.txt\n")
    
  write(as.matrix(names(DF.LIST)), file = paste(Tmp.Dir, "/SheetNames.txt", sep = ""))
  
  if (verbose)
    cat("\n")

  ## Call Perl script
  cmd <- paste(perl,
               " -I", shQuote(Perl.Path),
               " ", shQuote(Fn.Path),
               " --CSVPath ", shQuote(Tmp.Dir),
               " --verbose ", verbose,
               " --AdjWidth ", AdjWidth,
               " --AutoFilter ", AutoFilter,
               " --BoldHeaderRow ", BoldHeaderRow,
               " --FreezeRow ", FreezeRow,
               " --FreezeCol ", FreezeCol,
               " --Encoding ", Encoding,
               " --AllText ", AllText,
               " ", shQuote(ExcelFileName), sep = "")

  ## Call the external Perl script and get the result of the call
  Result <- system(cmd)

  ## Check to see if Result != 0 in the case of the failure of the Perl script
  ## This should also raise an error for R CMD check for package testing on R-Forge and CRAN
  if (Result != 0) {
    err.msg <- paste("The Perl script '", PerlScript, "' failed to run successfully.", sep = "")
    message(err.msg)
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE))
  }
}
