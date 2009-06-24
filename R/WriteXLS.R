###############################################################################
#
# WriteXLS.R
#
# Write R data frames to an Excel binary file using a Perl script
#
# Copyright 2009, Marc Schwartz <marc_schwartz@me.com>
#
# This software is distributed under the terms of the GNU General
# Public License Version 2, June 1991.  



WriteXLS <- function(x, ExcelFileName = "R.xls", SheetNames = NULL, perl = "perl", verbose = FALSE,
                     Encoding = c("UTF-8", "latin1"), envir = parent.frame())
{
  # Check to be sure that each 'x' is a data frame
  if (!all(sapply(x, function(i) is.data.frame(get(as.character(i), envir = envir)))))
    stop("One or more of the objects named in 'x' is not a data frame or does not exist")

  Encoding <- match.arg(Encoding)
  
  # Check to see if SheetNames is specified and if so:
  #  check for duplications
  #  they are same length as the number of dataframes
  #  check to see if any SheetNames are >31 chars, which is the Excel Limit
  #  check for invalid characters: []:*?/\
  # ELSE
  #  check to see if first 31 characters of data frame names are unique
  if (!is.null(SheetNames))
  {
    if (any(duplicated(SheetNames)))
    {  
      message("At least one entry in 'SheetNames' is duplicated. Excel worksheets must have unique names.")
      return(invisible(FALSE))
    }
     
    if (length(x) != length(SheetNames))
    {  
      message("The number of 'SheetNames' does not equal the number of data frames in 'x'")
      return(invisible(FALSE))
    }

    if (any(nchar(SheetNames) > 31))
    {
      message("At least one of 'SheetNames' is > 31 characters, which is the Excel limit")
      return(invisible(FALSE))
    }

    if (any(grep("\\[|\\]|\\*|\\?|:|/|\\\\", SheetNames)))
    {  
      message("Invalid characters found in at least one entry in 'SheetNames'. Invalid characters are: []:*?/\\")
      return(invisible(FALSE))
    }
  } else {
    if (any(duplicated(substr(x, 1, 31))))
    {
      message("At least one data frame entry in 'x' is duplicated up to the first 31 characters. Excel worksheets must have unique names.")
      return(invisible(FALSE))
    }

    if (any(grep("\\[|\\]|\\*|\\?|:|/|\\\\", x)))
    {  
      message("Invalid characters found in at least one data frame entry in 'x'. Invalid characters are: []:*?/\\")
      return(invisible(FALSE))
    }  
  }
  
  # Get path to WriteXLS.pl
  Perl.Path <- file.path(.path.package("WriteXLS"), "Perl")
  Fn.Path <- file.path(Perl.Path, "WriteXLS.pl")

  # Get path for Tmp.Dir for CSV files
  Tmp.Dir <- file.path(tempdir(), "WriteXLS")

  # Remove Tmp.Dir and Files
  clean.up <- function()
  {
    if (verbose)
      cat("Cleaning Up Temporary Files and Directory\n\n")

    unlink(Tmp.Dir, recursive = TRUE)
  }

  # Clean up on function exit
  on.exit(clean.up())

  # Cleanup now, in case Tmp.Dir still exists from a prior run
  if (file.exists(Tmp.Dir))
  {
    if (verbose)
      cat("Cleaning Up Temporary Files and Directory From Prior Run\n\n")
    
    unlink(Tmp.Dir, recursive = TRUE)
  }

  # Create Tmp.Dir for new run
  if (verbose)
    cat("Creating Temporary Directory for CSV Files: ", Tmp.Dir, "\n\n")
  
  dir.create(Tmp.Dir, recursive = TRUE)

  #  Write Comma Delimited CSV files
  for (i in as.character(x))
  {
    if (verbose)
      cat("Creating CSV File: ", i, "\n")
   
    write.table(get(i, envir = envir), file = paste(Tmp.Dir, "/", i, ".csv", sep = ""),
                sep = ",", quote = TRUE, na = "", row.names = FALSE)
  }

  # Write 'x' (character vector of data frame names) to file
  # appending Tmp.Dir and ".csv" to each x
  x <- paste(Tmp.Dir, "/", x, ".csv", sep = "")
  write(as.matrix(x), file = paste(Tmp.Dir, "/FileNames.txt", sep = ""))

  if (!is.null(SheetNames))
  {
    if (verbose)
      cat("Creating SheetNames.txt\n")
    
    write(as.matrix(SheetNames), file = paste(Tmp.Dir, "/SheetNames.txt", sep = ""))
    SN <- TRUE
  } else {
    SN <- FALSE
  }

  if (verbose)
    cat("\n")

  # Call Perl script
  cmd <- paste(perl,
               " -I", Perl.Path,
               " ", Fn.Path,
               " --CSVPath ", Tmp.Dir,
               " --verbose ", verbose,
               " --SN ", SN,
               " --Encoding ", Encoding,
               " ", ExcelFileName, sep = "")

  # Call the external Perl script and get the result of the call
  Result <- system(cmd)

  # Check to see if Result != 0 in the case of the failure of the Perl script
  # This should also raise an error for R CMD check for package testing on R-Forge and CRAN
  if (Result != 0)
  {
    message("The Perl script 'WriteXLS.pl' failed to run successfully.")
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE))
  }
}
