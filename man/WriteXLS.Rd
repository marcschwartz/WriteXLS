\name{WriteXLS}
\alias{WriteXLS}
\title{Cross-platform Perl based R function to create Excel 2003 (XLS) files}
\description{Writes one or more R data frames to an Excel file}
\usage{
  WriteXLS(x, ExcelFileName = "R.xls", SheetNames = NULL, perl = "perl",
           verbose = FALSE, Encoding = c("UTF-8", "latin1"), envir = parent.frame())
}
\arguments{
  \item{x}{A character vector containing the names of one or more R
       data frames to be exported to the Excel file.}
  \item{ExcelFileName}{The name of the Excel file to be created.
       Must be a valid Excel filename. May include an existing path.}
  \item{SheetNames}{A character vector containing the names of each worksheet
       to be created. If \code{NULL} (the default), the
       names of the dataframes will be used instead. Worksheet names may
       be up to 31 characters in length and must be unique. If
       specified, \code{length(SheetNames)} must be the same as
       \code{length(x)}. NOTE: The order of the names here must match
       the order of the data frames as listed in \code{x}.}
  \item{perl}{Name of the perl executable to be called.}
  \item{verbose}{Output step-by-step status messages during the creation
    of the Excel file. Default is FALSE.}
  \item{Encoding}{Define the character encoding to be used for the
    exported data frames. Defaults to \code{UTF-8}.}
  \item{envir}{The environment in which to look for the data frames
       named in \code{x}. This defaults to the environment in which
       \code{WriteXLS} was called.}
}
\details{
  This function takes a character vector containing the names of one or
  more R data frames and writes them to an Excel 2003 spreadsheet file. Each
  data frame will be written to a separate worksheet in the Excel file.

  The order of the worksheets created in the Excel file will match the
  order of the entries in \code{x}.
  
  The actual creation of the Excel file is performed by a Perl script
  called WriteXLS.pl, which is included with this package.

  Note that the named Excel file, if it already exists, will be
  overwritten and no warning is given. In addition, if the file exists
  and is open by another application (eg. Excel, OO.org, etc.) you will
  likely get an error message regarding the inability to open the file
  and/or that the file is already in use by another application or
  user. Errors can also occur if the file has been marked as
  read-only or if your access rights do not allow you to overwrite the
  file or write to the folder you have indicated in the path to the file.

  There is an intermediate step, where the R data frames are first written
  to CSV files using \code{write.table} before being written
  to the Excel file by the Perl script. \code{tempdir} is used to determine the 
  current R session temporary directory and a new sub-directory called "WriteXLS"
  will be created there. The CSV files will be written to that directory and
  both the files and the directory will be deleted prior to the function
  terminating normally using \code{on.exit}. It is possible that these 
  will remain in place if this function terminates abnormally or is aborted
  prior to completion.

  As \code{write.table} is used to write the data frames to CSV files, the 
  data types supported by \code{write.table} will be exported to their 
  character representation correctly. For other data types, it is recommended
  that you first coerce them to character columns formatted as you require
  and then use \code{WriteXLS} to create the Excel file.

  All of the CSV files will be created prior to the creation of the Excel file
  as the Perl script will loop over them as part of the process. Thus, 
  sufficient free disk space must be available for these files and the Excel
  file at the same time.

  If \code{SheetNames} is specified, a text file called "SheetNames.txt"
  will be created in the same temporary directory as the CSV files. This
  file will contain the sheet names, one per line and will be used by
  the Perl script to name the worksheets in the Excel file.

  Each worksheet will be named using either the names in \code{SheetNames} or
  the names of the data frames (up to the first 31 characters, which is an Excel
  limitation). If any the data frames specified in \code{x} are longer
  than 31 characters, they will be truncated to 31
  characters. \code{SheetNames} if specified, will be checked to make
  sure that all of the entries are less than or equal to 31
  characters. If not, an error message will be displayed.

  Note that the order of the names in \code{SheetNames} MUST match the
  order of the data frames named in \code{x}.

  Note that the worksheets must have unique names. Thus, if
  \code{SheetNames} is \code{NULL}, the data frame names will be checked
  to be sure that they are unique up through the first 31 characters. If
  \code{SheetNames} is specified, the entries will be checked to be sure
  that they are unique. If not, an error message will be displayed.

  Note that the following characters are not allowed for Excel worksheet
  names: []:*?/\ 
  
  The data frame column names will be exported "as is" and will be the first 
  row in the corresponding worksheet.

  The data frame row names will NOT be exported.

  UTF-8 encoded content in the data frame should be properly exported using
  the Perl \code{Encode} module by default. If you are operating in a
  'latin1' based locale (also known as iso-8859-1), set \code{Encoding}
  to 'latin1'.
}
\value{
  TRUE if the Excel file was successfully created.
  FALSE if any errors occurred.
}
\note{
  Please be sure to read the included INSTALL file (in the main package installation 
  directory) for additional details on meeting the requirements for Perl and the 
  additional Perl modules that are necessary for this function to work properly. The file
  includes platform specific recommendations for common scenarios. The path to the package
  installation directory can be located using \code{.path.package("WriteXLS")} after using
  \code{library(WriteXLS)}.

  A working installed version of Perl must be present in the current 
  system searchpath or the exact path of the perl executable
  must be provided via the \code{perl} argument.
  Perl modules required for this function that may not be part of a default
  Perl installation are included with this package. These modules include:

  OLE::Storage\_Lite, Parse::RecDescent, Getopt::Long, File::Basename and 
  Spreadsheet::WriteExcel

  Note that the required Perl modules Encode and Text::CSV\_XS are not included
  with this package.  They are platform specific, requiring local compilation and installation 
  from CPAN or via your OS package manager.

  To test your Perl installation and verify that all required Perl modules are available,
  use the \code{testPerl} function provided in this package.
}
\author{
  Marc Schwartz \email{marc\_schwartz@me.com}

  Many thanks to Prof. Brian Ripley for his assistance in the testing of this package. 
}
\references{
  Spreadsheet::WriteExcel Perl Module \url{http://search.cpan.org/dist/Spreadsheet-WriteExcel}

  Excel 2003 Specifications and Limitations \url{http://office.microsoft.com/en-us/excel/HP051992911033.aspx}

  For Perl Unicode Issues \url{http://www.ahinea.com/en/tech/perl-unicode-struggle.html}
}
\seealso{
  \code{\link[utils]{write.table}} and \code{\link[WriteXLS]{testPerl}}
}
\examples{
  # Only run the examples if Perl and all modules are present
  if (testPerl(verbose = FALSE))
  {
    # Examples using built-in data frames 
    WriteXLS("iris", "iris.xls")

    WriteXLS(c("iris", "infert", "esoph"), "Example.xls")

    # Clean up and delete XLS files
    unlink("iris.xls")
    unlink("Example.xls")
  }
}
\keyword{file}
