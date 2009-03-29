\name{WriteXLS}
\alias{WriteXLS}
\title{Cross-platform Perl based R function to create Excel 2003 (XLS) files}
\description{Writes one or more R data frames to an Excel file}
\usage{
  WriteXLS(x, ExcelFileName = "R.xls", perl = "perl", verbose = FALSE)
}
\arguments{
  \item{x}{A character vector containing the names of one or more R 
   data frames to be exported to the Excel file.}
  \item{ExcelFileName}{The name of the Excel file to be created.
   Must be a valid Excel filename. May include an existing path.}
  \item{perl}{Name of the perl executable to be called.}
  \item{verbose}{Output step-by-step status messages during the creation
        of the Excel file. Default is FALSE.}
}
\details{
  This function takes a character vector containing the names of one or
  more R data frames and writes them to an Excel 2003 spreadsheet file. Each
  data frame will be written to a separate worksheet in the Excel file.
  Each worksheet will be named using the name of the data frame (up to the
  first 31 characters, which is an Excel limitation). It is therefore possible
  that if two data frames have the same name up to the 31st character, the 
  corresponding worksheets in the Excel file will have the same name.

  The data frame column names will be exported "as is" and will be the first 
  row in the corresponding worksheet.

  The data frame row names will NOT be exported.

  UTF-8 encoded content in the data frame should be properly exported using
  the Perl \code{Encode} module by default.

  There is an intermediate step, where the R data frames are first written
  to CSV files using \code{write.table} before being written
  to the Excel file. \code{tempdir} is used to determine the 
  current R session temporary directory and a new sub-directory called "WriteXLS"
  will be created there. The CSV files will be written to that directory and
  both the files and the directory will be deleted prior to the function
  terminating normally using \code{on.exit}. It is possible that these 
  will remain in place if this function terminates abnormally or is aborted
  prior to completion.

  All of the CSV files will be created prior to the creation of the Excel file
  as the Perl script will loop over them as part of the process. Thus, 
  sufficient free disk space must be available for these files and the Excel
  file at the same time.
}
\value{
  None
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

  Note that the required Perl modules Encode, File::Glob and Text::CSV\_XS are not included
  with this package.  They are platform specific, requiring local compilation and installation 
  from CPAN or via your OS package manager.

  To test your Perl installation and verify that all required Perl modules are available,
  use the \code{testPerl} function provided in this package.
}
\author{
  Marc Schwartz \email{marcs\_schwartz@me.com}

  Many thanks to Prof. Brian Ripley for his assistance in the testing of this package. 
}
\references{
  Spreadsheet::WriteExcel Perl Module \url{http://search.cpan.org/dist/Spreadsheet-WriteExcel}

  Excel 2003 Specifications and Limitations \url{http://office.microsoft.com/en-us/excel/HP051992911033.aspx}

  For Perl Unicode Issues \url{http://www.ahinea.com/en/tech/perl-unicode-struggle.html}
}
\seealso{
  \code{\link[base]{write.table}} and \code{\link[WriteXLS]{testPerl}}
}
\examples{
  # Only run the examples if Perl and all modules are present
  if (testPerl())
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
