\name{testPerl}
\alias{testPerl}
\title{Test Perl installation and required modules for WriteXLS()}
\description{Test Perl installation and required modules for WriteXLS()}
\usage{
  testPerl(perl = "perl", verbose = TRUE)
}
\arguments{
  \item{perl}{Name of the perl executable to be called.}
  \item{verbose}{Output test result messages.}
}
\details{
  This function will test your current system to be sure that Perl is installed
  and if so, whether or not all of the Perl modules required for \code{WriteXLS()}
  are present.

  Success and/or error messages as appropriate will be output. 
}
\value{
  A boolean value (TRUE or FALSE). TRUE if Perl and ALL required modules are found
}
\note{
  Please be sure to read the included INSTALL file (in the main package installation 
  directory) for additional details on meeting the requirements for Perl and the 
  additional Perl modules that are necessary for \code{WriteXLS} to work properly. The file
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
}
\author{
  Marc Schwartz \email{marc\_schwartz@me.com}

  Many thanks to Prof. Brian Ripley for his assistance in the testing of this package. 
}
\seealso{
  \code{\link[WriteXLS]{WriteXLS}}
}
\keyword{file}
