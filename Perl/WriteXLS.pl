#!/usr/bin/perl -w

###############################################################################
#
# WriteXLS
#
# Write to an Excel binary file.
#
# Copyright 2009, Marc Schwartz <marc_schwartz@me.com>
#
# This software is distributed under the terms of the GNU General
# Public License Version 2, June 1991.  


# Called as: WriteXLS.pl [--CSVpath] [--CSVfiles] ExcelFileName

# Spreadsheet::WriteExcel 
# http://search.cpan.org/~jmcnamara/Spreadsheet-WriteExcel/lib/Spreadsheet/WriteExcel.pm

# Excel 2007 specifications and limitations
# http://office.microsoft.com/en-us/excel/HP100738491033.aspx

# For unicode issues:
# http://www.ahinea.com/en/tech/perl-unicode-struggle.html

use strict;

use Spreadsheet::WriteExcel;
use Getopt::Long;
use File::Glob;
use File::Basename;
use Text::CSV_XS;
use Encode;


# Initialize and get command line arguments
my $CSVPath = '.';
my $CSVFiles = "*.csv";

GetOptions ('CSVpath=s' => \$CSVPath, 
            'CSVfiles=s' => \$CSVFiles);

my $ExcelFileName = $ARGV[0];


# Create Excel XLS File
print "Creating Excel File: $ExcelFileName\n\n";
my $XLSFile  = Spreadsheet::WriteExcel->new($ExcelFileName);


# Glob file path and names
my @FileNames = <$CSVPath/$CSVFiles>;


foreach my $FileName (@FileNames) {

  print "Reading: $FileName\n";

  # Open CSV File
  my $csv = Text::CSV_XS->new ({ binary => 1 });
  open (CSVFILE, "$FileName") || die "ERROR: cannot open $FileName. $!\n";

  # Create new sheet with filename prefix
  # ($base, $dir, $ext) = fileparse ($FileName, '..*');
  my $FName = (fileparse ($FileName, '\..*'))[0];

  # Only take the first 31 chars, which is the
  # limit for a worksheet name
  my $SheetName = substr($FName, 0, 31);

  print "Creating New WorkSheet: $SheetName\n\n";

  my $WorkSheet = $XLSFile->add_worksheet($SheetName);

  # Rows and columns are zero indexed
  my $Row = 0;

  # Write to Sheet
  while (<CSVFILE>) {

    if ($csv->parse($_)) {
      my @Fields = $csv->fields();

      my $Col = 0;

      foreach my $Fld (@Fields) {
         $WorkSheet->write($Row, $Col, decode_utf8($Fld));
         $Col++;
     }
    $Row++;
   }
 }

  close CSVFILE;
}
