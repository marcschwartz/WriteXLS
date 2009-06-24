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


# Called as: WriteXLS.pl [--CSVpath] [--verbose] [--SN] [--Encoding] ExcelFileName

# CSVpath = Path to CSV Files. Defaults to '.'
# verbose = Output status messages. TRUE or FALSE. Defaults to FALSE
# SN = SheetNames flag. TRUE if SheetNames.txt file present, FALSE if not.
# Encoding = character encoding. Either "UTF-8" (default) or "latin1" (aka "iso-8859-1")

# Spreadsheet::WriteExcel 
# http://search.cpan.org/~jmcnamara/Spreadsheet-WriteExcel/lib/Spreadsheet/WriteExcel.pm

# Excel 2003 specifications and limitations
# http://office.microsoft.com/en-us/excel/HP051992911033.aspx

# For unicode issues:
# http://www.ahinea.com/en/tech/perl-unicode-struggle.html

use strict;

use Spreadsheet::WriteExcel;
use Getopt::Long;
use File::Basename;
use Text::CSV_XS;
use Encode;


# Initialize and get command line arguments
my $CSVPath = '.';
my $verbose = "FALSE";
my $SN = "FALSE";
my $Encoding = "UTF-8";


GetOptions ('CSVpath=s' => \$CSVPath, 
            'verbose=s' => \$verbose,
            'SN=s' => \$SN,
            'Encoding=s' => \$Encoding);

my $ExcelFileName = $ARGV[0];


# Create Excel XLS File
if ($verbose eq "TRUE") {
  print "Creating Excel File: $ExcelFileName\n\n";
}

my $XLSFile  = Spreadsheet::WriteExcel->new($ExcelFileName);


# If SheetNames.txt present, read it
my @SheetNames = "";
my $SNInd = 0;
if ($SN eq "TRUE") {
  open (SNHANDLE, "$CSVPath/SheetNames.txt") || die "ERROR: cannot open $CSVPath/SheetNames.txt. $!\n";
  @SheetNames = <SNHANDLE>;
  close SNHANDLE;  

  # Use chomp() to remove trailing newline ('\n') from each element
  # which will be a remnant from reading the file
  # Otherwise the newline will be counted in the length of the worksheet name
  chomp(@SheetNames);
}


# Get data frame file names
my @FileNames = "";
open (DFHANDLE, "$CSVPath/FileNames.txt") || die "ERROR: cannot open $CSVPath/FileNames.txt. $!\n";
@FileNames = <DFHANDLE>;
close DFHANDLE;  
# Use chomp() to remove trailing newline ('\n') from each element
# which will be a remnant from reading the file
chomp(@FileNames);


foreach my $FileName (@FileNames) {

  if ($verbose eq "TRUE") {
    print "Reading: $FileName\n";
  }

  # Open CSV File
  my $csv = Text::CSV_XS->new ({ binary => 1 });
  open (CSVFILE, "$FileName") || die "ERROR: cannot open $FileName. $!\n";

  # Create new sheet with filename prefix
  # ($base, $dir, $ext) = fileparse ($FileName, '..*');
  my $FName = (fileparse ($FileName, '\..*'))[0];
  
  my $SheetName = "";
  if ($SN eq "TRUE") {
    $SheetName = $SheetNames[$SNInd]; 
    $SNInd++;
  } else {
    # Only take the first 31 chars, which is the
    # limit for a worksheet name
    $SheetName = substr($FName, 0, 31);
  }

  if ($verbose eq "TRUE") {
    print "Creating New WorkSheet: $SheetName\n\n";
  }

  my $WorkSheet = $XLSFile->add_worksheet($SheetName);

  # Rows and columns are zero indexed
  my $Row = 0;

  # Write to Sheet
  while (<CSVFILE>) {

    if ($csv->parse($_)) {
      my @Fields = $csv->fields();

      my $Col = 0;

      foreach my $Fld (@Fields) {
	if ($Encoding eq "UTF-8") {
          $WorkSheet->write($Row, $Col, decode_utf8($Fld));
        } else {
          $WorkSheet->write($Row, $Col, decode("iso-8859-1", $Fld));
	}
        $Col++;
     }
    $Row++;
   }
 }

  close CSVFILE;
}
