#!/usr/bin/perl -w

###############################################################################
#
# WriteXLSX
#
# Write to an Excel binary file.
#
# Copyright 2014, Marc Schwartz <marc_schwartz@me.com>
#
# This software is distributed under the terms of the GNU General
# Public License Version 2, June 1991.  


# Called as: WriteXLS.pl [--CSVpath] [--verbose] [--Encoding] [--AdjWidth] [--AutoFilter] [--BoldHeaderRow] ExcelFileName

# CSVpath = Path to CSV Files. Defaults to '.'
# verbose = Output status messages. TRUE or FALSE. Defaults to FALSE
# adj.width = Adjust column widths based upon longest entry in each column. Defaults to FALSE
# autofilter = Set autofilter for each sheet. Defaults to FALSE
# bold.header.row = Set bold font for header row. Defaults to FALSE
# Encoding = character encoding. Either "UTF-8" (default) or "latin1" (aka "iso-8859-1")

# Excel::Writer:XLSX
# http://search.cpan.org/~jmcnamara/Excel-Writer-XLSX/

# Excel 2007 specifications and limitations
# http://office.microsoft.com/en-us/excel-help/excel-specifications-and-limits-HP010073849.aspx

# For unicode issues:
# http://www.ahinea.com/en/tech/perl-unicode-struggle.html

use strict;

use Excel::Writer::XLSX;
use Getopt::Long;
use File::Basename;
use Text::CSV_PP;
use Encode;




###############################################################################
# Initialize and get command line arguments
#

my $CSVPath = '.';
my $verbose = "FALSE";
my $AdjWidth = "FALSE";
my $AutoFilter = "FALSE";
my $BoldHeaderRow = "FALSE";
my $Encoding = "UTF-8";
my $FreezeRow = 0;
my $FreezeCol = 0;

GetOptions ('CSVpath=s' => \$CSVPath, 
            'verbose=s' => \$verbose,
            'AdjWidth=s' => \$AdjWidth,
            'AutoFilter=s' => \$AutoFilter,
            'BoldHeaderRow=s' => \$BoldHeaderRow,
            'Encoding=s' => \$Encoding,
            'FreezeRow=i' => \$FreezeRow,
            'FreezeCol=i' => \$FreezeCol);

my $ExcelFileName = $ARGV[0];

my $Row = 0;
my $Column = 0;


###############################################################################
# Create Excel XLS File
#

if ($verbose eq "TRUE") {
  print "Creating Excel File: $ExcelFileName\n\n";
}

my $XLSFile  = Excel::Writer::XLSX->new($ExcelFileName);
die "Problems creating new Excel file: $!" unless defined $XLSFile;




###############################################################################
# Get SheetNames.txt contents for Worksheet Names
#

my @SheetNames = "";
my $SNInd = 0;

open (SNHANDLE, "$CSVPath/SheetNames.txt") || die "ERROR: cannot open $CSVPath/SheetNames.txt. $!\n";
@SheetNames = <SNHANDLE>;
close SNHANDLE;  

# Use chomp() to remove trailing newline ('\n') from each element
# which will be a remnant from reading the file
# Otherwise the newline will be counted in the length of the worksheet name
chomp(@SheetNames);




###############################################################################
# Get data frame file names
#

my @FileNames = "";
open (DFHANDLE, "$CSVPath/FileNames.txt") || die "ERROR: cannot open $CSVPath/FileNames.txt. $!\n";
@FileNames = <DFHANDLE>;
close DFHANDLE;  
# Use chomp() to remove trailing newline ('\n') from each element
# which will be a remnant from reading the file
chomp(@FileNames);




###############################################################################
# if AdjWidth, add a write handler to store the column string widths to enable 
# adjustments
# Based upon code from:
# http://search.cpan.org/dist/Spreadsheet-WriteExcel/lib/Spreadsheet/WriteExcel/Examples.pm#Example:_autofit.pl
# Not using full code base, since we are not formatting using fancy fonts, etc. and it requires yet another external module
# So this will be an approximation




###############################################################################
#
# Adjust the column widths to fit the longest string in the column.
#

sub autofit_columns {

    my $worksheet = shift;
    my $col       = 0;

    for my $width (@{$worksheet->{__col_widths}}) {

	$worksheet->set_column($col, $col, $width) if $width;
	$col++;
    }
}
    



###############################################################################
#
# The following function is a callback that was added via add_write_handler()
# above. It modifies the write() function so that it stores the maximum
# unwrapped width of a string in a column.
#

sub store_string_widths {

    my $worksheet = shift;
    my $col       = $_[1];
    my $token     = $_[2];

    # Ignore some tokens that we aren't interested in.
    return if not defined $token;       # Ignore undefs.
    return if $token eq '';             # Ignore blank cells.
    return if ref $token eq 'ARRAY';    # Ignore array refs.
    return if $token =~ /^=/;           # Ignore formula

    # Ignore numbers
    return if $token =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/;

    # Ignore various internal and external hyperlinks. In a real scenario
    # you may wish to track the length of the optional strings used with
    # urls.
    return if $token =~ m{^[fh]tt?ps?://};
    return if $token =~ m{^mailto:};
    return if $token =~ m{^(?:in|ex)ternal:};


    # We store the string width as data in the Worksheet object. We use
    # a double underscore key name to avoid conflicts with future names.
    #
    my $old_width    = $worksheet->{__col_widths}->[$col];
    my $string_width = string_width($token);

    if (not defined $old_width or $string_width > $old_width) {
	# You may wish to set a minimum column width as follows.
	#return undef if $string_width < 10;

	$worksheet->{__col_widths}->[$col] = $string_width;
    }


    # Return control to write();
    return undef;
}




###############################################################################
#
# Very simple conversion between string length and string width for Arial 10.
# Increases length by 10% of the longest field.

sub string_width {

    return 1.1 * length $_[0];
}




###############################################################################
#
# Write out each worksheet to file
#

foreach my $FileName (@FileNames) {

  if ($verbose eq "TRUE") {
    print "Reading: $FileName\n";
  }

  # Open CSV File
  my $csv = Text::CSV_PP->new ({ binary => 1 });
  open (CSVFILE, "$FileName") || die "ERROR: cannot open $FileName. $!\n";

  # Create new sheet with filename prefix
  # ($base, $dir, $ext) = fileparse ($FileName, '..*');
  my $FName = (fileparse ($FileName, '\..*'))[0];
  
  my $SheetName = "";
  $SheetName = $SheetNames[$SNInd]; 
  $SNInd++;

  if ($verbose eq "TRUE") {
    print "Creating New WorkSheet: $SheetName\n\n";
  }

  my $WorkSheet = $XLSFile->add_worksheet($SheetName);

  # adjust column widths?
  # add a write handler to store the column string widths
  # This is done on a worksheet by worksheet basis and used by functions above
  # See reference above
  if ($AdjWidth eq "TRUE") {
    $WorkSheet->add_write_handler(qr[\w], \&store_string_widths); 
  }

  # Rows and columns are zero indexed
  $Row = 0;

  if ($BoldHeaderRow eq "TRUE") {
    my $bold = $XLSFile->add_format(bold => 1);
    $WorkSheet->set_row(0, undef, $bold);
  }

  my $CommentRow = 0;

  # Write to Sheet
  while (<CSVFILE>) {

    if ($csv->parse($_)) {
      my @Fields = $csv->fields();

      $Column = 0;

      # The row with comments will be 0 if the column names are not 
      # output in the CSV file, 1 otherwise.
      if ($Row <= 1) {
        if (index($Fields[0], "WRITEXLS COMMENT: ") != -1) {
          $CommentRow = 1;

          foreach my $Fld (@Fields) {
            $Fld = substr $Fld, 18;
            if ($Fld ne "") {
              if ($Encoding eq "UTF-8") {
                $WorkSheet->write_comment(0, $Column, decode_utf8($Fld));
	      } else {
                $WorkSheet->write_comment(0, $Column, decode("iso-8859-1", $Fld));
              }
            }

            $Column++; 
	  }
	}
      }

      if ($CommentRow != 1) {
        foreach my $Fld (@Fields) {
          if ($Encoding eq "UTF-8") {
            $WorkSheet->write($Row, $Column, decode_utf8($Fld));
          } else {
            $WorkSheet->write($Row, $Column, decode("iso-8859-1", $Fld));
	  }

          $Column++;
	}

        $Row++;
      }

      $CommentRow = 0;
    }
  }

  close CSVFILE;

  if ($AdjWidth eq "TRUE") {
    autofit_columns($WorkSheet);
  }

  if ($AutoFilter eq "TRUE") {
    $WorkSheet->autofilter(0, 0, $Row - 1, $Column - 1);
  }

  if (($FreezeRow > 0) || ($FreezeCol > 0)) {
    $WorkSheet->freeze_panes($FreezeRow, $FreezeCol);
  }
}

# Explicitly close the Excel file
$XLSFile->close() or die "Error closing file: $!";

