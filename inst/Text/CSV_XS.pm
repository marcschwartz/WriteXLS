package Text::CSV_XS;

# Copyright (c) 2007-2009 H.Merijn Brand.  All rights reserved.
# Copyright (c) 1998-2001 Jochen Wiedmann. All rights reserved.
# Portions Copyright (c) 1997 Alan Citterman. All rights reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

################################################################################
# HISTORY
#
# Written by:
#    Jochen Wiedmann <joe@ispsoft.de>
#
# Based on Text::CSV by:
#    Alan Citterman <alan@mfgrtl.com>
#
# Extended and Remodelled by:
#    H.Merijn Brand (h.m.brand@xs4all.nl)
#
############################################################################

require 5.005;

use strict;
use warnings;

use DynaLoader ();
use Carp;

use vars   qw( $VERSION @ISA );
$VERSION = "0.60";
@ISA     = qw( DynaLoader );
bootstrap Text::CSV_XS $VERSION;

sub PV { 0 }
sub IV { 1 }
sub NV { 2 }

# version
#
#   class/object method expecting no arguments and returning the version
#   number of Text::CSV.  there are no side-effects.

sub version
{
    return $VERSION;
    } # version

# new
#
#   class/object method expecting no arguments and returning a reference to
#   a newly created Text::CSV object.

my %def_attr = (
    quote_char		=> '"',
    escape_char		=> '"',
    sep_char		=> ',',
    eol			=> '',
    always_quote	=> 0,
    binary		=> 0,
    keep_meta_info	=> 0,
    allow_loose_quotes	=> 0,
    allow_loose_escapes	=> 0,
    allow_whitespace	=> 0,
    blank_is_undef	=> 0,
    verbatim		=> 0,
    types		=> undef,

    _EOF		=> 0,
    _STATUS		=> undef,
    _FIELDS		=> undef,
    _FFLAGS		=> undef,
    _STRING		=> undef,
    _ERROR_INPUT	=> undef,
    _COLUMN_NAMES	=> undef,
    _BOUND_COLUMNS	=> undef,
    );
my $last_new_err = Text::CSV_XS->SetDiag (0);

sub new
{
    $last_new_err =
	"usage: my \$csv = Text::CSV_XS->new ([{ option => value, ... }]);";
    my $proto = shift;
    my $attr  = shift || {};
    my $class = ref ($proto) || $proto	or return;
    for (keys %{$attr}) {
	if (m/^[a-z]/ && exists $def_attr{$_}) {
	    $] >= 5.008002 && m/_char$/ and utf8::decode $attr->{$_};
	    next;
	    }
#	croak?
	$last_new_err = "Unknown attribute '$_'";
	return;
	}
    $last_new_err = SetDiag (undef, 0);
    my $self  = {%def_attr, %{$attr}};
    defined $\ && !exists $attr->{eol} and $self->{eol} = $\;
    bless $self, $class;
    defined $self->{types} and $self->types ($self->{types});
    $self;
    } # new

my %_cache_id = (	# Keep in sync with XS!
    quote_char		=>  0,
    escape_char		=>  1,
    sep_char		=>  2,
    binary		=>  3,
    keep_meta_info	=>  4,
    always_quote	=>  5,
    allow_loose_quotes	=>  6,
    allow_loose_escapes	=>  7,
    allow_double_quoted	=>  8,
    allow_whitespace	=>  9,
    blank_is_undef	=> 10,

    eol			=> 11,	# 11 .. 18
    eol_len		=> 19,
    eol_is_cr		=> 20,
    has_types		=> 21,
    verbatim		=> 22,

    _is_bound		=> 23,	# 23 .. 26
    );

sub _set_attr_C
{
    my ($self, $name, $val) = @_;
    defined $val or $val = 0;
    $] >= 5.008002 and utf8::decode $val;
    $self->{$name} = $val;
    $self->{_CACHE} or return;
    my @cache = unpack "C*", $self->{_CACHE};
    $cache[$_cache_id{$name}] = unpack "C", $val;
    $self->{_CACHE} = pack "C*", @cache;
    } # _set_attr_C

sub _set_attr_N
{
    my ($self, $name, $val) = @_;
    $self->{$name} = $val;
    my @cache = unpack "C*", $self->{_CACHE};
    my $i = $_cache_id{$name};
    $cache[$i++] = $_ for unpack "C*", pack "N", $val;
    $self->{_CACHE} = pack "C*", @cache;
    } # _set_attr_N

# Accessor methods.
#   It is unwise to change them halfway through a single file!
sub quote_char
{
    my $self = shift;
    @_ and $self->_set_attr_C ("quote_char", shift);
    $self->{quote_char};
    } # quote_char

sub escape_char
{
    my $self = shift;
    @_ and $self->_set_attr_C ("escape_char", shift);
    $self->{escape_char};
    } # escape_char

sub sep_char
{
    my $self = shift;
    @_ and $self->_set_attr_C ("sep_char", shift);
    $self->{sep_char};
    } # sep_char

sub eol
{
    my $self = shift;
    if (@_) {
	my $eol = shift;
	defined $eol or $eol = "";
	my $eol_len = length $eol;
	$self->{eol} = $eol;
	$self->{_CACHE} or return;
	my @cache = unpack "C*", $self->{_CACHE};
	if (($cache[$_cache_id{eol_len}] = $eol_len) < 8) {
	    $cache[$_cache_id{eol_is_cr}] = $eol eq "\r" ? 1 : 0;
	    }
	else {
	    $cache[$_cache_id{eol_is_cr}] = 0;
	    }
	$eol .= "\0\0\0\0\0\0\0\0";
	$cache[$_cache_id{eol} + $_] = unpack "C", substr $eol, $_, 1 for 0 .. 7;
	$self->{_CACHE} = pack "C*", @cache;
	}
    $self->{eol};
    } # eol

sub always_quote
{
    my $self = shift;
    @_ and $self->_set_attr_C ("always_quote", shift);
    $self->{always_quote};
    } # always_quote

sub binary
{
    my $self = shift;
    @_ and $self->_set_attr_C ("binary", shift);
    $self->{binary};
    } # binary

sub keep_meta_info
{
    my $self = shift;
    @_ and $self->_set_attr_C ("keep_meta_info", shift);
    $self->{keep_meta_info};
    } # keep_meta_info

sub allow_loose_quotes
{
    my $self = shift;
    @_ and $self->_set_attr_C ("allow_loose_quotes", shift);
    $self->{allow_loose_quotes};
    } # allow_loose_quotes

sub allow_loose_escapes
{
    my $self = shift;
    @_ and $self->_set_attr_C ("allow_loose_escapes", shift);
    $self->{allow_loose_escapes};
    } # allow_loose_escapes

sub allow_whitespace
{
    my $self = shift;
    @_ and $self->_set_attr_C ("allow_whitespace", shift);
    $self->{allow_whitespace};
    } # allow_whitespace

sub blank_is_undef
{
    my $self = shift;
    @_ and $self->_set_attr_C ("blank_is_undef", shift);
    $self->{blank_is_undef};
    } # blank_is_undef

sub verbatim
{
    my $self = shift;
    @_ and $self->_set_attr_C ("verbatim", shift);
    $self->{verbatim};
    } # verbatim

# status
#
#   object method returning the success or failure of the most recent
#   combine () or parse ().  there are no side-effects.

sub status
{
    my $self = shift;
    return $self->{_STATUS};
    } # status

sub eof
{
    my $self = shift;
    return $self->{_EOF};
    } # status

# error_input
#
#   object method returning the first invalid argument to the most recent
#   combine () or parse ().  there are no side-effects.

sub error_input
{
    my $self = shift;
    return $self->{_ERROR_INPUT};
    } # error_input

# erro_diag
#
#   If (and only if) an error occured, this function returns a code that
#   indicates the reason of failure

sub error_diag
{
    my $self = shift;
    my @diag = (0, $last_new_err, 0);

    unless ($self && ref $self) {	# Class method or direct call
	$last_new_err and $diag[0] = 1000;
	}
    elsif ($self->isa (__PACKAGE__) && exists $self->{_ERROR_DIAG}) {
	@diag = (0 + $self->{_ERROR_DIAG}, $self->{_ERROR_DIAG});
	exists $self->{_ERROR_POS} and $diag[2] = 1 + $self->{_ERROR_POS};
	}
    my $context = wantarray;
    unless (defined $context) {	# Void context
	$diag[0] and print STDERR "# CSV_XS ERROR: $diag[0] - $diag[1]\n";
	return;
	}
    return $context ? @diag : $diag[1];
    } # error_diag

# string
#
#   object method returning the result of the most recent combine () or the
#   input to the most recent parse (), whichever is more recent.  there are
#   no side-effects.

sub string
{
    my $self = shift;
    return ref $self->{_STRING} ? ${$self->{_STRING}} : undef;
    } # string

# fields
#
#   object method returning the result of the most recent parse () or the
#   input to the most recent combine (), whichever is more recent.  there
#   are no side-effects.

sub fields
{
    my $self = shift;
    return ref $self->{_FIELDS} ? @{$self->{_FIELDS}} : undef;
    } # fields

# meta_info
#
#   object method returning the result of the most recent parse () or the
#   input to the most recent combine (), whichever is more recent.  there
#   are no side-effects. meta_info () returns (if available)  some of the
#   field's properties

sub meta_info
{
    my $self = shift;
    return ref $self->{_FFLAGS} ? @{$self->{_FFLAGS}} : undef;
    } # meta_info

sub is_quoted
{
    my ($self, $idx, $val) = @_;
    ref $self->{_FFLAGS} &&
	$idx >= 0 && $idx < @{$self->{_FFLAGS}} or return;
    $self->{_FFLAGS}[$idx] & 0x0001 ? 1 : 0;
    } # is_quoted

sub is_binary
{
    my ($self, $idx, $val) = @_;
    ref $self->{_FFLAGS} &&
	$idx >= 0 && $idx < @{$self->{_FFLAGS}} or return;
    $self->{_FFLAGS}[$idx] & 0x0002 ? 1 : 0;
    } # is_binary

# combine
#
#   object method returning success or failure.  the given arguments are
#   combined into a single comma-separated value.  failure can be the
#   result of no arguments or an argument containing an invalid character.
#   side-effects include:
#      setting status ()
#      setting fields ()
#      setting string ()
#      setting error_input ()

sub combine
{
    my $self = shift;
    my $str  = "";
    $self->{_FIELDS} = \@_;
    $self->{_FFLAGS} = undef;
    $self->{_STATUS} = (@_ > 0) && $self->Combine (\$str, \@_, 0);
    $self->{_STRING} = \$str;
    $self->{_STATUS};
    } # combine

# parse
#
#   object method returning success or failure.  the given argument is
#   expected to be a valid comma-separated value.  failure can be the
#   result of no arguments or an argument containing an invalid sequence
#   of characters. side-effects include:
#      setting status ()
#      setting fields ()
#      setting meta_info ()
#      setting string ()
#      setting error_input ()

sub parse
{
    my ($self, $str) = @_;

    my $fields = [];
    my $fflags = [];
    $self->{_STRING} = \$str;
    if (defined $str && $self->Parse ($str, $fields, $fflags)) {
	$self->{_ERROR_INPUT} = undef;
	$self->{_FIELDS} = $fields;
	$self->{_FFLAGS} = $fflags;
	$self->{_STATUS} = 1;
	}
    else {
	$self->{_FIELDS} = undef;
	$self->{_FFLAGS} = undef;
	$self->{_STATUS} = 0;
	}
    $self->{_STATUS};
    } # parse

sub column_names
{
    my ($self, @keys) = @_;
    @keys or
	return defined $self->{_COLUMN_NAMES} ? @{$self->{_COLUMN_NAMES}} : undef;

    @keys == 1 && ! defined $keys[0] and
	return $self->{_COLUMN_NAMES} = undef;

    if (@keys == 1 && ref $keys[0] eq "ARRAY") {
	@keys = @{$keys[0]};
	}
    elsif (join "", map { defined $_ ? ref $_ : "" } @keys) {
	croak ($self->SetDiag (3001));
	}

    $self->{_BOUND_COLUMNS} && @keys != @{$self->{_BOUND_COLUMNS}} and
	croak ($self->SetDiag (3003));

    $self->{_COLUMN_NAMES} = [ map { defined $_ ? $_ : "\cAUNDEF\cA" } @keys ];
    @{$self->{_COLUMN_NAMES}};
    } # column_names

sub bind_columns
{
    my ($self, @refs) = @_;
    @refs or
	return defined $self->{_BOUND_COLUMNS} ? @{$self->{_BOUND_COLUMNS}} : undef;

    @refs == 1 && ! defined $refs[0] and
	return $self->{_BOUND_COLUMNS} = undef;

    $self->{_COLUMN_NAMES} && @refs != @{$self->{_COLUMN_NAMES}} and
	croak ($self->SetDiag (3003));

    join "", map { ref $_ eq "SCALAR" ? "" : "*" } @refs and
	croak ($self->SetDiag (3004));

    $self->_set_attr_N ("_is_bound", scalar @refs);
    $self->{_BOUND_COLUMNS} = [ @refs ];
    @refs;
    } # bind_columns

sub getline_hr
{
    my ($self, @args, %hr) = @_;
    $self->{_COLUMN_NAMES} or croak ($self->SetDiag (3002));
    my $fr = $self->getline (@args) or return;
    @hr{@{$self->{_COLUMN_NAMES}}} = @$fr;
    \%hr;
    } # getline_hr

sub types
{
    my $self = shift;
    if (@_) {
	if (my $types = shift) {
	    $self->{_types} = join "", map { chr $_ } @{$types};
	    $self->{types}  = $types;
	    }
	else {
	    delete $self->{types};
	    delete $self->{_types};
	    undef;
	    }
	}
    else {
	$self->{types};
	}
    } # types

1;

__END__

=head1 NAME

Text::CSV_XS - comma-separated values manipulation routines

=head1 SYNOPSIS

 use Text::CSV_XS;

 $csv = Text::CSV_XS->new ();          # create a new object
 $csv = Text::CSV_XS->new (\%attr);    # create a new object

 $status  = $csv->combine (@columns);  # combine columns into a string
 $line    = $csv->string ();           # get the combined string

 $status  = $csv->parse ($line);       # parse a CSV string into fields
 @columns = $csv->fields ();           # get the parsed fields

 $status       = $csv->status ();      # get the most recent status
 $bad_argument = $csv->error_input (); # get the most recent bad argument
 $diag         = $csv->error_diag ();  # if an error occured, explains WHY

 $status = $csv->print ($io, $colref); # Write an array of fields
                                       # immediately to a file $io
 $colref = $csv->getline ($io);        # Read a line from file $io,
                                       # parse it and return an array
                                       # ref of fields
 $csv->bind_columns (@refs);           # Set return fields for getline ()
 $csv->column_names (@names);          # Set column names for getline_hr ()
 $ref = $csv->getline_hr ($io);        # getline (), but returns a hashref
 $eof = $csv->eof ();                  # Indicate if last parse or
                                       # getline () hit End Of File

 $csv->types (\@t_array);              # Set column types

=head1 DESCRIPTION

Text::CSV_XS provides facilities for the composition and decomposition of
comma-separated values.  An instance of the Text::CSV_XS class can combine
fields into a CSV string and parse a CSV string into fields.

The module accepts either strings or files as input and can utilize any
user-specified characters as delimiters, separators, and escapes so it is
perhaps better called ASV (anything separated values) rather than just CSV.

=head2 Embedded newlines

B<Important Note>: The default behavior is to only accept ascii characters.
This means that fields can not contain newlines. If your data contains
newlines embedded in fields, or characters above 0x7e (tilde), or binary data,
you *must* set C<< binary => 1 >> in the call to C<new ()>.  To cover the widest
range of parsing options, you will always want to set binary.

But you still have the problem that you have to pass a correct line to the
C<parse ()> method, which is more complicated from the usual point of
usage:

 my $csv = Text::CSV_XS->new ({ binary => 1, eol => $/ });
 while (<>) {		#  WRONG!
     $csv->parse ($_);
     my @fields = $csv->fields ();

will break, as the while might read broken lines, as that doesn't care
about the quoting. If you need to support embedded newlines, the way to go
is either

 my $csv = Text::CSV_XS->new ({ binary => 1, eol => $/ });
 while (my $row = $csv->getline (*ARGV)) {
     my @fields = @$row;

or, more safely in perl 5.6 and up

 my $csv = Text::CSV_XS->new ({ binary => 1, eol => $/ });
 open my $io, "<", $file or die "$file: $!";
 while (my $row = $csv->getline ($io)) {
     my @fields = @$row;

=head2 Unicode (UTF8)

On parsing (both for C<getline ()> and C<parse ()>), if the source is
marked being UTF8, then parsing that source will mark all fields that
are marked binary will also be marked UTF8.

On combining (C<print ()> and C<combine ()>), if any of the combining
fields was marked UTF8, the resulting string will be marked UTF8.

For complete control over encoding, please use Text::CSV::Encoded:

    use Text::CSV::Encoded;
    my $csv = Text::CSV::Encoded->new ({
        encoding_in  => "iso-8859-1", # the encoding comes into   Perl
        encoding_out => "cp1252",     # the encoding comes out of Perl
        });

    $csv = Text::CSV::Encoded->new ({ encoding  => "utf8" });
    # combine () and print () accept *literally* utf8 encoded data
    # parse () and getline () return *literally* utf8 encoded data

    $csv = Text::CSV::Encoded->new ({ encoding  => undef }); # default
    # combine () and print () accept UTF8 marked data
    # parse () and getline () return UTF8 marked data

=head1 SPECIFICATION

While no formal specification for CSV exists, RFC 4180 1) describes a common
format and establishes "text/csv" as the MIME type registered with the IANA.

Many informal documents exist that describe the CSV format. How To: The Comma
Separated Value (CSV) File Format 2) provides an overview of the CSV format in
the most widely used applications and explains how it can best be used and
supported.

 1) http://tools.ietf.org/html/rfc4180
 2) http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm

The basic rules are as follows:

B<CSV> is a delimited data format that has fields/columns separated by the comma
character and records/rows separated by newlines. Fields that contain a special
character (comma, newline, or double quote), must be enclosed in double quotes.
However, if a line contains a single entry which is the empty string, it may be
enclosed in double quotes. If a field's value contains a double quote character
it is escaped by placing another double quote character next to it. The CSV file
format does not require a specific character encoding, byte order, or line
terminator format.

=over 2

=item *

Each record is one line terminated by a line feed (ASCII/LF=0x0A) or a
carriage return and line feed pair (ASCII/CRLF=0x0D 0x0A), however,
line-breaks can be embedded.

=item *

Fields are separated by commas.

=item *

Allowable characters within a CSV field include 0x09 (tab) and the inclusive
range of 0x20 (space) through 0x7E (tilde). In binary mode all characters
are accepted, at least in quoted fields.

=item *

A field within CSV must be surrounded by double-quotes to contain a
the separator character (comma).

=back

Though this is the most clear and restrictive definition, Text::CSV_XS is way
more liberal than this, and allows extension:

=over 2

=item *

Line termination by a single carriage return is accepted by default

=item *

The separation-, escape-, and escape- characters can be any ASCII character
in the range from 0x20 (space) to 0x7E (tilde). Characters outside this range
may or may not work as expected. Multibyte characters, like U+060c (ARABIC
COMMA), U+FF0C (FULLWIDTH COMMA), U+241B (SYMBOL FOR ESCAPE), U+2424 (SYMBOL
FOR NEWLINE), U+FF02 (FULLWIDTH QUOTATION MARK), and U+201C (LEFT DOUBLE
QUOTATION MARK) (to give some examples of what might look promising) are
therefor not allowed.

If you use perl-5.8.2 or higher, these three attributes are utf8-decoded, to
increase the likelyhood of success. This way U+00FE will be allowed as a
quote character.

=item *

A field within CSV must be surrounded by double-quotes to contain an embedded
double-quote, represented by a pair of consecutive double-quotes. In binary
mode you may additionally use the sequence C<"0> for representation of a
NULL byte.

=item *

Several violations of the above specification may be allowed by passing
options to the object creator.

=back

=head1 FUNCTIONS

=head2 version ()

(Class method) Returns the current module version.

=head2 new (\%attr)

(Class method) Returns a new instance of Text::CSV_XS. The objects
attributes are described by the (optional) hash ref C<\%attr>.
Currently the following attributes are available:

=over 4

=item eol

An end-of-line string to add to rows. C<undef> is replaced with an
empty string. The default is C<$\>. Common values for C<eol> are
C<"\012"> (Line Feed) or C<"\015\012"> (Carriage Return, Line Feed).
Cannot be longer than 7 (ASCII) characters.

If both C<$/> and C<eol> equal C<"\015">, parsing lines that end on
only a Carriage Return without Line Feed, will be C<parse>d correct.
Line endings, whether in C<$/> or C<eol>, other than C<undef>,
C<"\n">, C<"\r\n">, or C<"\r"> are not (yet) supported for parsing.

=item sep_char

The char used for separating fields, by default a comma. (C<,>).
Limited to a single-byte character, usually in the range from 0x20
(space) to 0x7e (tilde).

The separation character can not be equal to the quote character.
The separation character can not be equal to the escape character.

See also CAVEATS

=item allow_whitespace

When this option is set to true, whitespace (TAB's and SPACE's)
surrounding the separation character is removed when parsing. So
lines like:

  1 , "foo" , bar , 3 , zapp

are now correctly parsed, even though it violates the CSV specs.
Note that B<all> whitespace is stripped from start and end of each
field. That would make is more a I<feature> than a way to be able
to parse bad CSV lines, as

 1,   2.0,  3,   ape  , monkey

will now be parsed as

 ("1", "2.0", "3", "ape", "monkey")

even if the original line was perfectly sane CSV.

=item blank_is_undef

Under normal circumstances, CSV data makes no distinction between
quoted- and unquoted empty fields. They both end up in an empty
string field once read, so

 1,"",," ",2

is read as

 ("1", "", "", " ", "2")

When I<writing> CSV files with C<always_quote> set, the unquoted empty
field is the result of an undefined value. To make it possible to also
make this distinction when reading CSV data, the C<blank_is_undef> option
will cause unquoted empty fields to be set to undef, causing the above to
be parsed as

 ("1", "", undef, " ", "2")

=item quote_char

The char used for quoting fields containing blanks, by default the
double quote character (C<">). A value of undef suppresses
quote chars. (For simple cases only).
Limited to a single-byte character, usually in the range from 0x20
(space) to 0x7e (tilde).

The quote character can not be equal to the separation character.

=item allow_loose_quotes

By default, parsing fields that have C<quote_char> characters inside
an unquoted field, like

 1,foo "bar" baz,42

would result in a parse error. Though it is still bad practice to
allow this format, we cannot help there are some vendors that make
their applications spit out lines styled like this.

In case there is B<really> bad CSV data, like

 1,"foo "bar" baz",42

or

 1,""foo bar baz"",42

there is a way to get that parsed, and leave the quotes inside the quoted
field as-is. This can be achieved by setting C<allow_loose_quotes> B<AND>
making sure that the C<escape_char> is I<not> equal to C<quote_char>.

=item escape_char

The character used for escaping certain characters inside quoted fields.
Limited to a single-byte character, usually in the range from 0x20
(space) to 0x7e (tilde).

The C<escape_char> defaults to being the literal double-quote mark (C<">)
in other words, the same as the default C<quote_char>. This means that
doubling the quote mark in a field escapes it:

  "foo","bar","Escape ""quote mark"" with two ""quote marks""","baz"

If you change the default quote_char without changing the default
escape_char, the escape_char will still be the quote mark.  If instead
you want to escape the quote_char by doubling it, you will need to change
the escape_char to be the same as what you changed the quote_char to.

The escape character can not be equal to the separation character.

=item allow_loose_escapes

By default, parsing fields that have C<escape_char> characters that
escape characters that do not need to be escaped, like:

 my $csv = Text::CSV_XS->new ({ escape_char => "\\" });
 $csv->parse (qq{1,"my bar\'s",baz,42});

would result in a parse error. Though it is still bad practice to
allow this format, this option enables you to treat all escape character
sequences equal.

=item binary

If this attribute is TRUE, you may use binary characters in quoted fields,
including line feeds, carriage returns and NULL bytes. (The latter must
be escaped as C<"0>.) By default this feature is off.

If a string is marked UTF8, binary will be turned on automatically when
binary characters other than CR or NL are encountered. Note that a simple
string like C<"\x{00a0}"> might still be binary, but not marked UTF8, so
setting C<{ binary => 1 }> is still a wise option.

=item types

A set of column types; this attribute is immediately passed to the
I<types> method below. You must not set this attribute otherwise,
except for using the I<types> method. For details see the description
of the I<types> method below.

=item always_quote

By default the generated fields are quoted only, if they need to, for
example, if they contain the separator. If you set this attribute to
a TRUE value, then all fields will be quoted. This is typically easier
to handle in external applications. (Poor creatures who aren't using
Text::CSV_XS. :-)

=item keep_meta_info

By default, the parsing of input lines is as simple and fast as
possible. However, some parsing information - like quotation of
the original field - is lost in that process. Set this flag to
true to be able to retrieve that information after parsing with
the methods C<meta_info ()>, C<is_quoted ()>, and C<is_binary ()>
described below.  Default is false.

=item verbatim

This is a quite controversial attribute to set, but it makes hard
things possible.

The basic thought behind this is to tell the parser that the normally
special characters newline (NL) and Carriage Return (CR) will not be
special when this flag is set, and be dealt with as being ordinary
binary characters. This will ease working with data with embedded
newlines.

When C<verbatim> is used with C<getline ()>, C<getline ()>
auto-chomp's every line.

Imagine a file format like

  M^^Hans^Janssen^Klas 2\n2A^Ja^11-06-2007#\r\n

where, the line ending is a very specific "#\r\n", and the sep_char
is a ^ (caret). None of the fields is quoted, but embedded binary
data is likely to be present. With the specific line ending, that
shouldn't be too hard to detect.

By default, Text::CSV_XS' parse function however is instructed to only
know about "\n" and "\r" to be legal line endings, and so has to deal
with the embedded newline as a real end-of-line, so it can scan the next
line if binary is true, and the newline is inside a quoted field.
With this attribute however, we can tell parse () to parse the line
as if \n is just nothing more than a binary character.

For parse () this means that the parser has no idea about line ending
anymore, and getline () chomps line endings on reading.

=back

To sum it up,

 $csv = Text::CSV_XS->new ();

is equivalent to

 $csv = Text::CSV_XS->new ({
     quote_char          => '"',
     escape_char         => '"',
     sep_char            => ',',
     eol                 => $\,
     always_quote        => 0,
     binary              => 0,
     keep_meta_info      => 0,
     allow_loose_quotes  => 0,
     allow_loose_escapes => 0,
     allow_whitespace    => 0,
     blank_is_undef      => 0,
     verbatim            => 0,
     });

For all of the above mentioned flags, there is an accessor method
available where you can inquire for the current value, or change
the value

 my $quote = $csv->quote_char;
 $csv->binary (1);

It is unwise to change these settings halfway through writing CSV
data to a stream. If however, you want to create a new stream using
the available CSV object, there is no harm in changing them.

If the C<new ()> constructor call fails, it returns C<undef>, and makes
the fail reason available through the C<error_diag ()> method.

 $csv = Text::CSV_XS->new ({ ecs_char => 1 }) or
     die Text::CSV_XS->error_diag ();

C<error_diag ()> will return a string like

 "Unknown attribute 'ecs_char'"

=head2 combine

 $status = $csv->combine (@columns);

This object function constructs a CSV string from the arguments, returning
success or failure.  Failure can result from lack of arguments or an argument
containing an invalid character.  Upon success, C<string ()> can be called to
retrieve the resultant CSV string.  Upon failure, the value returned by
C<string ()> is undefined and C<error_input ()> can be called to retrieve an
invalid argument.

=head2 print

 $status = $csv->print ($io, $colref);

Similar to combine, but it expects an array ref as input (not an array!)
and the resulting string is not really created, but immediately written
to the I<$io> object, typically an IO handle or any other object that
offers a I<print> method. Note, this implies that the following is wrong:

 open FILE, ">", "whatever";
 $status = $csv->print (\*FILE, $colref);

The glob C<\*FILE> is not an object, thus it doesn't have a print
method. The solution is to use an IO::File object or to hide the
glob behind an IO::Wrap object. See L<IO::File(3)> and L<IO::Wrap(3)>
for details.

For performance reasons the print method doesn't create a result string.
In particular the I<$csv-E<gt>string ()>, I<$csv-E<gt>status ()>,
I<$csv->fields ()> and I<$csv-E<gt>error_input ()> methods are meaningless
after executing this method.

=head2 string

 $line = $csv->string ();

This object function returns the input to C<parse ()> or the resultant CSV
string of C<combine ()>, whichever was called more recently.

=head2 parse

 $status = $csv->parse ($line);

This object function decomposes a CSV string into fields, returning
success or failure.  Failure can result from a lack of argument or the
given CSV string is improperly formatted.  Upon success, C<fields ()> can
be called to retrieve the decomposed fields .  Upon failure, the value
returned by C<fields ()> is undefined and C<error_input ()> can be called
to retrieve the invalid argument.

You may use the I<types ()> method for setting column types. See the
description below.

=head2 getline

 $colref = $csv->getline ($io);

This is the counterpart to print, like parse is the counterpart to
combine: It reads a row from the IO object $io using $io->getline ()
and parses this row into an array ref. This array ref is returned
by the function or undef for failure.

When fields are bound with C<bind_columns ()>, the return value is a
reference to an empty list.

The I<$csv-E<gt>string ()>, I<$csv-E<gt>fields ()> and I<$csv-E<gt>status ()>
methods are meaningless, again.

=head2 getline_hr

The C<getline_hr ()> and C<column_names ()> methods work together to allow
you to have rows returned as hashrefs. You must call C<column_names ()>
first to declare your column names.

 $csv->column_names (qw( code name price description ));
 $hr = $csv->getline_hr ($io);
 print "Price for $hr->{name} is $hr->{price} EUR\n";

C<getline_hr ()> will croak if called before C<column_names ()>.

=head2 column_names

Set the keys that will be used in the C<getline_hr ()> calls. If no keys
(column names) are passed, it'll return the current setting.

C<column_names ()> accepts a list of scalars (the column names) or a
single array_ref, so you can pass C<getline ()>

  $csv->column_names ($csv->getline ($io));

C<column_names ()> does B<no> checking on duplicates at all, which might
lead to unwanted results. Undefined entries will be replaced with the
string C<"\cAUNDEF\cA">, so

  $csv->column_names (undef, "", "name", "name");
  $hr = $csv->getline_hr ($io);

Will set C<$hr->{"\cAUNDEF\cA"}> to the 1st field, C<$hr->{""}> to the
2nd field, and C<$hr->{name}> to the 4th field, discarding the 2rd field.

C<column_names ()> croaks on invalid arguments.

=head2 bind_columns

Takes a list of references to scalars to store the fields fetched
C<getline ()> in. When you don't pass enough references to store the
fetched fields in, C<getline ()> will fail. If you pass more than there are
fields to return, the remaining references are left untouched.

  $csv->bind_columns (\$code, \$name, \$price, \$description);
  while ($csv->getline ($io)) {
      print "The price of a $name is \x{20ac} $price\n";
      }

=head2 eof

 $eof = $csv->eof ();

If C<parse ()> or C<getline ()> was used with an IO stream, this
method will return true (1) if the last call hit end of file, otherwise
it will return false (''). This is useful to see the difference between
a failure and end of file.

=head2 types

 $csv->types (\@tref);

This method is used to force that columns are of a given type. For
example, if you have an integer column, two double columns and a
string column, then you might do a

 $csv->types ([Text::CSV_XS::IV (),
               Text::CSV_XS::NV (),
               Text::CSV_XS::NV (),
               Text::CSV_XS::PV ()]);

Column types are used only for decoding columns, in other words
by the I<parse ()> and I<getline ()> methods.

You can unset column types by doing a

 $csv->types (undef);

or fetch the current type settings with

 $types = $csv->types ();

=over 4

=item IV

Set field type to integer.

=item NV

Set field type to numeric/float.

=item PV

Set field type to string.

=back

=head2 fields

 @columns = $csv->fields ();

This object function returns the input to C<combine ()> or the resultant
decomposed fields of C successfull <parse ()>, whichever was called more
recently.

Note that the return value is undefined after using C<getline ()>, which
does not fill the data structures returned by C<parse ()>.

=head2 meta_info

 @flags = $csv->meta_info ();

This object function returns the flags of the input to C<combine ()> or
the flags of the resultant decomposed fields of C<parse ()>, whichever
was called more recently.

For each field, a meta_info field will hold flags that tell something about
the field returned by the C<fields ()> method or passed to the C<combine ()>
method. The flags are bitwise-or'd like:

=over 4

=item 0x0001

The field was quoted.

=item 0x0002

The field was binary.

=back

See the C<is_*** ()> methods below.

=head2 is_quoted

  my $quoted = $csv->is_quoted ($column_idx);

Where C<$column_idx> is the (zero-based) index of the column in the
last result of C<parse ()>.

This returns a true value if the data in the indicated column was
enclosed in C<quote_char> quotes. This might be important for data
where C<,20070108,> is to be treated as a numeric value, and where
C<,"20070108",> is explicitly marked as character string data.

=head2 is_binary

  my $binary = $csv->is_binary ($column_idx);

Where C<$column_idx> is the (zero-based) index of the column in the
last result of C<parse ()>.

This returns a true value if the data in the indicated column
contained any byte in the range [\x00-\x08,\x10-\x1F,\x7F-\xFF]

=head2 status

 $status = $csv->status ();

This object function returns success (or failure) of C<combine ()> or
C<parse ()>, whichever was called more recently.

=head2 error_input

 $bad_argument = $csv->error_input ();

This object function returns the erroneous argument (if it exists) of
C<combine ()> or C<parse ()>, whichever was called more recently.

=head2 error_diag

 Text::CSV_XS->error_diag ();
 $csv->error_diag ();
 $error_code   = 0  + $csv->error_diag ();
 $error_str    = "" . $csv->error_diag ();
 ($cde, $str, $pos) = $csv->error_diag ();

If (and only if) an error occured, this function returns the diagnostics
of that error.

If called in void context, it will print the internal error code and the
associated error message to STDERR.

If called in list context, it will return the error code and the error
message in that order. If the last error was from parsing, the third
value returned is a best guess at the location within the line that was
being parsed. It's value is 1-based. See C<examples/csv-check> for how
this can be used.

If called in scalar context, it will return the diagnostics in a single
scalar, a-la $!. It will contain the error code in numeric context, and
the diagnostics message in string context.

When called as a class method or a direct function call, the error diag
is that of the last C<new ()> call.

=head2 SetDiag

 $csv->SetDiag (0);

Use to reset the diagnostics if you are dealing with errors.

=head1 INTERNALS

=over 4

=item Combine (...)

=item Parse (...)

=back

The arguments to these two internal functions are deliberately not
described or documented to enable the module author(s) to change it
when they feel the need for it and using them is highly discouraged
as the API may change in future releases.

=head1 EXAMPLES

An example for parsing CSV strings:

  my $csv = Text::CSV_XS->new ({ keep_meta_info => 1, binary => 1 });

  my $sample_input_string =
      qq{"I said, ""Hi!""",Yes,"",2.34,,"1.09","\x{20ac}",};
  if ($csv->parse ($sample_input_string)) {
      my @field = $csv->fields;
      foreach my $col (0 .. $#field) {
          my $quo = $csv->is_quoted ($col) ? $csv->{quote_char} : "";
          printf "%2d: %s%s%s\n", $col, $quo, $field[$col], $quo;
          }
      }
  else {
      my $err = $csv->error_input;
      print STDERR "parse () failed on argument: ", $err, "\n";
      $csv->error_diag ();
      }

An example for creating CSV files:

  my $csv = Text::CSV_XS->new;

  open my $csv_fh, ">", "hello.csv" or die "hello.csv: $!";

  my @sample_input_fields = (
      'You said, "Hello!"',   5.67,
      '"Surely"',   '',   '3.14159');
  if ($csv->combine (@sample_input_fields)) {
      my $string = $csv->string;
      print $csv_fh "$string\n";
      }
  else {
      my $err = $csv->error_input;
      print "combine () failed on argument: ", $err, "\n";
      }
  close $csv_fh or die "hello.csv: $!";

Or using the C<print ()> method, which is faster like in
dumping the content of a database ($dbh) table ($tbl) to CSV:

  my $csv = Text::CSV_XS->new ({ binary => 1, eol => $/ });
  open my $fh, ">", "$tbl.csv" or die "$tbl.csv: $!";
  my $sth = $dbh->prepare ("select * from $tbl");
  $sth->execute;
  $csv->print ($fh, $sth->{NAME_lc});
  while (my $row = $sth->fetch) {
      $csv->print ($fh, $row) or ...;
      }
  close $fh or die "$tbl.csv: $!";

Reading a CSV file line by line:

  my $csv = Text::CSV_XS->new ({ binary => 1 });
  open my $fh, "<", "file.csv" or die "file.csv: $!";
  while (my $row = $csv->getline ($fh)) {
      # do something with @$row
      }
  $csv->eof or $csv->error_diag;
  close $fh or die "file.csv: $!";

For more extended examples, see the C<examples/> subdirectory in the
original distribution. Included is C<examples/parser-xs.pl>, that could
be used to `fix' bad CSV and parse beyond errors.

  perl examples/parser-xs.pl bad.csv >good.csv

=head1 CAVEATS

C<Text::CSV_XS> is not designed to detect the characters used for field
separation and quoting. The parsing is done using predefined settings. In
the examples subdirectory, you can find scripts that demonstrate how you
can try to detect these characters yourself.

=head2 Microsoft Excel

The import/export from Microsoft Excel is a I<risky task>, according to the
documentation in C<Text::CSV::Separator>. Microsoft uses the system's default
list separator defined in the regional settings, which happens to be a
semicolon for Dutch, German and Spanish (and probably some others as well).
For the English locale, the default is a comma. In Windows however, the user
is free to choose a predefined locale, and then change every individual
setting in it, so checking the locale is no solution.

=head1 TODO

=over 2

=item More tests

For all possible errors, there should be a test.

All XS code should be covered in the test cases, except for perl
internal failure, like failing to store a hash value.

=item More Errors & Warnings

New extensions ought to be clear and concise in reporting what error
occurred where and why, and possibly also tell a remedy to the problem.
error_diag is a (very) good start, but there is more work to be done here.

Basic calls should croak or warn on illegal parameters. Errors should be
documented.

=item setting meta info

Future extensions might include extending the C<meta_info ()>,
C<is_quoted ()>, and C<is_binary ()> to accept setting these flags
for fields, so you can specify which fields are quoted in the
combine ()/string () combination.

  $csv->meta_info (0, 1, 1, 3, 0, 0);
  $csv->is_quoted (3, 1);

=item combined methods

Requests for adding means (methods) that combine C<combine ()> and
C<string ()> in a single call will B<not> be honored. Likewise for
C<parse ()> and C<fields ()>. Given the trouble with embedded newlines,
Using C<getline ()> and C<print ()> instead is the prefered way to go.

=item Unicode

We probably need many more tests to check if all edge-cases are covered.
See t/50_utf8.t.

=item Parse the whole file at once

Implement a new methods that enables the parsing of a complete file
at once, returning a lis of hashes. Possible extension to this could
be to enable a column selection on the call:

   my @AoH = $csv->parse_file ($filename, { cols => [ 1, 4..8, 12 ]});

Returning something like

   [ { fields => [ 1, 2, "foo", 4.5, undef, "", 8 ],
       flags  => [ ... ],
       errors => [ ... ],
       },
     { fields => [ ... ],
       .
       .
       },
     ]

=item EBCDIC

The hard-coding of characters and character ranges makes this module
unusable on EBCDIC system. Using some #ifdef structure could enable
these again without loosing speed. Testing would be the hard part.

=back

=head1 Release plan

No guarantees, but this is what I have in mind right now:

=over 2

=item next

 - This might very well be 1.00
 - DIAGNOSTICS setction in pod to *describe* the errors (see below)
 - croak / carp

=item next + 1

 - csv2csv - a script to regenerate a CSV file to follow standards
 - EBCDIC support

=back

=head1 DIAGNOSTICS

Still under construction ...

If an error occured, C<$csv->error_diag ()> can be used to get more information
on the cause of the failure. Note that for speed reasons, the internal value
is never cleared on success, so using the value returned by C<error_diag ()> in
normal cases - when no error occured - may cause unexpected results.

Currently errors as described below are available. I've tried to make the error
itself explanatory enough, but more descriptions will be added. For most of
these errors, the first three capitals describe the error category:

=over 2

=item INI

Initialization error or option conflict.

=item ECR

Carriage-Return related parse error.

=item EOF

End-Of-File related parse error.

=item EIQ

Parse error inside quotation.

=item EIF

Parse error inside field.

=item ECB

Combine error.

=item EHR

HashRef parse related error.

=back

=over 2

=item 1001 "INI - sep_char is equal to quote_char or escape_char"

The separation character cannot be equal to either the quotation character
or the escape character, as that will invalidate all parsing rules.

=item 2010 "ECR - QUO char inside quotes followed by CR not part of EOL"

When C<eol> has been set to something specific, other than the default,
like C<"\r\t\n">, and the C<"\r"> is following the B<second> (closing)
C<quote_char>, where the characters following the C<"\r"> do not make up
the C<eol> sequence, this is an error.

=item 2011 "ECR - Characters after end of quoted field"

Sequences like C<1,foo,"bar"baz,2> are not allowed. C<"bar"> is a quoted
field, and after the closing quote, there should be either a new-line
sequence or a separation character.

=item 2012 "EOF - End of data in parsing input stream"

Self-explaining. End-of-file while inside parsing a stream. Can only
happen when reading from streams with C<getline ()>, as using C<parse ()>
is done on strings that are not required to have a trailing C<eol>.

=item 2021 "EIQ - NL char inside quotes, binary off"

Sequences like C<1,"foo\nbar",2> are only allowed when the binary option
has been selected with the constructor.

=item 2022 "EIQ - CR char inside quotes, binary off"

Sequences like C<1,"foo\rbar",2> are only allowed when the binary option
has been selected with the constructor.

=item 2023 "EIQ - QUO character not allowed

Sequences like C<"foo "bar" baz",quux> and C<2023,",2008-04-05,"Foo, Bar",\n>
will cause this error.

=item 2024 "EIQ - EOF cannot be escaped, not even inside quotes"

The escape character is not allowed as last character in an input stream.

=item 2025 "EIQ - Loose unescaped escape"

An escape character should escape only characters that need escaping. Allowing
the escape for other characters is possible with the C<allow_loose_escape>
attribute.

=item 2026 "EIQ - Binary character inside quoted field, binary off"

Binary characters are not allowed by default. Exceptions are fields that
contain valid UTF-8, that will automatically be upgraded is the content is
valid UTF-8. Pass the C<binary> attribute with a true value to accept binary
characters.

=item 2027 "EIQ - Quoted field not terminated"

When parsing a field that started with a quotation character, the field is
expected to be closed with a quotation character. When the parsed line is
exhausted before the quote is found, that field is not terminated.

=item 2030 "EIF - NL char inside unquoted verbatim, binary off"

=item 2031 "EIF - CR char is first char of field, not part of EOL"

=item 2032 "EIF - CR char inside unquoted, not part of EOL"

=item 2034 "EIF - Loose unescaped quote"

=item 2035 "EIF - Escaped EOF in unquoted field"

=item 2036 "EIF - ESC error"

=item 2037 "EIF - Binary character in unquoted field, binary off"

=item 2110 "ECB - Binary character in Combine, binary off"

=item 2200 "EIO - print to IO failed. See errno"

=item 3001 "EHR - Unsupported syntax for column_names ()"

=item 3002 "EHR - getline_hr () called before column_names ()"

=item 3003 "EHR - bind_columns () and column_names () fields count mismatch"

=item 3004 "EHR - bind_columns () only accepts refs to scalars"

=item 3006 "EHR - bind_columns () did not pass enough refs for parsed fields"

=item 3007 "EHR - bind_columns needs refs to writeable scalars"

=item 3008 "EHR - unexpected error in bound fields

=back

=head1 SEE ALSO

L<perl(1)>, L<IO::File(3)>, L<IO::Handle(3)>, L<IO::Wrap(3)>,
L<Text::CSV(3)>, L<Text::CSV_PP(3)>, L<Text::CSV::Encoded>,
L<Text::CSV::Separator(3)>, and L<Spreadsheet::Read(3)>.

=head1 AUTHORS and MAINTAINERS

Alan Citterman F<E<lt>alan@mfgrtl.comE<gt>> wrote the original Perl
module. Please don't send mail concerning Text::CSV_XS to Alan, as
he's not involved in the C part which is now the main part of the
module.

Jochen Wiedmann F<E<lt>joe@ispsoft.deE<gt>> rewrote the encoding and
decoding in C by implementing a simple finite-state machine and added
the variable quote, escape and separator characters, the binary mode
and the print and getline methods. See ChangeLog releases 0.10 through
0.23.

H.Merijn Brand F<E<lt>h.m.brand@xs4all.nlE<gt>> cleaned up the code,
added the field flags methods, wrote the major part of the test suite,
completed the documentation, fixed some RT bugs. See ChangeLog releases
0.25 and on.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007-2009 H.Merijn Brand for PROCURA B.V.
Copyright (C) 1998-2001 Jochen Wiedmann. All rights reserved.
Portions Copyright (C) 1997 Alan Citterman. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
