# !!! This module has been changed from the CPAN version of ver2.51 - 
#
# The, "use YAML::Syck;" line has been commented out, so as not to be needed to be loaded. 
#
# !!!

package HTML::TextToHTML;
use 5.8.1;
use strict;
#------------------------------------------------------------------------

=head1 NAME

HTML::TextToHTML - convert plain text file to HTML.

=head1 VERSION

This describes version B<2.51> of HTML::TextToHTML.

=cut

our $VERSION = '2.51';

=head1 SYNOPSIS

  From the command line:

    txt2html I<arguments>

  From Scripts:

    use HTML::TextToHTML;
 
    # create a new object
    my $conv = new HTML::TextToHTML();

    # convert a file
    $conv->txt2html(infile=>[$text_file],
                     outfile=>$html_file,
		     title=>"Wonderful Things",
		     mail=>1,
      ]);

    # reset arguments
    $conv->args(infile=>[], mail=>0);

    # convert a string
    $newstring = $conv->process_chunk($mystring)

=head1 DESCRIPTION

HTML::TextToHTML converts plain text files to HTML. The txt2html script
uses this module to do the same from the command-line.

It supports headings, tables, lists, simple character markup, and
hyperlinking, and is highly customizable. It recognizes some of the
apparent structure of the source document (mostly whitespace and
typographic layout), and attempts to mark that structure explicitly
using HTML. The purpose for this tool is to provide an easier way of
converting existing text documents to HTML format, giving something nicer
than just whapping the text into a big PRE block.

=head2 History

The original txt2html script was written by Seth Golub (see
http://www.aigeek.com/txt2html/), and converted to a perl module by
Kathryn Andersen (see http://www.katspace.com/tools/text_to_html/) and
made into a sourceforge project by Sun Tong (see
http://sourceforge.net/projects/txt2html/).  Earlier versions of the
HTML::TextToHTML module called the included script texthyper so as not
to clash with the original txt2html script, but now the projects have
all been merged.

=head1 OPTIONS

All arguments can be set when the object is created, and further options
can be set when calling the actual txt2html method. Arguments
to methods can take a hash of arguments.

Note that all option-names must match exactly -- no abbreviations are
allowed.  The argument-keys are expected to have values matching those
required for that argument -- whether that be a boolean, a string, a
reference to an array or a reference to a hash.  These will replace any
value for that argument that might have been there before.

=over

=item append_file

    append_file=>I<filename>

If you want something appended by default, put the filename here.
The appended text will not be processed at all, so make sure it's
plain text or correct HTML.  i.e. do not have things like:
    Mary Andersen E<lt>kitty@example.comE<gt>
but instead, have:
    Mary Andersen &lt;kitty@example.com&gt;

(default: nothing)

=item append_head

    append_head=>I<filename>

If you want something appended to the head by default, put the filename here.
The appended text will not be processed at all, so make sure it's
plain text or correct HTML.  i.e. do not have things like:
    Mary Andersen E<lt>kitty@example.comE<gt>
but instead, have:
    Mary Andersen &lt;kitty@example.com&gt;

(default: nothing)

=item body_deco

    body_deco=>I<string>

Body decoration string: a string to be added to the BODY tag so that
one can set attributes to the BODY (such as class, style, bgcolor etc)
For example, "class='withimage'".

=item bold_delimiter

    bold_delimiter=>I<string>

This defines what character (or string) is taken to be the delimiter of
text which is to be interpreted as bold (that is, to be given a STRONG
tag).  If this is empty, then no bolding of text will be done.
(default: #)

=item bullets

    bullets=>I<string>

This defines what single characters are taken to be "bullet" characters
for unordered lists.  Note that because this is used as a character
class, if you use '-' it must come first.
(default:-=o*\267)

=item bullets_ordered

    bullets_ordered=>I<string>

This defines what single characters are taken to be "bullet" placeholder
characters for ordered lists.  Ordered lists are normally marked by
a number or letter followed by '.' or ')' or ']' or ':'.  If an ordered
bullet is used, then it simply indicates that this is an ordered list,
without giving explicit numbers.

Note that because this is used as a character class, if you use '-' it
must come first.
(default:nothing)

=item caps_tag

    caps_tag=>I<tag>

Tag to put around all-caps lines
(default: STRONG)
If an empty tag is given, then no tag will be put around all-caps lines.

=item custom_heading_regexp

    custom_heading_regexp=>\@custom_headings

Add patterns for headings.  Header levels are assigned by regexp in the
order seen in the input text. When a line matches a custom header
regexp, it is tagged as a header.  If it's the first time that
particular regexp has matched, the next available header level is
associated with it and applied to the line.  Any later matches of that
regexp will use the same header level.  Therefore, if you want to match
numbered header lines, you could use something like this:

    my @custom_headings = ('^ *\d+\. \w+',
			   '^ *\d+\.\d+\. \w+',
			   '^ *\d+\.\d+\.\d+\. \w+');

    ...
	custom_heading_regexp=>\@custom_headings,
    ...

Then lines like

                " 1. Examples "
                " 1.1. Things"
            and " 4.2.5. Cold Fusion"

Would be marked as H1, H2, and H3 (assuming they were found in that
order, and that no other header styles were encountered).
If you prefer that the first one specified always be H1, the second
always be H2, the third H3, etc, then use the "explicit_headings"
option.

This expects a reference to an array of strings.

(default: none)

=item default_link_dict

    default_link_dict=>I<filename>

The name of the default "user" link dictionary.
(default: "$ENV{'HOME'}/.txt2html.dict" -- this is the same as for
the txt2html script.  If there is no $ENV{HOME} then it is just '.txt2html.dict')

=item demoronize

    demoronize=>1

Convert Microsoft-generated character codes that are non-ISO codes into
something more reasonable.
(default:true)

=item doctype

    doctype=>I<doctype>

This gets put in the DOCTYPE field at the top of the document, unless it's
empty.

Default :
'-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd'

If B<xhtml> is true, the contents of this is ignored, unless it's
empty, in which case no DOCTYPE declaration is output.

=item eight_bit_clean

    eight_bit_clean=>1

If false, convert Latin-1 characters to HTML entities.
If true, this conversion is disabled; also "demoronize" is set to
false, since this also changes 8-bit characters.
(default: false)

=item escape_HTML_chars

    escape_HTML_chars=>1

turn & E<lt> E<gt> into &amp; &gt; &lt;
(default: true)

=item explicit_headings

    explicit_headings=>1

Don't try to find any headings except the ones specified in the
--custom_heading_regexp option.
Also, the custom headings will not be assigned levels in the order they
are encountered in the document, but in the order they are specified on
the custom_heading_regexp option.
(default: false)

=item extract

    extract=>1

Extract Mode; don't put HTML headers or footers on the result, just
the plain HTML (thus making the result suitable for inserting into
another document (or as part of the output of a CGI script).
(default: false)

=item hrule_min

    hrule_min=>I<n>

Min number of ---s for an HRule.
(default: 4)

=item indent_width

    indent_width=>I<n>

Indents this many spaces for each level of a list.
(default: 2)

=item indent_par_break

    indent_par_break=>1

Treat paragraphs marked solely by indents as breaks with indents.
That is, instead of taking a three-space indent as a new paragraph,
put in a <BR> and three non-breaking spaces instead.
(see also --preserve_indent)
(default: false)

=item infile

    infile=>\@my_files
    infile=>['chapter1.txt', 'chapter2.txt']

The name of the input file(s).  
This expects a reference to an array of filenames.

The special filename '-' designates STDIN.

See also L</inhandle> and L</instring>.

(default:-)

=item inhandle

    inhandle=>\@my_handles
    inhandle=>[\*MYINHANDLE, \*STDIN]

An array of input filehandles; use this instead of
L</infile> or L</instring> to use a filehandle or filehandles
as input.

=item instring

    instring=>\@my_strings
    instring=>[$string1, $string2]

An array of input strings; use this instead of
L</infile> or L</inhandle> to use a string or strings
as input.

=item italic_delimiter

    italic_delimiter=>I<string>

This defines what character (or string) is taken to be the delimiter of
text which is to be interpreted as italic (that is, to be given a EM
tag).  If this is empty, no italicising of text will be done.
(default: *)

=item underline_delimiter

    underline_delimiter=>I<string>

This defines what character (or string) is taken to be the delimiter of
text which is to be interpreted as underlined (that is, to be given a U
tag).  If this is empty, no underlining of text will be done.
(default: _)

=item links_dictionaries

    links_dictionaries=>\@my_link_dicts
    links_dictionaries=>['url_links.dict', 'format_links.dict']

File(s) to use as a link-dictionary.  There can be more than one of
these.  These are in addition to the Global Link Dictionary and the User
Link Dictionary.  This expects a reference to an array of filenames.

=item link_only

    link_only=>1

Do no escaping or marking up at all, except for processing the links
dictionary file and applying it.  This is useful if you want to use
the linking feature on an HTML document.  If the HTML is a
complete document (includes HTML,HEAD,BODY tags, etc) then you'll
probably want to use the --extract option also.
(default: false)

=item lower_case_tags

     lower_case_tags=>1

Force all tags to be in lower-case.

=item mailmode

    mailmode=>1

Deal with mail headers & quoted text.  The mail header paragraph is
given the class 'mail_header', and mail-quoted text is given the class
'quote_mail'.
(default: false)

=item make_anchors

    make_anchors=>0

Should we try to make anchors in headings?
(default: true)

=item make_links

    make_links=>0

Should we try to build links?  If this is false, then the links
dictionaries are not consulted and only structural text-to-HTML
conversion is done.  (default: true)

=item make_tables

    make_tables=>1

Should we try to build tables?  If true, spots tables and marks them up
appropriately.  See L</Input File Format> for information on how tables
should be formatted.

This overrides the detection of lists; if something looks like a table,
it is taken as a table, and list-checking is not done for that
paragraph.

(default: false)

=item min_caps_length

    min_caps_length=>I<n>

min sequential CAPS for an all-caps line
(default: 3)

=item outfile

    outfile=>I<filename>

The name of the output file.  If it is "-" then the output goes
to Standard Output.
(default: - )

=item outhandle

The output filehandle; if this is given then the output goes
to this filehandle instead of to the file given in L</outfile>.

=item par_indent

    par_indent=>I<n>

Minumum number of spaces indented in first lines of paragraphs.
  Only used when there's no blank line
preceding the new paragraph.
(default: 2)

=item preformat_trigger_lines

    preformat_trigger_lines=>I<n>

How many lines of preformatted-looking text are needed to switch to <PRE>
          <= 0 : Preformat entire document
             1 : one line triggers
          >= 2 : two lines trigger

(default: 2)

=item endpreformat_trigger_lines

    endpreformat_trigger_lines=>I<n>

How many lines of unpreformatted-looking text are needed to switch from <PRE>
           <= 0 : Never preformat within document
              1 : one line triggers
           >= 2 : two lines trigger
(default: 2)

NOTE for preformat_trigger_lines and endpreformat_trigger_lines:
A zero takes precedence.  If one is zero, the other is ignored.
If both are zero, entire document is preformatted.

=item preformat_start_marker

    preformat_start_marker=>I<regexp>

What flags the start of a preformatted section if --use_preformat_marker
is true.

(default: "^(:?(:?&lt;)|<)PRE(:?(:?&gt;)|>)\$")

=item preformat_end_marker

    preformat_end_marker=>I<regexp>

What flags the end of a preformatted section if --use_preformat_marker
is true.

(default: "^(:?(:?&lt;)|<)/PRE(:?(:?&gt;)|>)\$")

=item preformat_whitespace_min

    preformat_whitespace_min=>I<n>

Minimum number of consecutive whitespace characters to trigger
normal preformatting. 
NOTE: Tabs are expanded to spaces before this check is made.
That means if B<tab_width> is 8 and this is 5, then one tab may be
expanded to 8 spaces, which is enough to trigger preformatting.
(default: 5)

=item prepend_file

    prepend_file=>I<filename>

If you want something prepended to the processed body text, put the
filename here.  The prepended text will not be processed at all, so make
sure it's plain text or correct HTML.

(default: nothing)

=item preserve_indent

    preserve_indent=>1

Preserve the first-line indentation of paragraphs marked with indents
by replacing the spaces of the first line with non-breaking spaces.
(default: false)

=item short_line_length

    short_line_length=>I<n>

Lines this short (or shorter) must be intentionally broken and are kept
that short.
(default: 40)

=item style_url

    style_url=>I<url>

This gives the URL of a stylesheet; a LINK tag will be added to the
output.

=item tab_width

    tab_width=>I<n>

How many spaces equal a tab?
(default: 8)

=item table_type
    
    table_type=>{ ALIGN=>0, PGSQL=>0, BORDER=>1, DELIM=>0 }

This determines which types of tables will be recognised when "make_tables"
is true.  The possible types are ALIGN, PGSQL, BORDER and DELIM.
(default: all types are true)

=item title

    title=>I<title>

You can specify a title.  Otherwise it will use a blank one.
(default: nothing)

=item titlefirst

    titlefirst=>1

Use the first non-blank line as the title. (See also "title")

=item underline_length_tolerance

    underline_length_tolerance=>I<n>

How much longer or shorter can underlines be and still be underlines?
(default: 1)

=item underline_offset_tolerance

    underline_offset_tolerance=>I<n>

How far offset can underlines be and still be underlines?
(default: 1)

=item unhyphenation

    unhyphenation=>0

Enables unhyphenation of text.
(default: true)

=item use_mosaic_header

    use_mosaic_header=>1

Use this option if you want to force the heading styles to match what Mosaic
outputs.  (Underlined with "***"s is H1,
with "==="s is H2, with "+++" is H3, with "---" is H4, with "~~~" is H5
and with "..." is H6)
This was the behavior of txt2html up to version 1.10.
(default: false)

=item use_preformat_marker

    use_preformat_marker=>1

Turn on preformatting when encountering "<PRE>" on a line by itself, and turn
it off when there's a line containing only "</PRE>".
When such preformatted text is detected, the PRE tag will be given the
class 'quote_explicit'.
(default: off)

=item xhtml

    xhtml=>1

Try to make the output conform to the XHTML standard, including
closing all open tags and marking empty tags correctly.  This
turns on --lower_case_tags and overrides the --doctype option.
Note that if you add a header or a footer file, it is up to you
to make it conform; the header/footer isn't touched by this.
Likewise, if you make link-dictionary entries that break XHTML,
then this won't fix them, except to the degree of putting all tags
into lower-case.

(default: true)

=back

=head1 DEBUGGING

There are global variables for setting types and levels
of debugging.  These should only be used by developers.

=over

=item $HTML::TextToHTML::Debug

$HTML::TextToHTML::Debug = 1;
    
Enable copious debugging output.
(default: false)

=item $HTML::TextToHTML::DictDebug

    $HTML::TextToHTML::DictDebug = I<n>;

Debug mode for link dictionaries. Bitwise-Or what you want to see:

          1: The parsing of the dictionary
          2: The code that will make the links
          4: When each rule matches something
          8: When each tag is created

(default: 0)

=back

=cut

our $Debug = 0;
our $DictDebug = 0;

=head1 METHODS

=cut

#------------------------------------------------------------------------
# use YAML::Syck;

our $PROG = 'HTML::TextToHTML';

#------------------------------------------------------------------------

########################################
# Definitions  (Don't change these)
#

# These are just constants I use for making bit vectors to keep track
# of what modes I'm in and what actions I've taken on the current and
# previous lines.

our $NONE         = 0;
our $LIST         = 1;
our $HRULE        = 2;
our $PAR          = 4;
our $PRE          = 8;
our $END          = 16;
our $BREAK        = 32;
our $HEADER       = 64;
our $MAILHEADER   = 128;
our $MAILQUOTE    = 256;
our $CAPS         = 512;
our $LINK         = 1024;
our $PRE_EXPLICIT = 2048;
our $TABLE        = 4096;
our $IND_BREAK    = 8192;
our $LIST_START   = 16384;
our $LIST_ITEM    = 32768;

# Constants for Link-processing
# bit-vectors for what to do with a particular link-dictionary entry
our $LINK_NOCASE    = 1;
our $LINK_EVAL      = 2;
our $LINK_HTML      = 4;
our $LINK_ONCE      = 8;
our $LINK_SECT_ONCE = 16;

# Constants for Ordered Lists and Unordered Lists.
# And Definition Lists.
# I use this in the list stack to keep track of what's what.

our $OL = 1;
our $UL = 2;
our $DL = 3;

# Constants for table types
our $TAB_ALIGN  = 1;
our $TAB_PGSQL  = 2;
our $TAB_BORDER = 3;
our $TAB_DELIM  = 4;

# Constants for tags
use constant {
    TAG_START	=> 1,
    TAG_END	=> 2,
    TAG_EMPTY	=> 3,
};

# Character entity names
# characters to replace with entities
our %char_entities = (
    "\241", "&iexcl;",  "\242", "&cent;",   "\243", "&pound;",
    "\244", "&curren;", "\245", "&yen;",    "\246", "&brvbar;",
    "\247", "&sect;",   "\250", "&uml;",    "\251", "&copy;",
    "\252", "&ordf;",   "\253", "&laquo;",  "\254", "&not;",
    "\255", "&shy;",    "\256", "&reg;",    "\257", "&hibar;",
    "\260", "&deg;",    "\261", "&plusmn;", "\262", "&sup2;",
    "\263", "&sup3;",   "\264", "&acute;",  "\265", "&micro;",
    "\266", "&para;",   "\270", "&cedil;",  "\271", "&sup1;",
    "\272", "&ordm;",   "\273", "&raquo;",  "\274", "&frac14;",
    "\275", "&frac12;", "\276", "&frac34;", "\277", "&iquest;",
    "\300", "&Agrave;", "\301", "&Aacute;", "\302", "&Acirc;",
    "\303", "&Atilde;", "\304", "&Auml;",   "\305", "&Aring;",
    "\306", "&AElig;",  "\307", "&Ccedil;", "\310", "&Egrave;",
    "\311", "&Eacute;", "\312", "&Ecirc;",  "\313", "&Euml;",
    "\314", "&Igrave;", "\315", "&Iacute;", "\316", "&Icirc;",
    "\317", "&Iuml;",   "\320", "&ETH;",    "\321", "&Ntilde;",
    "\322", "&Ograve;", "\323", "&Oacute;", "\324", "&Ocirc;",
    "\325", "&Otilde;", "\326", "&Ouml;",   "\327", "&times;",
    "\330", "&Oslash;", "\331", "&Ugrave;", "\332", "&Uacute;",
    "\333", "&Ucirc;",  "\334", "&Uuml;",   "\335", "&Yacute;",
    "\336", "&THORN;",  "\337", "&szlig;",  "\340", "&agrave;",
    "\341", "&aacute;", "\342", "&acirc;",  "\343", "&atilde;",
    "\344", "&auml;",   "\345", "&aring;",  "\346", "&aelig;",
    "\347", "&ccedil;", "\350", "&egrave;", "\351", "&eacute;",
    "\352", "&ecirc;",  "\353", "&euml;",   "\354", "&igrave;",
    "\355", "&iacute;", "\356", "&icirc;",  "\357", "&iuml;",
    "\360", "&eth;",    "\361", "&ntilde;", "\362", "&ograve;",
    "\363", "&oacute;", "\364", "&ocirc;",  "\365", "&otilde;",
    "\366", "&ouml;",   "\367", "&divide;", "\370", "&oslash;",
    "\371", "&ugrave;", "\372", "&uacute;", "\373", "&ucirc;",
    "\374", "&uuml;",   "\375", "&yacute;", "\376", "&thorn;",
    "\377", "&yuml;",   "\267", "&middot;",
);

# alignments for tables
our @alignments    = ('', '', ' ALIGN="RIGHT"', ' ALIGN="CENTER"');
our @lc_alignments = ('', '', ' align="right"', ' align="center"');
our @xhtml_alignments =
  ('', '', ' style="text-align: right;"', ' style="text-align: center;"');

#---------------------------------------------------------------#
# Object interface
#---------------------------------------------------------------#

=head2 new

    $conv = new HTML::TextToHTML()

    $conv = new HTML::TextToHTML(titlefirst=>1,
	...
    );

Create a new object with new. If arguments are given, these arguments
will be used in invocations of other methods.

See L</OPTIONS> for the possible values of the arguments.

=cut

sub new
{
    my $invocant = shift;
    my $self     = {};

    my $class = ref($invocant) || $invocant;    # Object or class name
    init_our_data($self);

    # bless self
    bless($self, $class);

    $self->args(@_);

    return $self;
}    # new

=head2 args

    $conv->args(short_line_length=>60,
	titlefirst=>1,
	....
    );

Updates the current arguments/options of the HTML::TextToHTML object.
Takes hash of arguments, which will be used in invocations of other
methods.
See L</OPTIONS> for the possible values of the arguments.

=cut

sub args
{
    my $self      = shift;
    my %args = @_;

    if (%args)
    {
        if ($Debug)
        {
            print STDERR "========args(hash)========\n";
            print STDERR Dump(%args);
        }
	my $arg;
	my $val;
	while (($arg, $val) = each %args)
        {
            if (defined $val)
            {
                if ($arg =~ /^-/)
                {
                    $arg =~ s/^-//;    # get rid of first dash
                    $arg =~ s/^-//;    # get rid of possible second dash
                }
                if ($Debug)
                {
                    print STDERR "--", $arg;
                }
                $self->{$arg} = $val;
                if ($Debug)
                {
                    print STDERR " ", $val, "\n";
                }
            }
        }
    }
    $self->deal_with_options();
    if ($Debug)
    {
        print STDERR Dump($self);
    }

    return 1;
}    # args

=head2 process_chunk

$newstring = $conv->process_chunk($mystring);

Convert a string to a HTML fragment.  This assumes that this string is
at the least, a single paragraph, but it can contain more than that.
This returns the processed string.  If you want to pass arguments to
alter the behaviour of this conversion, you need to do that earlier,
either when you create the object, or with the L</args> method.

    $newstring = $conv->process_chunk($mystring,
			    close_tags=>0);

If there are open tags (such as lists) in the input string,
process_chunk will automatically close them, unless you specify not
to, with the close_tags option.

    $newstring = $conv->process_chunk($mystring,
			    is_fragment=>1);

If you want this string to be treated as a fragment, and not assumed to
be a paragraph, set is_fragment to true.  If there is more than one
paragraph in the string (ie it contains blank lines) then this option
will be ignored.

=cut

sub process_chunk ($$;%)
{
    my $self  = shift;
    my $chunk = shift;
    my %args  = (
        close_tags  => 1,
        is_fragment => 0,
        @_
    );

    my $ret_str = '';
    my @paras   = split(/\r?\n\r?\n/, $chunk);
    my $ind     = 0;
    if (@paras == 1)    # just one paragraph
    {
        $ret_str .= $self->process_para(
            $chunk,
            close_tags  => $args{close_tags},
            is_fragment => $args{is_fragment}
        );
    }
    else
    {
        my $ind = 0;
        foreach my $para (@paras)
        {
            # if the paragraph doesn't end with a newline, add one
            $para .= "\n" if ($para !~ /\n$/);
            if ($ind == @paras - 1)    # last one
            {
                $ret_str .= $self->process_para(
                    $para,
                    close_tags  => $args{close_tags},
                    is_fragment => 0
                );
            }
            else
            {
                $ret_str .= $self->process_para(
                    $para,
                    close_tags  => 0,
                    is_fragment => 0
                );
            }
            $ind++;
        }
    }
    $ret_str;
}    # process_chunk

=head2 process_para

$newstring = $conv->process_para($mystring);

Convert a string to a HTML fragment.  This assumes that this string is
at the most a single paragraph, with no blank lines in it.  If you don't
know whether your string will contain blank lines or not, use the
L</process_chunk> method instead.

This returns the processed string.  If you want to pass arguments to
alter the behaviour of this conversion, you need to do that earlier,
either when you create the object, or with the L</args> method.

    $newstring = $conv->process_para($mystring,
			    close_tags=>0);

If there are open tags (such as lists) in the input string, process_para
will automatically close them, unless you specify not to, with the
close_tags option.

    $newstring = $conv->process_para($mystring,
			    is_fragment=>1);

If you want this string to be treated as a fragment, and not assumed to be
a paragraph, set is_fragment to true.

=cut

sub process_para ($$;%)
{
    my $self = shift;
    my $para = shift;
    my %args = (
        close_tags  => 1,
        is_fragment => 0,
        @_
    );

    # if this is an external call, do certain initializations
    $self->do_init_call();

    my $para_action = $NONE;

    # tables and mailheaders don't carry over from one para to the next
    if ($self->{__mode} & $TABLE)
    {
        $self->{__mode} ^= $TABLE;
    }
    if ($self->{__mode} & $MAILHEADER)
    {
        $self->{__mode} ^= $MAILHEADER;
    }

    # convert Microsoft character codes into sensible characters
    if ($self->{demoronize})
    {
        demoronize_char($para);
    }

    # if we are not just linking, we are discerning structure
    if (!$self->{link_only})
    {

        # Chop trailing whitespace and DOS CRs
        $para =~ s/[ \011]*\015$//;
        # Chop leading whitespace and DOS CRs
        $para =~ s/^[ \011]*\015//;
        $para =~ s/\r//g;             # remove any stray carriage returns

        my @done_lines = ();          # lines which have been processed

        # The PRE_EXPLICIT structure can carry over from one
        # paragraph to the next, but it is ended with the
        # explicit end-tag designated for it.
        # Therefore we can shortcut for this by checking
        # for the end of the PRE_EXPLICIT and chomping off
        # the preformatted string part of this para before
        # we have to split it into lines.
        # Note that after this check, we could *still* be
        # in PRE_EXPLICIT mode.
        if ($self->{__mode} & $PRE_EXPLICIT)
        {
            my $pre_str =
              $self->split_end_explicit_preformat(para_ref => \$para);
            if ($pre_str)
            {
                push @done_lines, $pre_str;
            }
        }

        if (defined $para && $para ne "")
        {
            #
            # Now we split the paragraph into lines
            #
            my $para_len         = length($para);
            my @para_lines       = split(/^/, $para);
            my @para_line_len    = ();
            my @para_line_indent = ();
            my @para_line_action = ();
            my $i                = 0;
            foreach my $line (@para_lines)
            {
                # Change all tabs to spaces
                while ($line =~ /\011/)
                {
                    my $tw = $self->{tab_width};
                    $line =~ s/\011/" " x ($tw - (length($`) % $tw))/e;
                }
                push @para_line_len, length($line);
                if ($line =~ /^\s*$/)
                {
                    # if the line is blank, use the previous indent
                    # if there is one
                    push @para_line_indent,
                      ($i == 0 ? 0 : $para_line_indent[$i - 1]);
                }
                else
                {
                    # count the number of leading spaces
                    my ($ws) = $line =~ /^( *)[^ ]/;
                    push @para_line_indent, length($ws);
                }
                push @para_line_action, $NONE;
                $i++;
            }

            # There are two more structures which carry over from one
            # paragraph to the next: LIST, PRE
            # There are also certain things which will immediately end
            # multi-paragraph LIST and PRE, if found at the start
            # of a paragraph:
            # A list will be ended by
            # TABLE, MAILHEADER, HEADER, custom-header
            # A PRE will be ended by
            # TABLE, MAILHEADER and non-pre text

            my $is_table         = 0;
            my $table_type       = 0;
            my $is_mailheader    = 0;
            my $is_header        = 0;
            my $is_custom_header = 0;
            if (@{$self->{custom_heading_regexp}})
            {
                $is_custom_header =
                  $self->is_custom_heading(line => $para_lines[0]);
            }
            if (   $self->{make_tables}
                && @para_lines > 1)
            {
                $table_type = $self->get_table_type(
                    rows_ref => \@para_lines,
                    para_len => $para_len
                );
                $is_table = ($table_type != 0);
            }
            if (   !$self->{explicit_headings}
                && @para_lines > 1
                && !$is_table)
            {
                $is_header = $self->is_heading(
                    line_ref => \$para_lines[0],
                    next_ref => \$para_lines[1]
                );
            }
            # Note that it is concievable that someone has
            # partially disabled mailmode by making a custom header
            # which matches the start of mail.
            # This is stupid, but allowable, so we check.
            if (   $self->{mailmode}
                && !$is_table
                && !$is_custom_header)
            {
                $is_mailheader = $self->is_mailheader(rows_ref => \@para_lines);
            }

            # end the list if we can end it
            if (
                ($self->{__mode} & $LIST)
                && (   $is_table
                    || $is_mailheader
                    || $is_header
                    || $is_custom_header)
              )
            {
                my $list_end = '';
                my $action   = 0;
                $self->endlist(
                    num_lists       => $self->{__listnum},
                    prev_ref        => \$list_end,
                    line_action_ref => \$action
                );
                push @done_lines, $list_end;
                $self->{__prev_para_action} |= $END;
            }

            # end the PRE if we can end it
            if (
                   ($self->{__mode} & $PRE)
                && !($self->{__mode} & $PRE_EXPLICIT)
                && (   $is_table
                    || $is_mailheader
                    || !$self->is_preformatted($para_lines[0]))
                && ($self->{preformat_trigger_lines} != 0)
              )
            {
                my $pre_end = '';
                my $tag     = $self->close_tag('pre');
                $pre_end = "${tag}\n";
                $self->{__mode} ^= ($PRE & $self->{__mode});
                push @done_lines, $pre_end;
                $self->{__prev_para_action} |= $END;
            }

            # The PRE and PRE_EXPLICIT structure can carry over
            # from one paragraph to the next, but because we don't
            # want trailing newlines, such newlines would have been
            # gotten rid of in the previous call.  However, with
            # a preformatted text, we do want the blank lines in it
            # to be preserved, so let's add a blank line in here.
            if ($self->{__mode} & $PRE)
            {
                push @done_lines, "\n";
            }

            # Now, we do certain things which are only found at the
            # start of a paragraph:
            # HEADER, custom-header, TABLE and MAILHEADER
            # These could concievably eat the rest of the paragraph.

            if ($is_custom_header)
            {
                # custom header eats the first line
                my $header = shift @para_lines;
                shift @para_line_len;
                shift @para_line_indent;
                shift @para_line_action;
                $self->custom_heading(line_ref => \$header);
                push @done_lines, $header;
                $self->{__prev_para_action} |= $HEADER;
            }
            elsif ($is_header)
            {
                # normal header eats the first two lines
                my $header = shift @para_lines;
                shift @para_line_len;
                shift @para_line_indent;
                shift @para_line_action;
                my $underline = shift @para_lines;
                shift @para_line_len;
                shift @para_line_indent;
                shift @para_line_action;
                $self->heading(
                    line_ref => \$header,
                    next_ref => \$underline
                );
                push @done_lines, $header;
                $self->{__prev_para_action} |= $HEADER;
            }

            # do the table stuff on the array of lines
            if ($self->{make_tables} && $is_table)
            {
                if (
                    $self->tablestuff(
                        table_type => $table_type,
                        rows_ref   => \@para_lines,
                        para_len   => $para_len
                    )
                  )
                {
                    # this has used up all the lines
                    push @done_lines, @para_lines;
                    @para_lines = ();
                }
            }

            # check of this para is a mail-header
            if (   $is_mailheader
                && !($self->{__mode} & $TABLE)
                && @para_lines)
            {
                $self->mailheader(rows_ref => \@para_lines);
                # this has used up all the lines
                push @done_lines, @para_lines;
                @para_lines = ();
            }

            #
            # Now go through the paragraph lines one at a time
            # Note that we won't have TABLE, MAILHEADER, HEADER modes
            # because they would have eaten the lines
            #
            my $prev        = '';
            my $prev_action = $self->{__prev_para_action};
            for (my $i = 0; $i < @para_lines; $i++)
            {
                my $prev_ref;
                my $prev_action_ref;
                my $prev_line_indent;
                my $prev_line_len;
                if ($i == 0)
                {
                    $prev_ref         = \$prev;
                    $prev_action_ref  = \$prev_action;
                    $prev_line_indent = 0;
                    $prev_line_len    = 0;
                }
                else
                {
                    $prev_ref         = \$para_lines[$i - 1];
                    $prev_action_ref  = \$para_line_action[$i - 1];
                    $prev_line_indent = $para_line_indent[$i - 1];
                    $prev_line_len    = $para_line_len[$i - 1];
                }
                my $next_ref;
                if ($i == $#para_lines)
                {
                    $next_ref = undef;
                }
                else
                {
                    $next_ref = \$para_lines[$i + 1];
                }

                $para_lines[$i] = escape($para_lines[$i])
                  if ($self->{escape_HTML_chars});

                if ($self->{mailmode}
                    && !($self->{__mode} & ($PRE_EXPLICIT)))
                {
                    $self->mailquote(
                        line_ref        => \$para_lines[$i],
                        line_action_ref => \$para_line_action[$i],
                        prev_ref        => $prev_ref,
                        prev_action_ref => $prev_action_ref,
                        next_ref        => $next_ref
                    );
                }

                if (   ($self->{__mode} & $PRE)
                    && ($self->{preformat_trigger_lines} != 0))
                {
                    $self->endpreformat(
                        para_lines_ref  => \@para_lines,
                        para_action_ref => \@para_line_action,
                        ind             => $i,
                        prev_ref        => $prev_ref
                    );
                }

                if (!($self->{__mode} & $PRE))
                {
                    $self->hrule(
                        para_lines_ref  => \@para_lines,
                        para_action_ref => \@para_line_action,
                        ind             => $i
                    );
                }
                if (!($self->{__mode} & ($PRE))
                    && ($para_lines[$i] !~ /^\s*$/))
                {
                    $self->liststuff(
                        para_lines_ref       => \@para_lines,
                        para_action_ref      => \@para_line_action,
                        para_line_indent_ref => \@para_line_indent,
                        ind                  => $i,
                        prev_ref             => $prev_ref
                    );
                }
                if (   !($para_line_action[$i] & ($HEADER | $LIST))
                    && !($self->{__mode} & ($LIST | $PRE))
                    && $self->{__preformat_enabled})
                {
                    $self->preformat(
                        mode_ref        => \$self->{__mode},
                        line_ref        => \$para_lines[$i],
                        line_action_ref => \$para_line_action[$i],
                        prev_ref        => $prev_ref,
                        next_ref        => $next_ref,
                        prev_action_ref => $prev_action_ref
                    );
                }
                if (!($self->{__mode} & ($PRE)))
                {
                    $self->paragraph(
                        line_ref        => \$para_lines[$i],
                        line_action_ref => \$para_line_action[$i],
                        prev_ref        => $prev_ref,
                        prev_action_ref => $prev_action_ref,
                        line_indent     => $para_line_indent[$i],
                        prev_indent     => $prev_line_indent,
                        is_fragment     => $args{is_fragment},
                        ind             => $i,
                    );
                }
                if (!($self->{__mode} & ($PRE | $LIST)))
                {
                    $self->shortline(
                        line_ref        => \$para_lines[$i],
                        line_action_ref => \$para_line_action[$i],
                        prev_ref        => $prev_ref,
                        prev_action_ref => $prev_action_ref,
                        prev_line_len   => $prev_line_len
                    );
                }
                if (!($self->{__mode} & ($PRE)))
                {
                    $self->caps(
                        line_ref        => \$para_lines[$i],
                        line_action_ref => \$para_line_action[$i]
                    );
                }

                # put the "prev" line in front of the first line
                $para_lines[$i] = $prev . $para_lines[$i]
                  if ($i == 0 && ($prev !~ /^\s*$/));
            }

            # para action is the action of the last line of the para
            $para_action = $para_line_action[$#para_line_action];
            $para_action = $NONE if (!defined $para_action);

            # push them on the done lines
            push @done_lines, @para_lines;
            @para_lines = ();

        }
        # now put the para back together as one string
        $para = join('', @done_lines);

        # if this is a paragraph, and we are in XHTML mode,
        # close an open paragraph.
        if ($self->{xhtml})
        {
            my $open_tag = @{$self->{__tags}}[$#{$self->{__tags}}];
            if (defined $open_tag && $open_tag eq 'p')
            {
                $para .= $self->close_tag('p');
            }
        }

        if (
            $self->{unhyphenation}

            # ends in hyphen & next line starts w/letters
            && ($para =~ /[^\W\d_]\-\n\s*[^\W\d_]/s) && !(
                $self->{__mode} &
                ($PRE | $HEADER | $MAILHEADER | $TABLE | $BREAK)
            )
          )
        {
            $self->unhyphenate_para(\$para);
        }
        # chop trailing newlines for continuing lists and PRE
        if (   $self->{__mode} & $LIST
            || $self->{__mode} & $PRE)
        {
            $para =~ s/\n$//g;
        }
    }

    # apply links and bold/italic/underline formatting
    if ($para !~ /^\s*$/)
    {
        $self->apply_links(
            para_ref        => \$para,
            para_action_ref => \$para_action
        );
    }

    # close any open lists if required to
    if (   $args{close_tags}
        && $self->{__mode} & $LIST)    # End all lists
    {
        $self->endlist(
            num_lists       => $self->{__listnum},
            prev_ref        => \$para,
            line_action_ref => \$para_action
        );
    }
    # close any open tags
    if ($args{close_tags} && $self->{xhtml})
    {
        while (@{$self->{__tags}})
        {
            $para .= $self->close_tag('');
        }
    }

    # convert remaining Microsoft character codes into sensible HTML
    if ($self->{demoronize} && !$self->{eight_bit_clean})
    {
        $para = demoronize_code($para);
    }
    # All the matching and formatting is done.  Now we can
    # replace non-ASCII characters with character entities.
    if (!$self->{eight_bit_clean})
    {
        my @chars = split(//, $para);
        foreach $_ (@chars)
        {
            $_ = $char_entities{$_} if defined($char_entities{$_});
        }
        $para = join('', @chars);
    }

    $self->{__prev_para_action} = $para_action;

    return $para;
}    # process_para

=head2 txt2html

    $conv->txt2html(%args);

Convert a text file to HTML.  Takes a hash of arguments.  See
L</OPTIONS> for the possible values of the arguments.  Arguments which
have already been set with B<new> or B<args> will remain as they are,
unless they are overridden.

=cut

sub txt2html ($;$)
{
    my $self = shift;

    if (@_)
    {
        $self->args(@_);
    }

    $self->do_init_call();

    my $outhandle;
    my $outhandle_needs_closing;

    # set up the output
    if ($self->{outhandle})
    {
        $outhandle               = $self->{outhandle};
        $outhandle_needs_closing = 1;
    }
    elsif ($self->{outfile} eq "-")
    {
        $outhandle               = *STDOUT;
        $outhandle_needs_closing = 0;
    }
    else
    {
        open($outhandle, "> " . $self->{outfile})
          || die "Error: unable to open ", $self->{outfile}, ": $!\n";
        $outhandle_needs_closing = 1;
    }

    # slurp up a paragraph at a time, a file at a time
    local $/ = "";
    my $para        = '';
    my $count       = 0;
    my $print_count = 0;
    my @sources     = ();
    my $source_type;
    if ($self->{infile} and @{$self->{infile}})
    {
        @sources     = @{$self->{infile}};
        $source_type = 'file';
    }
    elsif ($self->{inhandle} and @{$self->{inhandle}})
    {
        @sources     = @{$self->{inhandle}};
        $source_type = 'filehandle';
    }
    elsif ($self->{instring} and @{$self->{instring}})
    {
        @sources     = @{$self->{instring}};
        $source_type = 'string';
    }
    my $inhandle;
    my $inhandle_needs_closing = 0;
    foreach my $source (@sources)
    {
        $inhandle = undef;
        if ($source_type eq 'file')
        {
            if (!$source or $source eq '-')
            {
                $inhandle               = *STDIN;
                $inhandle_needs_closing = 0;
            }
            else
            {
                if (-f $source && open($inhandle, $source))
                {
                    $inhandle_needs_closing = 1;
                }
                else    # error
                {
                    warn "Could not open $source\n";
                    next;
                }
            }
        }
        elsif ($source_type eq 'filehandle')
        {
            $inhandle               = $source;
            $inhandle_needs_closing = 1;
        }
        if ($source_type eq 'string')
        {
            # process the string
            $para = $_;
            $para =~ s/\n$//;    # trim the endline
            if ($count == 0)
            {
                $self->do_file_start($outhandle, $para);
            }
            $self->{__done_with_sect_link} = [];
            $para = $self->process_chunk($para, close_tags => 0);
            print $outhandle $para, "\n";
            $print_count++;
            $count++;
        }
        else                     # file or filehandle
        {
            while (<$inhandle>)
            {
                $para = $_;
                $para =~ s/\n$//;    # trim the endline
                if ($count == 0)
                {
                    $self->do_file_start($outhandle, $para);
                }
                $self->{__done_with_sect_link} = [];
                $para = $self->process_chunk($para, close_tags => 0);
                print $outhandle $para, "\n";
                $print_count++;
                $count++;
            }
            if ($inhandle_needs_closing)
            {
                close($inhandle);
            }
        }
    }    # for each file

    $self->{__prev} = "";
    if ($self->{__mode} & $LIST)    # End all lists
    {
        $self->endlist(
            num_lists       => $self->{__listnum},
            prev_ref        => \$self->{__prev},
            line_action_ref => \$self->{__line_action}
        );
    }
    print $outhandle $self->{__prev};

    # end open preformats
    if ($self->{__mode} & $PRE)
    {
        my $tag = $self->close_tag('pre');
        print $outhandle $tag;
    }

    # close all open tags
    if (   $self->{xhtml}
        && !$self->{extract}
        && @{$self->{__tags}})
    {
        if ($DictDebug & 8)
        {
            print STDERR "closing all tags at end\n";
        }
        # close any open tags (until we get to the body)
        my $open_tag = @{$self->{__tags}}[$#{$self->{__tags}}];
        while (@{$self->{__tags}}
            && $open_tag ne 'body'
            && $open_tag ne 'html')
        {
            print $outhandle $self->close_tag('');
            $open_tag = @{$self->{__tags}}[$#{$self->{__tags}}];
        }
        print $outhandle "\n";
    }

    if ($self->{append_file})
    {
        if (-r $self->{append_file})
        {
            open(APPEND, $self->{append_file});
            while (<APPEND>)
            {
                print $outhandle $_;
                $print_count++;
            }
            close(APPEND);
        }
        else
        {
            print STDERR "Can't find or read file ", $self->{append_file},
              " to append.\n";
        }
    }

    # print the closing tags (if we have printed stuff at all)
    if ($print_count && !$self->{extract})
    {
        print $outhandle $self->close_tag('body'), "\n";
        print $outhandle $self->close_tag('html'), "\n";
    }
    if ($outhandle_needs_closing)
    {
        close($outhandle);
    }
    return 1;
}

=head1 PRIVATE METHODS

These are methods used internally, only of interest to developers.

=cut

#---------------------------------------------------------------#
# Init-related subroutines

=head2 init_our_data

$self->init_our_data();

Initializes the internal object data.

=cut
sub init_our_data ($)
{
    my $self = shift;

    #
    # All the options, in alphabetical order
    #
    $self->{append_file}           = '';
    $self->{append_head}           = '';
    $self->{body_deco}             = '';
    $self->{bullets}               = '-=o*\267';
    $self->{bullets_ordered}       = '';
    $self->{bold_delimiter}        = '#';
    $self->{caps_tag}              = 'STRONG';
    $self->{custom_heading_regexp} = [];
    $self->{default_link_dict}     =
      ($ENV{HOME} ? "$ENV{HOME}/.txt2html.dict" : '.txt2html.dict');
    $self->{doctype}                    = '-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd';
    $self->{demoronize}                 = 1;
    $self->{eight_bit_clean}            = 0;
    $self->{escape_HTML_chars}          = 1;
    $self->{explicit_headings}          = 0;
    $self->{extract}                    = 0;
    $self->{hrule_min}                  = 4;
    $self->{indent_width}               = 2;
    $self->{indent_par_break}           = 0;
    $self->{infile}                     = [];
    $self->{inhandle}                   = [];
    $self->{instring}                   = [];
    $self->{italic_delimiter}           = '*';
    $self->{links_dictionaries}         = [];
    $self->{link_only}                  = 0;
    $self->{lower_case_tags}            = 0;
    $self->{mailmode}                   = 0;
    $self->{make_anchors}               = 1;
    $self->{make_links}                 = 1;
    $self->{make_tables}                = 0;
    $self->{min_caps_length}            = 3;
    $self->{outfile}                    = '-';
    $self->{par_indent}                 = 2;
    $self->{preformat_trigger_lines}    = 2;
    $self->{endpreformat_trigger_lines} = 2;
    $self->{preformat_start_marker}     = "^(:?(:?&lt;)|<)PRE(:?(:?&gt;)|>)\$";
    $self->{preformat_end_marker}       = "^(:?(:?&lt;)|<)/PRE(:?(:?&gt;)|>)\$";
    $self->{preformat_whitespace_min}   = 5;
    $self->{prepend_file}               = '';
    $self->{preserve_indent}            = 0;
    $self->{short_line_length}          = 40;
    $self->{style_url}                  = '';
    $self->{tab_width}                  = 8;
    $self->{table_type}                 = {
        ALIGN  => 1,
        PGSQL  => 1,
        BORDER => 1,
        DELIM  => 1,
    };
    $self->{title}                      = '';
    $self->{titlefirst}                 = 0;
    $self->{underline_delimiter}        = '_';
    $self->{underline_length_tolerance} = 1;
    $self->{underline_offset_tolerance} = 1;
    $self->{unhyphenation}              = 1;
    $self->{use_mosaic_header}          = 0;
    $self->{use_preformat_marker}       = 0;
    $self->{xhtml}                      = 1;

    # accumulation variables
    $self->{__file}               = "";    # Current file being processed
    $self->{__heading_styles}     = {};
    $self->{__num_heading_styles} = 0;
    $self->{__links_table}        = {};
    $self->{__links_table_order}  = [];
    $self->{__links_table_patterns} = {};
    $self->{__search_patterns}    = [];
    $self->{__repl_code}          = [];
    $self->{__prev_para_action}   = 0;
    $self->{__non_header_anchor}  = 0;
    $self->{__mode}               = 0;
    $self->{__listnum}            = 0;
    $self->{__list_nice_indent}   = "";
    $self->{__list_indent}        = [];

    $self->{__call_init_done} = 0;

    #
    # The global links data
    #
    my $system_dict = <<'EOT';
#
# Global links dictionary file for HTML::TextToHTML
# http://www.katspace.com/tools/text_to_html
# http://txt2html.sourceforge.net/
# based on links dictionary for Seth Golub's txt2html
# http://www.aigeek.com/txt2html/
#
# This dictionary contains some patterns for converting obvious URLs,
# ftp sites, hostnames, email addresses and the like to hrefs.
#
# Original adapted from the html.pl package by Oscar Nierstrasz in
# the Software Archive of the Software Composition Group
# http://iamwww.unibe.ch/~scg/Src/
#

# Some people even like to mark the URL label explicitly <URL:foo:label>
/&lt;URL:([-\w\.\/:~_\@]+):([a-zA-Z0-9'() ]+)&gt;/ -h-> <A HREF="$1">$2</A>

# Some people like to mark URLs explicitly <URL:foo>
/&lt;URL:\s*(\S+?)\s*&gt;/ -h-> <A HREF="$1">$1</A>

#  <http://site>
/&lt;(http:\S+?)\s*&gt;/ -h-> &lt;<A HREF="$1">$1</A>&gt;

# Urls: <service>:<rest-of-url>

|snews:[\w\.]+|        -> $&
|news:[\w\.]+|         -> $&
|nntp:[\w/\.:+\-]+|    -> $&
|http:[\w/\.:\@+\-~\%#?=&;,]+[\w/]|  -> $&
|shttp:[\w/\.:+\-~\%#?=&;,]+| -> $&
|https:[\w/\.:+\-~\%#?=&;,]+| -> $&
|file:[\w/\.:+\-]+|     -> $&
|ftp:[\w/\.:+\-]+|      -> $&
|wais:[\w/\.:+\-]+|     -> $&
|gopher:[\w/\.:+\-]+|   -> $&
|telnet:[\w/\@\.:+\-]+|   -> $&


# catch some newsgroups to avoid confusion with sites:
|([^\w\-/\.:\@>])(alt\.[\w\.+\-]+[\w+\-]+)|    -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(bionet\.[\w\.+\-]+[\w+\-]+)| -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(bit\.[\w\.+\-]+[\w+\-]+)|    -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(biz\.[\w\.+\-]+[\w+\-]+)|    -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(clari\.[\w\.+\-]+[\w+\-]+)|  -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(comp\.[\w\.+\-]+[\w+\-]+)|   -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(gnu\.[\w\.+\-]+[\w+\-]+)|    -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(humanities\.[\w\.+\-]+[\w+\-]+)| 
          -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(k12\.[\w\.+\-]+[\w+\-]+)|    -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(misc\.[\w\.+\-]+[\w+\-]+)|   -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(news\.[\w\.+\-]+[\w+\-]+)|   -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(rec\.[\w\.+\-]+[\w+\-]+)|    -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(soc\.[\w\.+\-]+[\w+\-]+)|    -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(talk\.[\w\.+\-]+[\w+\-]+)|   -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(us\.[\w\.+\-]+[\w+\-]+)|     -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(ch\.[\w\.+\-]+[\w+\-]+)|     -h-> $1<A HREF="news:$2">$2</A>
|([^\w\-/\.:\@>])(de\.[\w\.+\-]+[\w+\-]+)|     -h-> $1<A HREF="news:$2">$2</A>

# FTP locations (with directory):
# anonymous@<site>:<path>
|(anonymous\@)([[:alpha:]][\w\.+\-]+\.[[:alpha:]]{2,}):(\s*)([\w\d+\-/\.]+)|
  -h-> $1<A HREF="ftp://$2/$4">$2:$4</A>$3

# ftp@<site>:<path>
|(ftp\@)([[:alpha:]][\w\.+\-]+\.[[:alpha:]]{2,}):(\s*)([\w\d+\-/\.]+)|
  -h-> $1<A HREF="ftp://$2/$4">$2:$4</A>$3

# Email address
|[[:alnum:]_\+\-\.]+\@([[:alnum:]][\w\.+\-]+\.[[:alpha:]]{2,})|
  -> mailto:$&

# <site>:<path>
|([^\w\-/\.:\@>])([[:alpha:]][\w\.+\-]+\.[[:alpha:]]{2,}):(\s*)([\w\d+\-/\.]+)|
  -h-> $1<A HREF="ftp://$2/$4">$2:$4</A>$3

# NB: don't confuse an http server with a port number for
# an FTP location!
# internet number version: <internet-num>:<path>
|([^\w\-/\.:\@])(\d{2,}\.\d{2,}\.\d+\.\d+):([\w\d+\-/\.]+)|
  -h-> $1<A HREF="ftp://$2/$3">$2:$3</A>

# telnet <site> <port>
|telnet ([[:alpha:]][\w+\-]+(\.[\w\.+\-]+)+\.[[:alpha:]]{2,})\s+(\d{2,4})|
  -h-> telnet <A HREF="telnet://$1:$3/">$1 $3</A>

# ftp <site>
|ftp ([[:alpha:]][\w+\-]+(\.[\w\.+\-]+)+\.[[:alpha:]]{2,})|
  -h-> ftp <A HREF="ftp://$1/">$1</A>

# host with "ftp" in the machine name
|\b([[:alpha:]][\w])*ftp[\w]*(\.[\w+\-]+){2,}| -h-> ftp <A HREF="ftp://$&/">$&</A>

# ftp.foo.net/blah/
|ftp(\.[\w\@:-]+)+/\S+| -> ftp://$&

# www.thehouse.org/txt2html/
|www(\.[\w\@:-]+)+/\S+| -> http://$&

# host with "www" in the machine name
|\b([[:alpha:]][\w])*www[\w]*(\.[\w+\-]+){2,}| -> http://$&/

# <site> <port>
|([[:alpha:]][\w+\-]+\.[\w+\-]+\.[[:alpha:]]{2,})\s+(\d{2,4})|
  -h-> <A HREF="telnet://$1:$2/">$1 $2</A>

# just internet numbers with port:
|([^\w\-/\.:\@])(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})\s+(\d{1,4})|
  -h-> $1<A HREF="telnet://$2:$3">$2 $3</A>

# just internet numbers:
|([^\w\-/\.:\@])(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})|
  -h-> $1<A HREF="telnet://$2">$2</A>

# RFCs
/RFC ?(\d+)/ -i-> http://www.cis.ohio-state.edu/rfc/rfc$1.txt

# Mark _underlined stuff_ as <U>underlined stuff</U>
# Don't mistake variable names for underlines, and
# take account of possible trailing punctuation
#/([ \t\n])_([[:alpha:]][[:alnum:]\s-]*[[:alpha:]])_([\s\.;:,\!\?])/ -h-> $1<U>$2</U>$3

# Seth and his amazing conversion program    :-)

"Seth Golub"  -o-> http://www.aigeek.com/
"txt2html"    -o-> http://txt2html.sourceforge.net/

# Kathryn and her amazing modules 8-)
"Kathryn Andersen"  -o-> http://www.katspace.com/
"HTML::TextToHTML"  -o-> http://www.katspace.com/tools/text_to_html/
"hypertoc"          -o-> http://www.katspace.com/tools/hypertoc/
"HTML::GenToc"      -o-> http://www.katspace.com/tools/hypertoc/

# End of global dictionary
EOT

    # pre-parse the above data by removing unwanted lines
    # skip lines that start with '#'
    $system_dict =~ s/^\#.*$//mg;
    # skip lines that end with unescaped ':'
    $system_dict =~ s/^.*[^\\]:\s*$//mg;

    $self->{__global_links_data} = $system_dict;

}    # init_our_data

#---------------------------------------------------------------#
# txt2html-related subroutines

=head2 deal_with_options

$self->deal_with_options();

do extra processing related to particular options

=cut
sub deal_with_options ($)
{
    my $self = shift;

    if (!$self->{make_links})
    {
        $self->{'links_dictionaries'} = 0;
    }
    if ($self->{append_file})
    {
        if (!-r $self->{append_file})
        {
            print STDERR "Can't find or read ", $self->{append_file}, "\n";
            $self->{append_file} = '';
        }
    }
    if ($self->{prepend_file})
    {
        if (!-r $self->{prepend_file})
        {
            print STDERR "Can't find or read ", $self->{prepend_file}, "\n";
            $self->{'prepend_file'} = '';
        }
    }
    if ($self->{append_head})
    {
        if (!-r $self->{append_head})
        {
            print STDERR "Can't find or read ", $self->{append_head}, "\n";
            $self->{'append_head'} = '';
        }
    }

    if (!$self->{outfile})
    {
        $self->{'outfile'} = "-";
    }

    $self->{'preformat_trigger_lines'} = 0
      if ($self->{preformat_trigger_lines} < 0);
    $self->{'preformat_trigger_lines'} = 2
      if ($self->{preformat_trigger_lines} > 2);

    $self->{'endpreformat_trigger_lines'} = 1
      if ($self->{preformat_trigger_lines} == 0);
    $self->{'endpreformat_trigger_lines'} = 0
      if ($self->{endpreformat_trigger_lines} < 0);
    $self->{'endpreformat_trigger_lines'} = 2
      if ($self->{endpreformat_trigger_lines} > 2);

    $self->{__preformat_enabled} =
      (($self->{endpreformat_trigger_lines} != 0)
          || $self->{use_preformat_marker});

    if ($self->{use_mosaic_header})
    {
        my $num_heading_styles = 0;
        my %heading_styles     = ();
        $heading_styles{"*"}          = ++$num_heading_styles;
        $heading_styles{"="}          = ++$num_heading_styles;
        $heading_styles{"+"}          = ++$num_heading_styles;
        $heading_styles{"-"}          = ++$num_heading_styles;
        $heading_styles{"~"}          = ++$num_heading_styles;
        $heading_styles{"."}          = ++$num_heading_styles;
        $self->{__heading_styles}     = \%heading_styles;
        $self->{__num_heading_styles} = $num_heading_styles;
    }
    # XHTML implies lower case
    $self->{'lower_case_tags'} = 1 if ($self->{xhtml});
}

=head2 escape

$newtext = escape($text);

Escape & < and >

=cut
sub escape ($)
{
    my ($text) = @_;
    $text =~ s/&/&amp;/g;
    $text =~ s/>/&gt;/g;
    $text =~ s/</&lt;/g;
    return $text;
}

=head2 demoronize_char

$newtext = demoronize_char($text);

Convert Microsoft character entities into characters.

Added by Alan Jackson, alan at ajackson dot org, and based
on the demoronize script by John Walker, http://www.fourmilab.ch/

=cut
sub demoronize_char($)
{
    my $s = shift;
    #   Map strategically incompatible non-ISO characters in the
    #   range 0x82 -- 0x9F into plausible substitutes where
    #   possible.

    $s =~ s/\x82/,/g;
    $s =~ s/\x84/,,/g;
    $s =~ s/\x85/.../g;

    $s =~ s/\x88/^/g;

    $s =~ s/\x8B/</g;
    $s =~ s/\x8C/Oe/g;

    $s =~ s/\x91/`/g;
    $s =~ s/\x92/'/g;
    $s =~ s/\x93/"/g;
    $s =~ s/\x94/"/g;
    $s =~ s/\x95/*/g;
    $s =~ s/\x96/-/g;
    $s =~ s/\x97/--/g;

    $s =~ s/\x9B/>/g;
    $s =~ s/\x9C/oe/g;

    return $s;
}

=head2 demoronize_code

$newtext = demoronize_code($text);

convert Microsoft character entities into HTML code

=cut
sub demoronize_code($)
{
    my $s = shift;
    #   Map strategically incompatible non-ISO characters in the
    #   range 0x82 -- 0x9F into plausible substitutes where
    #   possible.

    $s =~ s-\x83-<em>f</em>-g;

    $s =~ s-\x98-<sup>~</sup>-g;
    $s =~ s-\x99-<sup>TM</sup>-g;

    return $s;
}

=head2 get_tag

$tag = $self->get_tag($in_tag);

$tag = $self->get_tag($in_tag,
	tag_type=>TAG_START,
	inside_tag=>'');

output the tag wanted (add the <> and the / if necessary)
- output in lower or upper case
- do tag-related processing
options:
  tag_type=>TAG_START | tag_type=>TAG_END | tag_type=>TAG_EMPTY
  (default start)
  inside_tag=>string (default empty)

=cut
sub get_tag ($$;%)
{
    my $self   = shift;
    my $in_tag = shift;
    my %args   = (
        tag_type   => TAG_START,
        inside_tag => '',
        @_
    );
    my $inside_tag = $args{inside_tag};

    my $open_tag = @{$self->{__tags}}[$#{$self->{__tags}}];
    if (!defined $open_tag)
    {
        $open_tag = '';
    }
    # close any open tags that need closing
    # Note that we only have to check for the structural tags we make,
    # not every possible HTML tag
    my $tag_prefix = '';
    if ($self->{xhtml})
    {
        if (    $open_tag eq 'p'
            and $in_tag eq 'p'
            and $args{tag_type} != TAG_END)
        {
            $tag_prefix = $self->close_tag('p');
        }
        elsif ( $open_tag eq 'p'
            and $in_tag =~ /^(hr|ul|ol|dl|pre|table|h)/)
        {
            $tag_prefix = $self->close_tag('p');
        }
        elsif ( $open_tag eq 'li'
            and $in_tag eq 'li'
            and $args{tag_type} != TAG_END)
        {
            # close a LI before the next LI
            $tag_prefix = $self->close_tag('li');
        }
        elsif ( $open_tag eq 'li'
            and $in_tag =~ /^(ul|ol)$/
            and $args{tag_type} == TAG_END)
        {
            # close the LI before the list closes
            $tag_prefix = $self->close_tag('li');
        }
        elsif ( $open_tag eq 'dt'
            and $in_tag eq 'dd'
            and $args{tag_type} != TAG_END)
        {
            # close a DT before the next DD
            $tag_prefix = $self->close_tag('dt');
        }
        elsif ( $open_tag eq 'dd'
            and $in_tag eq 'dt'
            and $args{tag_type} != TAG_END)
        {
            # close a DD before the next DT
            $tag_prefix = $self->close_tag('dd');
        }
        elsif ( $open_tag eq 'dd'
            and $in_tag         eq 'dl'
            and $args{tag_type} == TAG_END)
        {
            # close the DD before the list closes
            $tag_prefix = $self->close_tag('dd');
        }
    }

    my $out_tag = $in_tag;
    if ($args{tag_type} == TAG_END)
    {
        $out_tag = $self->close_tag($in_tag);
    }
    else
    {
        if ($self->{lower_case_tags})
        {
            $out_tag =~ tr/A-Z/a-z/;
        }
        else    # upper case
        {
            $out_tag =~ tr/a-z/A-Z/;
        }
        if ($args{tag_type} == TAG_EMPTY)
        {
            if ($self->{xhtml})
            {
                $out_tag = "<${out_tag}${inside_tag}/>";
            }
            else
            {
                $out_tag = "<${out_tag}${inside_tag}>";
            }
        }
        else
        {
            push @{$self->{__tags}}, $in_tag;
            $out_tag = "<${out_tag}${inside_tag}>";
        }
    }
    $out_tag = $tag_prefix . $out_tag if $tag_prefix;
    if ($DictDebug & 8)
    {
        print STDERR
          "open_tag = '${open_tag}', in_tag = '${in_tag}', tag_type = ",
          $args{tag_type},
          ", inside_tag = '${inside_tag}', out_tag = '$out_tag'\n";
    }

    return $out_tag;
}    # get_tag

=head2 close_tag

$tag = $self->close_tag($in_tag);

close the open tag

=cut
sub close_tag ($$)
{
    my $self   = shift;
    my $in_tag = shift;

    my $open_tag = pop @{$self->{__tags}};
    $in_tag ||= $open_tag;
    # put the open tag back on the stack if the in-tag is not the same
    if (defined $open_tag && $open_tag ne $in_tag)
    {
        push @{$self->{__tags}}, $open_tag;
    }
    my $out_tag = $in_tag;
    if ($self->{lower_case_tags})
    {
        $out_tag =~ tr/A-Z/a-z/;
    }
    else    # upper case
    {
        $out_tag =~ tr/a-z/A-Z/;
    }
    $out_tag = "<\/${out_tag}>";
    if ($DictDebug & 8)
    {
        print STDERR
"close_tag: open_tag = '${open_tag}', in_tag = '${in_tag}', out_tag = '$out_tag'\n";
    }

    return $out_tag;
}

=head2 hrule

   $self->hrule(para_lines_ref=>$para_lines,
	     para_action_ref=>$para_action,
	     ind=>0);

Deal with horizontal rules.

=cut
sub hrule ($%)
{
    my $self = shift;
    my %args = (
        para_lines_ref  => undef,
        para_action_ref => undef,
        ind      