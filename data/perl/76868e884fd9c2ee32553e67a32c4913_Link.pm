# Manage links.

package Util::Link;

use strict;
use Exporter;

use POE;
use Util::Web;
use Util::Conf;
use Util::Database;

use vars qw(@ISA @EXPORT);
@ISA    = qw(Exporter);
@EXPORT = qw( get_link_as_table_row parse_link_from_message
            );
push @EXPORT, @Util::Database::EXPORT;

#------------------------------------------------------------------------------
# Parse link and description from message.  This is independent of the
# type of chat system.

sub parse_link_from_message {
  my $message = shift;

  # URLs and descriptions.
  my @link;
  my $description = $message;

  while ( $description =~
	  s/\s*(http:\/\/[^\s\'\"\>]+)[^\s\'\"\>]*/ [link] /
	) {
    push @link, $1;
  }

  $description =~ s/^\s*\[link\]\s*[\.\|\#\:\-]+\s*//;
  $description =~ s/\s*[\.\|\#\:\-]+\s*\[link\]\s*$//;

  return ($description, @link);
}

#------------------------------------------------------------------------------
# Return a formatted HTML table row for a given link.

sub get_link_as_table_row {
  my $link_id = shift;

  my $link = get_link_obj_by_id($link_id);

  return '' unless $link;

  my $html = ( 
      "<p>" .
      "<table border=0 width='100%' cellspacing=0 cellpadding=3 bgcolor='#e0e0f0'>"
    );

  if (defined $link->[PAGE_TITLE] and length $link->[PAGE_TITLE]) {
    $html .= ( "<tr>" .
               "<th align=left valign=top width='1%'>Title:</th>" .
               "<td width='99%'>".html_encode($link->[PAGE_TITLE])."</td>" .
               "</tr>"
             );
  }


  $html .= (
      "<tr>" .
      "<th align=left valign=top width='1%'>Link:</th>" .
      "<td width='99%'><a href='$link->[LINK]'>".html_encode($link->[LINK])."</a><td>" .
      "</tr>"
    );

  if (defined $link->[REDIRECT] and length $link->[REDIRECT]) {
    $html .=
      ( "<tr>" .
        "<th align=left valign=top width='1%'>Really:</th>" .
        ( "<td width='99%'>" .
          "<a href='$link->[REDIRECT]'>".html_encode($link->[REDIRECT])."</a>" .
          "<td>"
        ) .
        "</tr>"
      );
  }

  $html .=
    ( "<tr>" .
      "<th align=left valign=top width='1%'>From:</th>" .
      ( "<td width='99%'>$link->[USER] -- " .
        scalar(gmtime($link->[TIME])) .
        " GMT -- mentioned $link->[MENTION_COUNT] time" .
        ( ($link->[MENTION_COUNT] == 1) ? '' : 's' ) .
        "</td>"
      ) .
      "</tr>"
    );

  if (defined($link->[DESC]) and $link->[DESC] ne '(none)') {
    $html .=
      ( "<tr>" .
        "<th align=left valign=top width='1%'>Context:</th>" .
        "<td width='99%'>".html_encode($link->[DESC])."</td>" .
        "</tr>"
      );
  }

  if (defined $link->[PAGE_DESC] and length $link->[PAGE_DESC]) {
    $html .= ( "<tr>" .
               "<th align=left valign=top width='1%'>Description:</th>" .
               "<td width='99%'>".html_encode($link->[PAGE_DESC])."</td>" .
               "</tr>"
             );
  }

  if (defined $link->[PAGE_KEYS] and length $link->[PAGE_KEYS]) {
    $html .= ( "<tr>" .
               "<th align=left valign=top width='1%'>Keywords:</th>" .
               "<td width='99%'>".html_encode($link->[PAGE_KEYS])."</td>" .
               "</tr>"
             );
  }


  if (defined $link->[CHECK_TIME] and $link->[CHECK_TIME]) {
    $html .= ( "<tr>" .
               "<th align=left valign=top width='1%'>Status:</th>" .
               ( "<td width='99%'>" .
                 scalar(gmtime($link->[CHECK_TIME])) .
                 " GMT -- " .
                 "$link->[CHECK_STATUS]</td>"
               ) .
               "</tr>"
             );
  }

  if (defined $link->[PAGE_TIME] and length $link->[PAGE_TIME]) {
    $html .= ( "<tr>" .
               "<th align=left valign=top width='1%'>Updated:</th>" .
               ( "<td width='99%'>" .
                 scalar(gmtime($link->[PAGE_TIME])) .
                 " GMT</td>"
               ) .
               "</tr>"
             );
  }
  if (defined $link->[PAGE_TYPE] and length $link->[PAGE_TYPE]) {
    $html .= ( "<tr>" .
               "<th align=left valign=top width='1%'>Content:</th>" .
               ( "<td width='99%'>$link->[PAGE_TYPE]" .
                 ( $link->[PAGE_SIZE]
                   ? " ($link->[PAGE_SIZE] bytes)"
                   : " (unknown size)" ) .
                 "</td>"
               ) .
               "</tr>"
             );
  }

  $html .= "</table></p>";

  return $html;
}

#------------------------------------------------------------------------------
1;
