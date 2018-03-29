# Data Object for FormFieldListPlugin
# Plugin for Foswiki - The Free and Open Source Wiki, http://foswiki.org/
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details, published at
# http://www.gnu.org/copyleft/gpl.html

package Foswiki::Plugins::FormFieldListPlugin::FormFieldData;

use strict;
use overload ( '""' => \&as_string );

my %sortKeys = (
    '$fieldDefinition' => [ 'fieldDefinition', 'string' ],
    '$name'            => [ 'name',            'string' ],
    '$value'           => [ 'value',           'string' ],
    '$fieldDate'       => [ 'fieldDate',       'integer' ],
    '$topicName'       => [ 'topic',           'string' ],
    '$topicDate'       => [ 'date',            'integer' ],
    '$topicUser'       => [ 'user',            'string' ],
);

our $EMPTY_VALUE_PLACEHOLDER = '_FORMFIELDLISTPLUGIN_EMPTY_';

=pod

=cut

sub new {
    my ( $class, $web, $topic, $field, $name ) = @_;
    my $this = {};

    $this->{'field'} = $field;

    # only copy sort keys to FormFieldData attributes
    $this->{'name'}  = $name             || '';
    $this->{'title'} = $name             || $field->{'title'} || '';
    $this->{'value'} = $field->{'value'} || $EMPTY_VALUE_PLACEHOLDER;
    $this->{'date'}      = undef;
    $this->{'fieldDate'} = '';
    $this->{'topic'}     = $topic;
    $this->{'user'}      = undef;

    $this->{'web'}      = $web;
    $this->{'notfound'} = 0;

    bless $this, $class;
}

sub getSortKey {
    my ($inRawKey) = @_;
    return $sortKeys{$inRawKey}[0];
}

sub getCompareMode {
    my ($inRawKey) = @_;
    return $sortKeys{$inRawKey}[1];
}

sub setTopicDate {
    my ( $this, $inDate ) = @_;
    $this->{date} = $inDate;
    if ( !$this->{'fieldDate'} ) {
        $this->{'fieldDate'} = $inDate;
    }
}

sub setFieldDate {
    my ( $this, $inDate ) = @_;
    $this->{'fieldDate'} = $inDate;
}

=pod

Stringify function for data storage.

Most important data as string, each value separated by a tab:
- stringify version number
- web
- topic
- name
- value
- topicDate

=cut

sub stringify {
    my $this = shift;

    return
"1.0\t$this->{web}\t$this->{topic}\t$this->{name}\t$this->{value}\t$this->{date}";
}

=pod

Stringify function for debugging only.

=cut

sub as_string {
    my $this = shift;

    return
        "FormFieldListPlugin: web="
      . $this->{'web'}
      . "; topic="
      . $this->{'topic'}
      . "; name="
      . $this->{'name'}
      . "; value="
      . $this->{'value'};
}

1;
