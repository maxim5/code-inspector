# Manage the link database.

package Util::Database;

use strict;
use Exporter;

use POE;
use Util::Conf;

use vars qw(@ISA @EXPORT);
@ISA = qw(Exporter);

my @databases = get_names_by_type('database');
die "Only one database at a time for now please\n" if @databases > 1;
die "No database specified in config file\n" unless @databases;

my %conf  = get_items_by_name($databases[0]);
my $database_class = "Database::\u\L$conf{type}";
eval "use $database_class";
die "Can't load specified database '$database_class': $@" if ($@);
{ 
    no strict "refs";
    push @EXPORT, @{"${database_class}::EXPORT"};
}

#------------------------------------------------------------------------------
1;
