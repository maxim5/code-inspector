package IDS::Algorithm;
$IDS::Algorithm::VERSION = "1.0";

=head1 NAME

IDS::Algorithm - An IDS algorithm for use with IDS::Test

=head1 SYNOPSIS

A usage synopsis would go here.  Since it is not here, read on.

=head1 DESCRIPTION

An IDS::Algorithm is a superclass for all of the IDS algorithms.
This class was designed for HTTP requests, but should be usable in
other areas.  The idea here is to standardize the interface in such a
way that it can be easily used by many algorithms.

The interface was designed for one-pass learning algorithms.  Algorithms
needing multiple passes can be made to work, but the interface is less
clean.  See L<Methods/"next_pass">.

When working with an IDS::Algorithm algorithm, the following operations
exist:

=cut

use strict;
use warnings;
use Carp qw(cluck carp confess);
use IO::File;
use IDS::Utils qw(to_fh);

### It would be more efficient and cleaner to autoload only what we need
### rather than listing all possibilities here
use IDS::Algorithm::Null;
use IDS::Algorithm::DFA;
use IDS::Algorithm::MM;
use IDS::Algorithm::Ngram;
use IDS::Algorithm::WangStolfo;
use IDS::Algorithm::Length;
use IDS::Algorithm::KVCharDist;
use IDS::Algorithm::Order;
use IDS::Algorithm::EnumOrRandom;
use IDS::Algorithm::KruegelVigna;

=over

=item new()

Create the object for the algorithm.  If the parameters are supplied,
they are used; otherwise everything is defaults (unsurprisingly).  
If a filehandle is supplied, the filehandle is taken as the source for
a load operation.

The parameters affect how the algorithm operates.  See the specific
algorithm for most parameter information.  General parameters are
described below.

Parameters are simply entries in a hash (i.e., "name" => "value");

In most cases, this function can be used instead of the subclass
version.  Complex cases must duplicate the logic here.

=back

=cut

# Part of the logic here seems kind of backwards, but we cannot load
# until parameters have been loaded.  Some of the parameters may affect
# how we load.
sub new {
    my $invocant = shift;
    my $class = ref($invocant) || $invocant;
    my $self = { };
    my $source;
    my $state = 0;

    # necessary before we call handle_parameters.
    bless($self, $class);

    if ($self->can("new") && $self->can("new") ne \&new) {
        # if a new() exists, we assume it can do everything necessary
	# including handling parameters, etc
        return $self->new(@_);
    } else {
	$self->can("default_parameters") ? $self->default_parameters
	        : warn "$self ($class) has no default_parameters\n";
	$source = $self->handle_parameters(@_);

	$self->initialize; # may do nothing

	$self->load($source) if defined($source); # unlikely to occur
	                                          # due to IDS::Test framework
	return $self;
    }
    # return via one of the above choices
    confess "bug: impossible condition in ", *new{PACKAGE} . "::new";
}

=over

=item save(filename)

=item save(filehandle)

Save the data in a form that will allow testing later.  Some subclasses
may save in a format that allows further addition as well.  This is not
required.

This version simply uses Data::Dumper; subclasses are likely to have
better (and more efficient) designs.

=back

=cut

sub save {
    my $self = shift;
    my $arg = shift or
        confess "bug: missing fname/fhandle to ", *save{PACKAGE} . "::save";
    my $fh = to_fh($arg, ">");

    print $fh Dumper($self);
    undef $fh;

    return $self;
}

=over

=item load()

=item load(filename)

=item load(filehandle)

Load the data saved by save().  

Note for subclasses: If no filename/filehandle is provided, use a default
or command-line specified filename.  The recommended commandline
parameter is ids_state, tied to the parameter state_file.

This version simply uses Data::Dumper; subclasses are likely to have
better (and more efficient) designs.

=back

=cut

sub load {
    my $self = shift;
    my $arg = shift;
    defined($arg) && $arg or 
        $arg = ${$self->{"params"}}{"state_file"};
    defined($arg) && $arg or 
	confess *load{PACKAGE} .  "::load bug: missing fname/fhandle";

    my $fh = to_fh($arg);
    my $VAR1;

    eval join('', <$fh>);
    $self = $VAR1;
    undef $fh;

    return $self;
}

=over

=item add(tokenref, string, n)

In training, this is an instance to learn.  This function must be
subclassed.

tokenref is a reference to a list of tokens, assuming the
data source can produce tokens, an empty list otherwise.  

string is the string version of the instance.

n is an object counter, which may be useful in output.

=back

=cut

sub add {
    confess *add{PACKAGE} . "::add (IDS::Algorithm::add) called; " .
                            "this function must be subclassed.\n";
}

=over

=item test(tokenref, string, n)

In testing, this is an instance to test.  This function must be
subclassed.

tokenref is a reference to a list of tokens, assuming the
data source can produce tokens, an empty list otherwise.  

string is the string version of the instance.

n is an object counter, which may be useful in output.

=back

=cut

sub test {
    confess *test{PACKAGE} . "::test (IDS::Algorithm::test) called; " .
                            "this function must be subclassed.\n";
}

=over

=item next_pass(tokenref, string, n)

For algorithms that require two passes over the training data, this
function (if it exists) implements the next pass.  Algorithms requiring
more than two passes require that the algorithm remembers the training
data itself.

=cut

=over

=item initialize()

This (optional) function provides any subclass-specific initialization.

=back

=cut

sub initialize {
    my $self = shift;

    # do nothing, return nothing
}

=over

=item parameters()

=item parameters(param)

=item parameters(param, value, ...)

Set or retrieve the current parameters (individual or group).

=back

=cut

sub parameters {
    my $self = shift;

    return (wantarray ? %{$self->{"params"}} : $self->{"params"})
        if ($#_ == -1);

    return ${$self->{"params"}}{$_[0]} if ($#_ == 0);

    if ($#_ == 1) {
	my $old = ${$self->{"params"}}{$_[0]};
	${$self->{"params"}}{$_[0]} = $_[1];
	return $old;
    }

    scalar(@_) % 2 != 0 and confess "odd > 1 number of parameters passed to ",
        *parameters{PACKAGE}, ".  See documentation for proper usage.\n";

    for (my $i = 0; $i < $#_; $i+=2) {
	${$self->{"params"}}{$_[$i]} = $_[$i+1];
    }

    return 1;
}

=head2 Parameters

The following parameters are expected to be used (when the use makes
sense) by all subclasses.

=over

=item verbose

How much ``extra'' information to provide as we are working.  0 means
nothing other than warnings and errors.  Increasing values mean
increasing output, but these details are left to the subclasses.

In order to avoid a name clash, the convention for the getopt name for
the algorithm's verbose is algorithm_verbose, for example: ngram_verbose
or dfa_verbose.

=item msg_fh

Where warning and error messages go; nowhere if undefined.

=back

Additionally, subclasses may define their own parameters.

All subclasses must define all parameters to default values when new()
is called.  This is so the param_options can be properly handled.

=over

=item param_options()

These are options definitions for Getopt::Long.

=back

=cut

sub param_options {
    my $self = shift;

    warn *param_options{PACKAGE} . "::param_options not supplied by subclass\n";

    return (
            "verbose!" => \${$self->{"params"}}{"verbose"},
	   );
}

=head2 Utility functions

These may be useful for subclasses.

=over

=item handle_parameters($self, @_)

Handle the parameter string that IDS::Algorithm::new() will accept.
Extracted from new() for subclass usage.  Returns a filehandle if we
were called with a filehandle from which to load.

=back

=cut

sub handle_parameters {
    my $self = shift;
    my $loadfh;

    if (defined($_[0])) {
	# If we have a non-zero even number of arguments,  we have
	# nothing but parameters
	if (scalar(@_) % 2 == 0) {
	    undef $loadfh;
	} else {
	    $loadfh = shift;
	}
    }
    $self->parameters(@_);
    return $loadfh;
}

=over

=item default_parameters()

Assign default values to all parameters.

=back

=cut

sub default_parameters {
    my $self = shift;

    warn *default_parameters{PACKAGE} . "::default_parameters not supplied by subclass\n";

    $self->{"params"} = { "verbose" => 0 };
}

=over

=item find_fname(filename)

Return the supplied filename.   If it is undefined or otherwise missing,
fall back on using ${$self->{"params"}}{"state_file"}

=back

=cut

sub find_fname {
    my $self = shift;
    my $fname = shift;

    defined($fname) && $fname or $fname = ${$self->{"params"}}{"state_file"};
    return $fname;
}

=head1 AUTHOR INFORMATION

Copyright 2005-2007, Kenneth Ingham.  All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

Address bug reports and comments to: ids_test at i-pi.com.  When sending
bug reports, please provide the versions of IDS::Test.pm, IDS::Algorithm.pm,
IDS::DataSource.pm, the version of Perl, and the name and version of the
operating system you are using.  Since Kenneth is a PhD student, the
speed of the reponse depends on how the research is proceeding.

=head1 BUGS

Please report them.

=head1 SEE ALSO

L<IDS::Test>, L<IDS::DataSource>

=cut

1;
